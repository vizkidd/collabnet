# =============================================================================
# GScholarLENS-Graph.R
# -----------------------------------------------------------------------------
# Scalable co-occurrence network construction and analysis for CollabNET.
#
# Drop-in replacement for build_collaboration_network() in
# GScholarLENS-PlotGLENS.R, plus new analysis entry points.
#
# Design notes
# ------------
# The old implementation materialised the co-authorship projection with a
# self-join on paper_id:
#
#     clean_df %>% inner_join(clean_df, by = "paper_id") %>% filter(x < y)
#
# For a paper with k authors that is k^2 intermediate rows. A single ATLAS
# paper (k = 3000) produces 9,000,000 rows *before* the filter, as a tibble,
# in a single-threaded WASM heap. This file replaces that with a sparse
# bipartite incidence matrix and a C-level crossprod, which never
# materialises the intermediate and stores ~12 bytes per surviving pair
# instead of ~100.
#
# It also adds the pieces that actually make a 10^5-edge graph legible:
# Newman fractional weighting, disparity-filter backbone extraction,
# Louvain communities, level-of-detail collapsing, and a layout engine
# that degrades gracefully instead of running Fruchterman-Reingold on
# 50,000 vertices.
#
# Dependencies: Matrix (base-recommended, available in webR), igraph,
#               stringi. No new WASM packages required.
# =============================================================================

suppressPackageStartupMessages({
  require(Matrix)
  require(igraph)
})



# -----------------------------------------------------------------------------
# P0-5: community detection.
# algorithm "leiden" (default; modularity objective, weighted, iterated to
# convergence) or "louvain". Falls back to Louvain with a note if
# cluster_leiden is unavailable (igraph < 1.3).
# -----------------------------------------------------------------------------
cn_detect_communities <- function(g, algorithm = "leiden", resolution = 1, seed = 42L) {
  algorithm <- tolower(algorithm %||% "leiden")
  note <- NULL
  w <- igraph::E(g)$weight
  if (algorithm == "leiden" && !exists("cluster_leiden", envir = asNamespace("igraph"))) {
    algorithm <- "louvain"; note <- "cluster_leiden unavailable in this igraph build; fell back to Louvain"
  }
  cl <- tryCatch(with_seed(seed, switch(algorithm,
    leiden  = igraph::cluster_leiden(g, objective_function = "modularity", weights = w,
                                     resolution_parameter = resolution, n_iterations = -1),
    louvain = igraph::cluster_louvain(g, weights = w, resolution = resolution),
    stop("unknown community algorithm '", algorithm, "'"))),
    error = function(e) { note <<- paste("community detection failed:", conditionMessage(e)); NULL })
  if (is.null(cl)) return(list(membership = NULL, modularity = NA_real_, algorithm = algorithm, note = note))
  memb <- as.integer(igraph::membership(cl))
  q <- tryCatch(igraph::modularity(g, memb, weights = w, resolution = resolution),
                error = function(e) NA_real_)
  list(membership = memb, modularity = q,
       algorithm = if (algorithm == "leiden") "Leiden" else "Louvain", note = note)
}

# Stability of the partition across k seeds: mean pairwise adjusted Rand index.
cn_community_stability <- function(g, k = 10L, algorithm = "leiden", resolution = 1,
                                   seeds = seq_len(k)) {
  ms <- lapply(seeds, function(s) cn_detect_communities(g, algorithm, resolution, s)$membership)
  ms <- Filter(Negate(is.null), ms)
  if (length(ms) < 2L) return(list(mean_ari = NA_real_, n_runs = length(ms)))
  pr <- utils::combn(length(ms), 2)
  ari <- apply(pr, 2, function(ij) igraph::compare(ms[[ij[1]]], ms[[ij[2]]], method = "adjusted.rand"))
  list(mean_ari = mean(ari), min_ari = min(ari), n_runs = length(ms),
       n_communities = vapply(ms, function(m) length(unique(m)), 1L))
}

# -----------------------------------------------------------------------------
# Run expr with a local RNG seed, restoring the caller's .Random.seed (or its
# absence) afterwards, so the engine never mutates global RNG state.
# -----------------------------------------------------------------------------
# P0-8a (engine part): author names are untrusted dataset strings; visNetwork renders node `title` tooltips as
# HTML, so escape them before interpolation (base R, no htmltools dependency in the engine).
cn_escape_html <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE); gsub("'", "&#39;", x, fixed = TRUE)
}

with_seed <- function(seed, expr) {
  genv <- globalenv()
  had  <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old  <- if (had) get(".Random.seed", envir = genv, inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = genv)
    else if (exists(".Random.seed", envir = genv, inherits = FALSE))
      rm(".Random.seed", envir = genv)
  }, add = TRUE)
  set.seed(seed)
  expr
}

# -----------------------------------------------------------------------------
# Smart per-row delimiter splitting for multi-value columns (Authors, Keywords, etc.)
# Seamlessly handles delimiter variations (e.g. Scopus ';' vs CrossRef/ORCiD ',').
# -----------------------------------------------------------------------------
cn_split_multivalue <- function(raw, target_delim = ",") {
  if (is.null(raw) || length(raw) == 0) return(list())

  is_standard_delim <- is.null(target_delim) ||
                       !nzchar(trimws(target_delim)) ||
                       grepl("[,;]", target_delim)

  clean <- function(res) { res <- trimws(res); res[nzchar(res)] }

  if (!is_standard_delim) {
    return(lapply(raw, function(x) {
      if (is.na(x) || !nzchar(trimws(x))) return(character(0))
      clean(strsplit(x, target_delim, fixed = TRUE)[[1]])
    }))
  }

  # P0-6: decide per *column*, not per row. A row without ';' in a column
  # where ';' is the majority separator is a single "Last, First" record
  # ("Smith, J.") and must not be split on its comma. In comma columns we
  # re-pair flattened "Last, Initials" sequences and keep a lone
  # "Surname, Firstname" record intact.
  raw <- as.character(raw)
  nonempty <- !is.na(raw) & nzchar(trimws(raw))
  has_semi <- nonempty & grepl(";", raw, fixed = TRUE)
  has_any  <- nonempty & grepl("[,;]", raw)
  semi_col <- identical(target_delim, ";") ||
              (any(has_any) && sum(has_semi) >= 0.5 * sum(has_any))

  initials_re <- "^(\\p{Lu}\\.?[ -]?){1,4}$"
  one_word    <- function(z) !grepl("\\s", z)
  # tokens from comma rows in this column: are names typically multi-word?
  multiword_col <- local({
    cr <- raw[nonempty & !has_semi & grepl(",", raw, fixed = TRUE)]
    tl <- lapply(strsplit(utils::head(cr, 2000L), ",", fixed = TRUE), function(z) { z <- trimws(z); z[nzchar(z)] })
    # ignore the ambiguous rows themselves (two one-word tokens) and initials
    tl <- tl[!vapply(tl, function(z) length(z) == 2L && all(one_word(z)), logical(1))]
    tk <- unlist(tl)
    tk <- tk[!grepl(initials_re, tk, perl = TRUE)]
    length(tk) > 0 && mean(!one_word(tk)) >= 0.5
  })

  lapply(seq_along(raw), function(r) {
    x <- raw[r]
    if (!nonempty[r]) return(character(0))
    if (has_semi[r]) return(clean(strsplit(x, ";", fixed = TRUE)[[1]]))
    if (semi_col || !grepl(",", x, fixed = TRUE)) return(clean(x))
    parts <- clean(strsplit(x, ",", fixed = TRUE)[[1]])
    np <- length(parts)
    if (np >= 2L && np %% 2L == 0L) {
      ev <- parts[seq(2L, np, by = 2L)]
      od <- parts[seq(1L, np, by = 2L)]
      if (all(grepl(initials_re, ev, perl = TRUE)) && all(one_word(od) | grepl("-", od)))
        return(paste(od, ev, sep = ", "))                       # Last, I. pairs
      if (np == 2L && all(one_word(parts)) && multiword_col)
        return(paste(parts, collapse = ", "))                   # Surname, Firstname
    }
    parts
  })
}


# -----------------------------------------------------------------------------
# Internal: fast tokenisation of a multi-value column into a long (paper,
# entity) edge list of integer codes.
#
# Returns NULL when there is nothing to build from, otherwise a list with:
#   pid   integer, 1-based row index of the source publication
#   code  integer, 1-based entity code
#   label character, display label per code
#   n_ent integer, number of distinct entities
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# P1-1: author identity. Key nodes on a persistent author ID when one is available (ORCID iD, OpenAlex author ID,
# Scopus author ID, positionally aligned with the author list), otherwise on a normalised name:
#   "exact"      legacy: lower-case + trailing punctuation stripped (pre-6B behaviour)
#   "normalised" accent folding, case/whitespace/punctuation normalisation, "Last, First" vs "First Last" and
#                "Surname I.J." order reconciliation; given names reduced to initials only when they ARE initials
#   "initials"   surname + first initial (merges "Smith J.A." with "Smith J." and "John Smith"; may merge homonyms)
# Base R only (webR-safe): accent folding via an explicit chartr table, iconv TRANSLIT as a fallback.
# -----------------------------------------------------------------------------
CN_ACCENT_FROM <- "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105\u00e7\u0107\u010d\u010f\u00e8\u00e9\u00ea\u00eb\u0113\u0117\u0119\u011b\u011f\u00ec\u00ed\u00ee\u00ef\u012b\u0131\u0142\u00f1\u0144\u0148\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u0151\u0155\u0159\u015b\u0161\u015f\u0163\u0165\u00f9\u00fa\u00fb\u00fc\u016b\u016f\u0171\u00fd\u00ff\u017a\u017c\u017e"
CN_ACCENT_TO   <- "aaaaaaaaacccdeeeeeeeegiiiiiilnnnoooooooorrsssttuuuuuuuyyzzz"
cn_fold_accents <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))
  x <- gsub("ß", "ss", x, fixed = TRUE); x <- gsub("æ", "ae", x, fixed = TRUE); x <- gsub("œ", "oe", x, fixed = TRUE)
  x <- chartr(CN_ACCENT_FROM, CN_ACCENT_TO, x)
  if (any(grepl("[^\\x01-\\x7f]", x, perl = TRUE))) {            # anything left: best-effort transliteration
    y <- suppressWarnings(iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = ""))
    x <- ifelse(is.na(y), x, tolower(y))
  }
  x
}
CN_SURNAME_PARTICLES <- c("van", "von", "der", "den", "de", "del", "della", "di", "da", "dos", "das", "du", "la", "le", "ter", "ten", "bin", "al", "el")
cn_name_parts <- function(ent) {
  x <- cn_fold_accents(ent)
  x <- gsub("[‐-―-]", " ", x, perl = TRUE)             # hyphens / dashes -> space
  has_comma <- grepl(",", x, fixed = TRUE)
  sur <- character(length(x)); giv <- character(length(x))
  sur[has_comma] <- sub(",.*$", "", x[has_comma]); giv[has_comma] <- sub("^[^,]*,", "", x[has_comma])
  nc <- which(!has_comma)
  if (length(nc)) {
    toks <- strsplit(trimws(gsub("[[:space:]]+", " ", gsub("\\.", ". ", x[nc]))), " ", fixed = TRUE)
    for (k in seq_along(nc)) {
      t <- toks[[k]]; t <- t[nzchar(t)]
      if (length(t) <= 1L) { sur[nc[k]] <- paste(t, collapse = ""); next }
      is_init <- grepl("^[a-z]{1,2}\\.?$", t)                     # "j." / "ja" style tokens
      n <- length(t); r <- n
      while (r >= 1L && is_init[r]) r <- r - 1L                     # trailing run of initials
      if (r < n && r >= 1L) {                                       # Scopus "Surname [Surname2] I.J."
        sur[nc[k]] <- paste(t[seq_len(r)], collapse = " "); giv[nc[k]] <- paste(t[(r + 1L):n], collapse = " ")
      } else {                                                      # "First [Middle] [particles] Last"
        j <- n
        while (j > 2L && t[j - 1L] %in% CN_SURNAME_PARTICLES) j <- j - 1L
        sur[nc[k]] <- paste(t[j:n], collapse = " "); giv[nc[k]] <- paste(t[seq_len(j - 1L)], collapse = " ")
      }
    }
  }
  clean <- function(z) trimws(gsub("[[:space:]]+", " ", gsub("[^a-z0-9 ]", " ", z)))
  sur <- gsub(" ", "", clean(sur)); giv <- clean(gsub("\\.", ". ", giv))
  gtok <- strsplit(giv, " ", fixed = TRUE)
  initials_only <- vapply(gtok, function(t) length(t) > 0L && all(nchar(t[nzchar(t)]) <= 2L), logical(1))
  inits <- vapply(gtok, function(t) paste(substr(t[nzchar(t)], 1L, 1L), collapse = ""), "")
  list(surname = sur, given = gsub(" ", "", giv), initials = inits, initials_only = initials_only)
}
cn_identity_key <- function(ent, policy = c("normalised", "exact", "initials"), ids = NULL) {
  policy <- match.arg(policy)
  if (policy == "exact") {
    key <- trimws(gsub("[.;:,]+$", "", tolower(ent)))
    reason <- rep("exact (case/trailing punctuation)", length(ent))
  } else {
    p <- cn_name_parts(ent)
    given <- if (policy == "initials") substr(p$initials, 1L, 1L) else ifelse(p$initials_only, p$initials, p$given)
    key <- ifelse(nzchar(p$surname), paste0(p$surname, "|", given), trimws(tolower(ent)))
    reason <- rep(if (policy == "initials") "name: surname + first initial" else "name: normalised (accents, punctuation, order)", length(ent))
  }
  if (!is.null(ids)) {
    ids <- trimws(as.character(ids)); use <- !is.na(ids) & nzchar(ids)
    key[use] <- paste0("id:", tolower(ids[use])); reason[use] <- "author ID"
  }
  list(key = key, reason = reason)
}
# Positional author IDs (e.g. Scopus "Author(s) ID", "ORCIDs", "OpenAlex_IDs"): one ID list per row, aligned with the
# author list; rows whose ID count differs from the author count are ignored (fall back to names).
CN_ID_COLUMNS <- c("Author(s) ID", "Author IDs", "author_ids", "ORCIDs", "orcids", "OpenAlex_IDs", "openalex_author_ids")

cn_tokenise <- function(df, target_col, target_delim = ",", case_fold = TRUE,
                        identity_policy = "normalised", id_col = NULL) {

  if (is.null(df) || !nrow(df) || !target_col %in% colnames(df)) return(NULL)

  raw <- as.character(df[[target_col]])
  pid <- seq_along(raw)
  # Name normalisation / ID keying only make sense for person columns; keywords, sources etc. stay "exact".
  is_person_col <- grepl("author|name|orcid|person|inventor", target_col, ignore.case = TRUE)
  if (!is_person_col) { identity_policy <- "exact"; id_col <- NA_character_ }
  if (is.null(id_col)) { hit <- intersect(CN_ID_COLUMNS, colnames(df)); id_col <- if (length(hit)) hit[1] else NA_character_ }
  id_raw <- if (!is.na(id_col) && id_col %in% colnames(df)) as.character(df[[id_col]]) else NULL

  keep <- !is.na(raw) & nzchar(trimws(raw))
  raw  <- raw[keep]
  pid  <- pid[keep]
  if (!length(raw)) return(NULL)

  # Smart splitting handles semicolon and comma variations seamlessly
  pieces <- cn_split_multivalue(raw, target_delim)
  ids <- NULL
  if (!is.null(id_raw)) {
    idp <- lapply(strsplit(ifelse(is.na(id_raw[keep]), "", id_raw[keep]), "[;|]"), trimws)
    ids <- unlist(Map(function(a, b) if (length(b) == length(a)) b else rep(NA_character_, length(a)), pieces, idp), use.names = FALSE)
  }
  pid    <- rep.int(pid, lengths(pieces))
  ent    <- unlist(pieces, use.names = FALSE)

  ent <- trimws(gsub("[[:space:]]+", " ", ent))
  ok  <- !is.na(ent) & nzchar(ent)
  ent <- ent[ok]
  pid <- pid[ok]
  if (!is.null(ids)) ids <- ids[ok]
  if (!length(ent)) return(NULL)

  # P1-1: canonical identity key (author ID if aligned, else normalised name; see cn_identity_key()).
  if (!isTRUE(case_fold)) identity_policy <- "exact"
  idk <- cn_identity_key(ent, identity_policy, ids)
  key <- idk$key

  f     <- factor(key)
  code  <- as.integer(f)
  n_ent <- nlevels(f)

  # Display label: first surface form seen for each canonical key.
  label <- ent[match(seq_len(n_ent), code)]

  # De-duplicate (paper, entity) so an author listed twice on one paper does
  # not inflate the co-occurrence count. Sort-and-diff is O(n log n) with no
  # hashing of pasted strings.
  o    <- order(pid, code)
  pid  <- pid[o]
  code <- code[o]
  if (length(pid) > 1L) {
    dedup <- c(TRUE, (pid[-1L] != pid[-length(pid)]) | (code[-1L] != code[-length(code)]))
    pid   <- pid[dedup]
    code  <- code[dedup]
  }

  # Merge/split review table: which raw strings were mapped onto which node, and why.
  rv <- unique(data.frame(raw = ent, key = key, reason = idk$reason, stringsAsFactors = FALSE))
  rv$node <- label[match(rv$key, levels(f))]
  nraw <- table(rv$key); rv$n_raw_forms <- as.integer(nraw[rv$key])
  rv <- rv[order(-rv$n_raw_forms, rv$key, rv$raw), c("node", "raw", "key", "reason", "n_raw_forms")]
  rownames(rv) <- NULL

  list(pid = pid, code = code, label = label, n_ent = n_ent, identity = rv, identity_policy = identity_policy,
       id_col = if (!is.null(id_raw)) id_col else NA_character_)
}


# -----------------------------------------------------------------------------
# Internal: coerce any Matrix product to a strict upper-triangle triplet list.
# -----------------------------------------------------------------------------
cn_upper_triplets <- function(A) {
  Tm   <- methods::as(methods::as(A, "generalMatrix"), "TsparseMatrix")
  keep <- Tm@i < Tm@j
  list(i = Tm@i[keep] + 1L, j = Tm@j[keep] + 1L, x = Tm@x[keep])
}


# -----------------------------------------------------------------------------
# Internal: sparse bipartite projection.
#
# size_min / size_max bound how many entities a publication may contribute.
# oversize_mode decides what happens to publications above size_max:
#
#   "drop"   discard them (old behaviour)
#   "star"   keep them as a hub node connected to each of their authors.
#            O(k) edges instead of O(k^2), so a 3000-author paper costs
#            3000 edges rather than 4.5 million, and you can still see who
#            belongs to the collaboration.
#   "clique" expand anyway (only do this deliberately)
#
# weight_scheme:
#   "count"   raw number of shared publications
#   "newman"  sum over shared papers of 1/(k-1); the standard bibliometric
#             correction so a 3000-author paper does not outweigh 50 real
#             two-author collaborations
#   "jaccard" |P_u ∩ P_v| / |P_u ∪ P_v|; scale-free similarity
# -----------------------------------------------------------------------------
cn_project <- function(pid, code, n_ent,
                       size_min      = 1L,
                       size_max      = 100L,
                       weight_scheme = c("newman", "count", "jaccard"),
                       oversize_mode = c("star", "drop", "clique")) {

  weight_scheme <- match.arg(weight_scheme)
  oversize_mode <- match.arg(oversize_mode)

  upid  <- sort(unique(pid))
  pidx  <- match(pid, upid)
  ksize <- tabulate(pidx, nbins = length(upid))

  too_small <- ksize < size_min
  too_big   <- ksize > size_max
  cliqueable <- !too_small & !too_big

  n_dropped_small <- sum(too_small)
  n_oversize      <- sum(too_big)

  edges <- list(i = integer(0), j = integer(0),
                cnt = numeric(0), w = numeric(0))
  hub_edges <- NULL
  hub_labels <- character(0)

  # --- clique projection for normally sized publications --------------------
  sel <- cliqueable[pidx]
  if (any(sel)) {
    keep_p <- which(cliqueable)
    p2     <- match(pidx[sel], keep_p)
    c2     <- code[sel]
    k2     <- ksize[keep_p]

    Bc <- Matrix::sparseMatrix(i = p2, j = c2, x = 1,
                               dims = c(length(keep_p), n_ent))

    tc <- cn_upper_triplets(Matrix::crossprod(Bc))

    if (length(tc$i)) {
      key_c <- tc$i * (n_ent + 1) + tc$j
      ord   <- order(key_c)
      tc    <- list(i = tc$i[ord], j = tc$j[ord], x = tc$x[ord])

      w <- switch(
        weight_scheme,
        count = tc$x,
        jaccard = {
          occ <- tabulate(c2, nbins = n_ent)
          tc$x / pmax(occ[tc$i] + occ[tc$j] - tc$x, 1)
        },
        newman = {
          # Folding sqrt(1/(k-1)) into the incidence values means the
          # crossprod itself produces sum_p 1/(k_p - 1); no second pass over
          # the pair list is needed.
          wrow <- sqrt(1 / pmax(k2[p2] - 1L, 1L))
          Bw   <- Matrix::sparseMatrix(i = p2, j = c2, x = wrow,
                                       dims = c(length(keep_p), n_ent))
          tw   <- cn_upper_triplets(Matrix::crossprod(Bw))
          tw$x[order(tw$i * (n_ent + 1) + tw$j)]
        }
      )

      edges <- list(i = tc$i, j = tc$j, cnt = tc$x, w = w)
    }
  }

  # --- oversize publications ------------------------------------------------
  if (n_oversize > 0L && oversize_mode == "star") {
    big_p  <- which(too_big)
    selb   <- too_big[pidx]
    hp     <- match(pidx[selb], big_p)          # 1..n_oversize
    hub_id <- n_ent + hp                        # synthetic node codes
    hub_labels <- paste0("[collaboration #", big_p, " \u00b7 ",
                         ksize[big_p], " authors]")[hp]
    hub_labels <- hub_labels[!duplicated(hub_id)][order(unique(hub_id))]

    hub_edges <- list(
      i   = pmin(code[selb], hub_id),
      j   = pmax(code[selb], hub_id),
      cnt = rep(1, sum(selb)),
      w   = 1 / pmax(ksize[big_p][hp] - 1L, 1L)
    )
  } else if (n_oversize > 0L && oversize_mode == "clique") {
    rec <- cn_project(pid[too_big[pidx]], code[too_big[pidx]], n_ent,
                      size_min = 1L, size_max = .Machine$integer.max,
                      weight_scheme = weight_scheme, oversize_mode = "drop")
    hub_edges <- rec$edges
  }

  if (!is.null(hub_edges) && length(hub_edges$i)) {
    edges <- list(
      i   = c(edges$i,   hub_edges$i),
      j   = c(edges$j,   hub_edges$j),
      cnt = c(edges$cnt, hub_edges$cnt),
      w   = c(edges$w,   hub_edges$w)
    )
    # C-4: in clique mode a pair that co-authored both a normal and an oversize paper got TWO parallel edges
    # (inflating degree, k-core, density). Merge duplicates: counts and Newman weights are additive over papers.
    # (Jaccard weights are not additive; for weight_scheme = "jaccard" the merged value is an approximation.)
    if (oversize_mode == "clique" && length(edges$i)) {
      key <- paste(edges$i, edges$j)
      if (anyDuplicated(key)) {
        f <- factor(key, levels = unique(key)); first <- !duplicated(key)
        edges <- list(i = edges$i[first], j = edges$j[first],
                      cnt = as.numeric(tapply(edges$cnt, f, sum)), w = as.numeric(tapply(edges$w, f, sum)))
      }
    }
  }

  list(
    edges           = edges,
    hub_labels      = hub_labels,
    n_hubs          = length(hub_labels),
    n_dropped_small = n_dropped_small,
    n_oversize      = n_oversize,
    paper_sizes     = ksize
  )
}


# -----------------------------------------------------------------------------
# Disparity filter (Serrano, Boguna & Vespignani, PNAS 2009).
#
# For each node i with degree k_i and strength s_i, an incident edge of
# weight w is kept when (1 - w/s_i)^(k_i - 1) < alpha for at least one of
# its endpoints. This is the right tool for clique-contaminated
# co-authorship graphs: it removes the uniform low-weight noise that a
# mega-paper sprays across the network while preserving locally significant
# ties, and unlike a global weight threshold it does not amputate small
# groups that simply publish less.
#
# Returns a logical vector over the edge list.
# -----------------------------------------------------------------------------
cn_disparity_filter <- function(from, to, weight, n_nodes, alpha = 0.05) {

  if (!length(from)) return(logical(0))

  s <- numeric(n_nodes)
  k <- integer(n_nodes)

  # strength and degree, both endpoints
  s_add <- tapply(c(weight, weight), c(from, to), sum)
  k_add <- tapply(c(weight, weight), c(from, to), length)
  idx   <- as.integer(names(s_add))
  s[idx] <- as.numeric(s_add)
  k[idx] <- as.integer(k_add)

  pval <- function(node, w) {
    kk <- k[node]
    ss <- s[node]
    out <- rep(0, length(w))
    multi <- kk > 1L & ss > 0
    out[multi] <- (1 - w[multi] / ss[multi])^(kk[multi] - 1L)
    # Degree-1 nodes have no null model; always keep their single edge.
    out[kk <= 1L] <- 0
    out
  }

  p_from <- pval(from, weight)
  p_to   <- pval(to,   weight)

  pmin(p_from, p_to) < alpha
}


# -----------------------------------------------------------------------------
# Layout selection that degrades instead of hanging.
#
# layout_with_fr is O(V^2) per iteration. At 500 iterations it is fine for
# a few hundred vertices and unusable past a few thousand. DrL is a C
# multilevel force layout built for 10^4-10^6 vertices. layout_components()
# lays out and packs each connected component separately, which stops the
# classic "one giant hairball plus a ring of islands flung to infinity"
# problem that FR produces on disconnected bibliometric graphs.
# -----------------------------------------------------------------------------
cn_layout <- function(g, engine = "auto", fr_iterations = 500L, seed = 42L) {

  nv <- igraph::vcount(g)
  if (nv == 0L) return(matrix(numeric(0), ncol = 2))
  if (nv == 1L) return(matrix(c(0, 0), ncol = 2))

  # P0-5: local seed - stable coordinates without touching global RNG state.
  with_seed(seed, cn_layout_impl(g, nv, engine, fr_iterations))
}

cn_layout_impl <- function(g, nv, engine, fr_iterations) {
  if (identical(engine, "auto")) {
    # C2-1: in the browser app (option set by server.R) large graphs are laid
    # out by the JS worker pool, so R only produces a cheap placeholder.
    engine <- if (isTRUE(getOption("collabnet.worker_layout")) && nv >= CN_WORKER_LAYOUT_MIN_NODES) "worker"
              else if (nv <= 1500L) "fr" else if (nv <= 30000L) "drl" else "grid"
  }

  lay_fun <- switch(
    engine,
    fr   = function(gg) {
      # Seed FR from a cheap deterministic layout so it needs far fewer
      # iterations to settle.
      init <- igraph::layout_with_graphopt(gg, niter = 60)
      igraph::layout_with_fr(gg, coords = init,
                             niter = max(1L, as.integer(fr_iterations)),
                             grid = "nogrid")
    },
    drl  = function(gg) igraph::layout_with_drl(gg, options = igraph::drl_defaults$default),
    kk   = function(gg) igraph::layout_with_kk(gg),
    grid = function(gg) igraph::layout_on_grid(gg),
    # C2-1: "worker" = the browser Web Worker computes the final layout; the
    # server only sends a cheap placeholder grid (replaced client-side).
    worker = function(gg) igraph::layout_on_grid(gg),
    function(gg) igraph::layout_nicely(gg)
  )

  coords <- tryCatch(
    igraph::layout_components(g, layout = lay_fun),
    error = function(e) {
      warning("cn_layout(): ", conditionMessage(e), " - falling back to grid.")
      igraph::layout_on_grid(g)
    }
  )

  # Normalise to a stable pixel box so visNetwork's camera behaves the same
  # regardless of which engine ran.
  rng <- apply(coords, 2, function(z) diff(range(z)))
  rng[rng == 0 | !is.finite(rng)] <- 1
  scale_to <- 900
  sweep(coords, 2, apply(coords, 2, mean)) /
    max(rng) * scale_to * 2
}


# =============================================================================
# build_collaboration_network()
#
# Signature-compatible with the original. Every original argument keeps its
# meaning; the new arguments all have defaults that preserve or improve the
# previous behaviour.
#
# Returns list(nodes, edges, graph, stats).
# =============================================================================
.cn_build_collaboration_network_impl <- function(
    df,
    authors_per_pub    = c(1, 100),
    node_freq_range    = c(1, Inf),
    edge_freq_range    = c(1, Inf),
    main_authors_list  = character(0),
    target_col         = "Authors",
    target_delim       = ",",
    fr_iterations      = 500,
    prune_leaves       = FALSE,
    cluster_size_range = c(1, Inf),
    # ---- new ----
    weight_scheme      = "newman",
    oversize_mode      = "star",
    backbone_alpha     = NULL,
    max_edges          = 20000L,
    max_nodes          = 6000L,
    detect_communities = TRUE,
    compute_centrality = TRUE,
    layout_engine      = "auto",
    build_tooltips     = TRUE,
    verbose            = FALSE,
    drop_isolates      = FALSE,
    betweenness_approx = NULL,
    community_algorithm = "leiden",
    resolution         = 1,
    seed               = 42L,
    identity_policy    = "normalised",
    id_col             = NULL,
    ...
) {

  empty <- list(nodes = data.frame(), edges = data.frame(),
                graph = NULL, stats = list(reason = "empty"))

  if (is.null(main_authors_list)) main_authors_list <- character(0)
  main_authors_list <- main_authors_list[!is.na(main_authors_list) &
                                           nzchar(trimws(main_authors_list))]

  tk <- cn_tokenise(df, target_col, target_delim, identity_policy = identity_policy, id_col = id_col)
  if (is.null(tk)) return(empty)

  pid   <- tk$pid
  code  <- tk$code
  label <- tk$label
  n_ent <- tk$n_ent

  t0 <- Sys.time()

  # --- node occurrence filter (applied before projection, so it also
  #     shrinks the clique sizes that follow) --------------------------------
  occ      <- tabulate(code, nbins = n_ent)
  lo       <- if (length(node_freq_range) >= 1) node_freq_range[1] else 1
  hi       <- if (length(node_freq_range) >= 2) node_freq_range[2] else Inf
  if (!is.finite(hi)) hi <- max(occ, 1)
  ok_ent   <- occ >= lo & occ <= hi
  # never filter out an explicitly queried author
  if (length(main_authors_list)) {
    prot <- cn_match_targets(label, main_authors_list)
    ok_ent <- ok_ent | prot
  }
  if (!all(ok_ent)) {
    keep <- ok_ent[code]
    pid  <- pid[keep]
    code <- code[keep]
  }
  if (!length(code)) return(empty)

  size_min <- if (length(authors_per_pub) >= 1) max(1L, as.integer(authors_per_pub[1])) else 1L
  size_max <- if (length(authors_per_pub) >= 2) as.integer(authors_per_pub[2]) else 100L
  if (is.na(size_max) || size_max < size_min) size_max <- size_min

  proj <- cn_project(pid, code, n_ent,
                     size_min      = size_min,
                     size_max      = size_max,
                     weight_scheme = weight_scheme,
                     oversize_mode = oversize_mode)

  e <- proj$edges
  if (!length(e$i)) return(empty)

  if (proj$n_hubs > 0L) {
    label <- c(label, proj$hub_labels)
    occ   <- c(occ, rep(1L, proj$n_hubs))
  }
  n_all <- length(label)

  # --- edge frequency window (raw co-occurrence count) ----------------------
  elo <- if (length(edge_freq_range) >= 1) edge_freq_range[1] else 1
  ehi <- if (length(edge_freq_range) >= 2) edge_freq_range[2] else Inf
  if (!is.finite(ehi)) ehi <- max(e$cnt, 1)
  sel <- e$cnt >= elo & e$cnt <= ehi
  e   <- lapply(e, function(v) v[sel])
  if (!length(e$i)) return(empty)

  n_edges_raw <- length(e$i)

  # --- statistical backbone -------------------------------------------------
  n_backbone_removed <- 0L
  if (!is.null(backbone_alpha) && is.finite(backbone_alpha) &&
      backbone_alpha > 0 && backbone_alpha < 1 && length(e$i) > 50L) {
    keep <- cn_disparity_filter(e$i, e$j, e$w, n_all, alpha = backbone_alpha)
    if (any(keep)) {
      n_backbone_removed <- sum(!keep)
      e <- lapply(e, function(v) v[keep])
    }
  }

  # --- hard render cap ------------------------------------------------------
  n_capped <- 0L
  if (length(e$i) > max_edges) {
    ord      <- order(e$w, e$cnt, decreasing = TRUE)[seq_len(max_edges)]
    n_capped <- length(e$i) - max_edges
    e        <- lapply(e, function(v) v[ord])
  }

  # --- leaf pruning ---------------------------------------------------------
  if (isTRUE(prune_leaves) && length(e$i)) {
    deg <- tabulate(c(e$i, e$j), nbins = n_all)
    prot <- if (length(main_authors_list)) cn_match_targets(label, main_authors_list)
            else rep(FALSE, n_all)
    leaf <- deg == 1L & !prot
    if (any(leaf)) {
      keep <- !(leaf[e$i] | leaf[e$j])
      if (any(keep)) e <- lapply(e, function(v) v[keep])
    }
  }
  if (!length(e$i)) return(empty)

  # --- solo-only authors -----------------------------------------------------
  # P0-6: entities that only ever appear on single-entity publications have
  # no edges but are real members of the corpus; keep them as isolates unless
  # drop_isolates = TRUE.
  solo_codes <- integer(0)
  if (!isTRUE(drop_isolates)) {
    kp <- tabulate(match(pid, sort(unique(pid))))[match(pid, sort(unique(pid)))]
    has_co <- tabulate(code[kp >= 2L], nbins = n_ent) > 0L
    solo_codes <- setdiff(unique(code[kp == 1L]), which(has_co))
    solo_codes <- setdiff(solo_codes, c(e$i, e$j))
  }

  # --- build the igraph object once and reuse it for everything -------------
  used  <- sort(unique(c(e$i, e$j, solo_codes)))
  remap <- integer(n_all); remap[used] <- seq_along(used)

  g <- igraph::make_empty_graph(n = length(used), directed = FALSE)
  g <- igraph::add_edges(g, as.vector(rbind(remap[e$i], remap[e$j])))
  igraph::V(g)$name   <- label[used]
  igraph::E(g)$weight <- e$w
  igraph::E(g)$count  <- e$cnt

  # --- component / cluster-size window --------------------------------------
  # P0-1: this is a *display* filter only. The default c(1, Inf) (or NULL)
  # keeps every component, including the giant one. When the user does set a
  # finite window and it removes vertices, the counts are recorded in
  # stats$component_filter so the UI can warn and the methods log is honest.
  component_filter <- list(range = if (is.null(cluster_size_range)) c(1, Inf) else cluster_size_range,
                           active = FALSE, removed_nodes = 0L, removed_components = 0L)
  if (!is.null(cluster_size_range) && length(cluster_size_range) >= 1) {
    comp <- igraph::components(g)
    clo  <- suppressWarnings(as.numeric(cluster_size_range[1]))
    if (!is.finite(clo)) clo <- 1
    chi  <- if (length(cluster_size_range) >= 2) suppressWarnings(as.numeric(cluster_size_range[2])) else Inf
    if (is.na(chi)) chi <- Inf
    good <- which(comp$csize >= clo & comp$csize <= chi)
    keepv <- comp$membership %in% good
    if (length(main_authors_list)) {
      keepv <- keepv | cn_match_targets(igraph::V(g)$name, main_authors_list)
    }
    if (!all(keepv)) {
      component_filter$active <- TRUE
      component_filter$removed_nodes <- sum(!keepv)
      component_filter$removed_components <- length(unique(comp$membership[!keepv]))
      component_filter$removed_largest <- !any(keepv[comp$membership == which.max(comp$csize)])
      g <- igraph::induced_subgraph(g, which(keepv))
    }
  }
  if (igraph::vcount(g) == 0L || igraph::ecount(g) == 0L) return(empty)

  # --- node cap (keep the highest-strength vertices) ------------------------
  n_node_capped <- 0L
  if (igraph::vcount(g) > max_nodes) {
    str_v <- igraph::strength(g, weights = igraph::E(g)$weight)
    prot  <- cn_match_targets(igraph::V(g)$name, main_authors_list)
    ordv  <- order(prot, str_v, decreasing = TRUE)[seq_len(max_nodes)]
    n_node_capped <- igraph::vcount(g) - max_nodes
    g <- igraph::induced_subgraph(g, sort(ordv))
    if (isTRUE(drop_isolates)) g <- igraph::delete_vertices(g, which(igraph::degree(g) == 0L))
  }
  if (igraph::vcount(g) == 0L) return(empty)

  # --- communities ----------------------------------------------------------
  # P0-5: Leiden (modularity objective) is the default; Louvain optional.
  # Both run under a local seed so the same inputs give the same partition
  # and the caller's RNG state is untouched.
  memb <- rep(1L, igraph::vcount(g))
  modularity_score <- NA_real_
  community_info <- list(algorithm = if (isTRUE(detect_communities)) community_algorithm else "none",
                         resolution = resolution, seed = seed, note = NULL)
  if (isTRUE(detect_communities) && igraph::ecount(g) > 0L) {
    cd <- cn_detect_communities(g, algorithm = community_algorithm,
                                resolution = resolution, seed = seed)
    if (!is.null(cd$membership)) {
      memb <- cd$membership
      modularity_score <- cd$modularity
    }
    community_info$algorithm <- cd$algorithm
    community_info$note <- cd$note
  }

  # --- centralities ---------------------------------------------------------
  nv     <- igraph::vcount(g)
  deg    <- igraph::degree(g)
  stren  <- igraph::strength(g, weights = igraph::E(g)$weight)
  btw    <- rep(NA_real_, nv)
  clo    <- rep(NA_real_, nv)
  harm   <- rep(NA_real_, nv)
  pr     <- rep(NA_real_, nv)
  eig    <- rep(NA_real_, nv)
  coren  <- rep(NA_integer_, nv)
  is_cut <- rep(FALSE, nv)
  constr <- rep(NA_real_, nv); effsize <- rep(NA_real_, nv)
  betweenness_method <- "not computed"

  if (isTRUE(compute_centrality)) {
    coren <- igraph::coreness(g)
    # P2-3: structural holes. Burt's constraint (tie strength = Newman weight) and effective size (Borgatti 1997:
    # degree - 2 * ties among alters / degree, unweighted; NA for isolates). count_triangles(v) = ties among v's alters.
    constr <- tryCatch(as.numeric(igraph::constraint(g, weights = igraph::E(g)$weight)), error = function(err) rep(NA_real_, nv))
    tri <- tryCatch(as.numeric(igraph::count_triangles(g)), error = function(err) rep(NA_real_, nv))
    effsize <- ifelse(deg > 0, deg - 2 * tri / pmax(deg, 1), NA_real_)
    eig <- tryCatch(
      igraph::eigen_centrality(g, weights = igraph::E(g)$weight)$vector,
      error = function(err) rep(NA_real_, nv))
    pr <- tryCatch(
      igraph::page_rank(g, weights = igraph::E(g)$weight)$vector,
      error = function(err) rep(NA_real_, nv))
    clo <- tryCatch(
      igraph::closeness(g, weights = 1 / pmax(igraph::E(g)$weight, 1e-9),
                        normalized = TRUE),
      error = function(err) rep(NA_real_, nv))
    harm <- tryCatch(
      igraph::harmonic_centrality(g, weights = 1 / pmax(igraph::E(g)$weight, 1e-9),
                                  normalized = TRUE),
      error = function(err) rep(NA_real_, nv))
    # P0-3: exact Brandes betweenness (distances 1/w) at every size. The old
    # silent cutoff = 4 above 2,500 vertices is gone. An approximation is only
    # used when the caller opts in via betweenness_approx and is then labelled
    # in stats$betweenness_method.
    btw_res <- cn_betweenness(g, approx = betweenness_approx)
    btw <- btw_res$values
    # P0-7: isolates (kept since P0-6) have undefined closeness (NaN); define 0.
    clo[is.nan(clo)] <- 0
    betweenness_method <- btw_res$method
    # Articulation points (critical bottleneck cut-vertices)
    cut_v_indices <- tryCatch(igraph::articulation_points(g), error = function(e) integer(0))
    if (length(cut_v_indices) > 0) {
      is_cut[cut_v_indices] <- TRUE
    }
  }

  # --- layout ---------------------------------------------------------------
  coords <- cn_layout(g, engine = layout_engine, fr_iterations = fr_iterations, seed = seed)

  # --- assemble the visNetwork frames --------------------------------------
  ids     <- igraph::V(g)$name
  is_tgt  <- cn_match_targets(ids, main_authors_list)
  is_hub  <- grepl("^\\[collaboration #", ids)

  nodes <- data.frame(
    id                = ids,
    label             = ids,
    degree            = as.integer(deg),
    strength          = round(stren, 4),
    betweenness       = round(btw, 5),
    closeness         = round(clo, 5),
    harmonic          = round(harm, 5),
    pagerank          = round(pr, 5),
    eigen_centrality  = round(eig, 4),
    coreness          = as.integer(coren),
    constraint        = round(constr, 5),
    effective_size    = round(effsize, 4),
    is_cut_vertex     = as.logical(is_cut),
    community         = as.integer(memb),
    total_connections = as.integer(deg),
    group = ifelse(is_hub, "Mega-collaboration",
                   ifelse(is_tgt, "Queried Target", "Associated Entity")),
    x = coords[, 1] * 1.1,
    y = coords[, 2] * 1.1,
    stringsAsFactors = FALSE
  )
  nodes$size <- 12 + log1p(nodes$strength) * 6

  if (isTRUE(build_tooltips)) {
    nodes$title <- paste0(
      "<div style='padding:8px;border-radius:5px;background:#fff;color:#111;",
      "box-shadow:1px 1px 5px rgba(0,0,0,.2);font-size:12px;'>",
      "<b>", cn_escape_html(nodes$label), "</b><br>",
      "Degree: ", nodes$degree, "<br>",
      "Weighted strength: ", sprintf("%.2f", nodes$strength), "<br>",
      "Betweenness: ", sprintf("%.4f", nodes$betweenness), "<br>",
      "Closeness: ", sprintf("%.4f", nodes$closeness), "<br>",
      "Harmonic: ", sprintf("%.4f", nodes$harmonic), "<br>",
      "PageRank: ", sprintf("%.4f", nodes$pagerank), "<br>",
      "Community: ", nodes$community, "<br>",
      "k-core: ", nodes$coreness,
      ifelse(nodes$is_cut_vertex, "<br><span style='color:#e74c3c;font-weight:bold;'>&#9888; Critical Broker (Articulation Point)</span>", ""),
      "</div>")
  }

  el <- igraph::as_edgelist(g, names = TRUE)
  edges <- data.frame(
    from        = el[, 1],
    to          = el[, 2],
    connections = as.integer(igraph::E(g)$count),
    weight      = round(igraph::E(g)$weight, 5),
    stringsAsFactors = FALSE
  )
  edges$value <- edges$weight
  if (isTRUE(build_tooltips) && nrow(edges) <= 5000L) {
    # Per-edge HTML tooltips are a large JSON payload; only emit them when
    # the graph is small enough for a human to hover over an edge anyway.
    edges$title <- paste0("Co-occurrences: ", edges$connections,
                          " &middot; weight ", sprintf("%.3f", edges$weight))
  }

  stats <- list(
    n_nodes            = nrow(nodes),
    n_edges            = nrow(edges),
    n_edges_before     = n_edges_raw,
    n_backbone_removed = n_backbone_removed,
    n_edge_capped      = n_capped,
    n_node_capped      = n_node_capped,
    n_oversize_papers  = proj$n_oversize,
    n_hub_nodes        = proj$n_hubs,
    weight_scheme      = weight_scheme,
    oversize_mode      = oversize_mode,
    component_filter   = component_filter,
    drop_isolates      = isTRUE(drop_isolates),
    n_isolates         = sum(igraph::degree(g) == 0L),
    betweenness_method = betweenness_method,
    modularity         = modularity_score,
    community_algorithm = community_info$algorithm,
    community_resolution = community_info$resolution,
    community_note     = community_info$note,
    seed               = seed,
    identity_policy    = tk$identity_policy,
    identity_id_column = tk$id_col,
    identity_raw_forms = nrow(tk$identity),
    identity_merged_nodes = sum(!duplicated(tk$identity$key[tk$identity$n_raw_forms > 1L])),
    identity_merged_raw_forms = sum(tk$identity$n_raw_forms > 1L),
    n_communities      = length(unique(memb)),
    build_secs         = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
  )

  if (isTRUE(verbose)) message(
    sprintf("build_collaboration_network(): %d nodes / %d edges in %.2fs (%s weighting)",
            stats$n_nodes, stats$n_edges, stats$build_secs, weight_scheme))

  list(nodes = nodes, edges = edges, graph = g, stats = stats, identity = tk$identity)
}


# -----------------------------------------------------------------------------
# Vectorised, word-boundary target matching.
#
# Replaces sapply(id, function(x) any(str_detect(x, fixed(terms)))), which was
# one closure call per node and matched substrings ("Li" hit "Elias").
# -----------------------------------------------------------------------------
cn_match_targets <- function(ids, terms) {
  if (!length(terms) || !length(ids)) return(rep(FALSE, length(ids)))
  terms <- terms[!is.na(terms) & nzchar(trimws(terms))]
  if (!length(terms)) return(rep(FALSE, length(ids)))
  # escape regex metacharacters using base R
  esc <- function(x) gsub("([\\^$.|?*+()\\/\\[\\]{}])", "\\\\\\1", x, perl = TRUE)
  pat <- paste0("\\b(", paste(esc(terms), collapse = "|"), ")\\b")
  grepl(pat, ids, ignore.case = TRUE, perl = TRUE)
}


# =============================================================================
# Level-of-detail: collapse a large graph to its community metagraph.
#
# Use this above ~3000 vertices so the first render is legible, then expand a
# single community on demand with cn_expand_community().
# =============================================================================
cn_collapse_communities <- function(net, min_size = 2L) {

  g <- net$graph
  if (is.null(g) || igraph::vcount(g) == 0L) return(net)

  memb <- net$nodes$community[match(igraph::V(g)$name, net$nodes$id)]
  cg   <- igraph::contract(g, memb, vertex.attr.comb = "ignore")
  cg   <- igraph::simplify(cg, remove.multiple = TRUE, remove.loops = TRUE,
                           edge.attr.comb = list(weight = "sum", count = "sum"))

  sizes <- as.integer(table(factor(memb, levels = seq_len(max(memb)))))

  # Name each community by its highest-strength member: far more informative
  # than "Community 7".
  reps <- vapply(seq_len(max(memb)), function(k) {
    idx <- which(memb == k)
    if (!length(idx)) return(NA_character_)
    net$nodes$id[match(igraph::V(g)$name[idx][which.max(net$nodes$strength[
      match(igraph::V(g)$name[idx], net$nodes$id)])], net$nodes$id)]
  }, character(1))

  igraph::V(cg)$name <- ifelse(is.na(reps), paste0("C", seq_len(max(memb))),
                               paste0(reps, " +", pmax(sizes - 1L, 0L)))

  keep <- which(sizes >= min_size)
  if (length(keep) && length(keep) < igraph::vcount(cg)) {
    cg <- igraph::induced_subgraph(cg, keep)
    sizes <- sizes[keep]
  }

  coords <- cn_layout(cg, engine = "fr", fr_iterations = 300L)

  nodes <- data.frame(
    id    = igraph::V(cg)$name,
    label = igraph::V(cg)$name,
    size  = 14 + log1p(sizes) * 8,
    members = sizes,
    group = "Community",
    x = coords[, 1], y = coords[, 2],
    title = paste0("<b>", cn_escape_html(igraph::V(cg)$name), "</b><br>", sizes, " members"),
    stringsAsFactors = FALSE
  )
  el <- igraph::as_edgelist(cg, names = TRUE)
  edges <- data.frame(from = el[, 1], to = el[, 2],
                      value = igraph::E(cg)$weight,
                      connections = as.integer(igraph::E(cg)$count),
                      stringsAsFactors = FALSE)

  list(nodes = nodes, edges = edges, graph = cg,
       stats = c(net$stats, list(view = "community-metagraph")))
}


cn_expand_community <- function(net, community_id) {
  g <- net$graph
  if (is.null(g)) return(net)
  keep <- which(net$nodes$community[match(igraph::V(g)$name, net$nodes$id)] ==
                  community_id)
  if (!length(keep)) return(net)
  sg <- igraph::induced_subgraph(g, keep)
  coords <- cn_layout(sg, engine = "fr", fr_iterations = 400L)
  nd <- net$nodes[match(igraph::V(sg)$name, net$nodes$id), , drop = FALSE]
  nd$x <- coords[, 1]; nd$y <- coords[, 2]
  el <- igraph::as_edgelist(sg, names = TRUE)
  list(nodes = nd,
       edges = data.frame(from = el[, 1], to = el[, 2],
                          value = igraph::E(sg)$weight,
                          connections = as.integer(igraph::E(sg)$count),
                          stringsAsFactors = FALSE),
       graph = sg,
       stats = c(net$stats, list(view = paste0("community-", community_id))))
}


# =============================================================================
# Graph-level metrics, for a "Network Metrics" panel.

# -----------------------------------------------------------------------------
# P0-2: path-length conventions.
#
# Edge weights in this engine are *tie strengths* (larger = closer), so every
# path-based quantity must use distances 1/w - exactly what betweenness,
# closeness and harmonic centrality use. igraph silently uses E(g)$weight as a
# *distance* when weights = NULL, which inverted the meaning; we therefore
# always pass weights explicitly: cn_dist_weights(g) for the weighted version
# and NA for unweighted hop counts.
# -----------------------------------------------------------------------------
cn_dist_weights <- function(g) {
  w <- igraph::E(g)$weight
  if (is.null(w)) return(NA)
  1 / pmax(w, 1e-9)
}

# -----------------------------------------------------------------------------
# P0-3: betweenness centrality (normalised, distances 1/w).
#
# approx = NULL (default)  -> exact Brandes, any size.
# approx = list(method = "sample", k = K, seed = S)
#                          -> Brandes-Pich source sampling: exact dependency
#                             accumulation from K random sources, scaled by n/K.
#                             Explicitly opt-in and labelled "approximate".
#                             (The former weighted-length cutoff was removed:
#                             it discarded most paths, see Phase-2 E5.)
# Returns list(values, method, approximate).
# -----------------------------------------------------------------------------
# Exact Brandes dependency accumulation from the given sources only (weighted, distances = wts).
# Returns undirected betweenness contributions (pairs counted once, like igraph). Used by the opt-in
# source-sampling approximation in cn_betweenness().
cn_brandes_sources <- function(g, sources, wts, tol = 1e-9) {
  nv <- igraph::vcount(g); el <- igraph::as_edgelist(g, names = FALSE)
  w <- if (is.null(wts) || (length(wts) == 1L && is.na(wts))) rep(1, nrow(el)) else as.numeric(wts)
  a <- c(el[, 1], el[, 2]); b <- c(el[, 2], el[, 1]); ww <- c(w, w)
  bc <- numeric(nv)
  for (s in sources) {
    d <- as.numeric(igraph::distances(g, v = s, weights = wts))
    ok <- is.finite(d[a]) & is.finite(d[b]) & abs(d[a] + ww - d[b]) <= tol * pmax(1, d[b])   # a is a predecessor of b
    pa <- a[ok]; pb <- b[ok]
    ord <- order(d, na.last = NA); ord <- ord[is.finite(d[ord])]
    preds <- split(pa, factor(pb, levels = seq_len(nv)))
    sigma <- numeric(nv); sigma[s] <- 1
    for (v in ord) if (v != s) { p <- preds[[v]]; if (length(p)) sigma[v] <- sum(sigma[p]) }
    delta <- numeric(nv)
    for (v in rev(ord)) { p <- preds[[v]]; if (length(p) && sigma[v] > 0) delta[p] <- delta[p] + sigma[p] / sigma[v] * (1 + delta[v]) }
    delta[s] <- 0; bc <- bc + delta
  }
  bc / 2   # undirected: each pair is seen from both endpoints when all sources are used
}

cn_betweenness <- function(g, approx = NULL, normalized = TRUE) {
  nv <- igraph::vcount(g)
  if (nv == 0L) return(list(values = numeric(0), method = "exact (Brandes, 1/w)", approximate = FALSE))
  wts <- cn_dist_weights(g)
  if (is.null(approx) || identical(approx$method, "exact")) {
    v <- tryCatch(igraph::betweenness(g, weights = wts, normalized = FALSE),
                  error = function(e) rep(NA_real_, nv))
    meth <- "exact (Brandes, 1/w)"; apx <- FALSE
  } else if (identical(approx$method, "sample")) {
    # Opt-in Brandes-Pich source sampling: exact single-source dependency accumulation (Brandes 2001) from k
    # uniformly sampled sources over the FULL graph, scaled by n/k. Unbiased for exact betweenness; the error
    # shrinks ~ 1/sqrt(k) (see VALIDATION_AFTER.md for measured rank correlation). Labelled approximate.
    k <- min(nv, max(1L, as.integer(approx$k %||% 200L)))
    src <- with_seed(approx$seed %||% 42L, sample.int(nv, k))
    v <- cn_brandes_sources(g, src, wts) * nv / k
    meth <- sprintf("approximate (Brandes-Pich source sampling, k = %d of %d sources, 1/w)", k, nv); apx <- TRUE
  } else {
    stop("cn_betweenness(): unknown approx$method '", approx$method, "'")
  }
  if (isTRUE(normalized)) {
    # undirected normalisation 2 / ((n-1)(n-2)); defined as 0 for n <= 2
    # (no vertex can lie strictly between two others).
    v <- if (nv <= 2L) rep(0, nv) else v * 2 / ((nv - 1) * (nv - 2))
  }
  list(values = v, method = meth, approximate = apx)
}

# Average path length over reachable pairs. Exact up to `exact_max_nodes`.
# Returns list(value, lo, hi, estimate, n_sources).
cn_mean_path_length <- function(g, weighted = TRUE, exact_max_nodes = 5000L,
                                n_sources = 300L, seed = 42L) {
  nv <- igraph::vcount(g)
  wts <- if (weighted) cn_dist_weights(g) else NA
  if (nv < 2L || igraph::ecount(g) == 0L)
    return(list(value = NA_real_, lo = NA_real_, hi = NA_real_, estimate = FALSE, n_sources = 0L))
  if (nv <= exact_max_nodes) {
    v <- igraph::mean_distance(g, weights = wts, directed = FALSE, unconnected = TRUE)
    return(list(value = v, lo = v, hi = v, estimate = FALSE, n_sources = nv))
  }
  cn_mean_path_length_sampled(g, wts, n_sources = n_sources, seed = seed)
}

# P0-4: APL estimate for large graphs. Samples k *source* vertices uniformly
# from the full graph and computes their distances to *all* vertices (no
# induced-subgraph sampling, which destroyed paths and biased APL upward).
# Per-source mean distances over reachable targets are averaged, weighted by
# reachable-target counts (ratio estimator); 95% CI by normal approximation
# on the per-source means. Labelled "estimate" by the caller.
cn_mean_path_length_sampled <- function(g, wts, n_sources = 300L, seed = 42L) {
  nv <- igraph::vcount(g)
  k  <- min(nv, max(2L, as.integer(n_sources)))
  src <- with_seed(seed, sort(sample.int(nv, k)))
  sums <- numeric(k); cnts <- numeric(k)
  for (b in split(seq_len(k), ceiling(seq_len(k) / 50L))) {   # 50 x n chunks
    D <- igraph::distances(g, v = src[b], weights = wts)
    D[!is.finite(D)] <- NA
    D[cbind(seq_along(b), src[b])] <- NA                     # drop self-pairs
    sums[b] <- rowSums(D, na.rm = TRUE)
    cnts[b] <- rowSums(!is.na(D))
  }
  ok <- cnts > 0
  if (!any(ok)) return(list(value = NA_real_, lo = NA_real_, hi = NA_real_, estimate = TRUE, n_sources = k))
  est <- sum(sums[ok]) / sum(cnts[ok])
  m   <- sums[ok] / cnts[ok]
  # linearised SE of the ratio estimator
  r   <- (sums[ok] - est * cnts[ok]) / mean(cnts[ok])
  se  <- if (sum(ok) > 1) stats::sd(r) / sqrt(sum(ok)) * sqrt(1 - sum(ok) / nv) else NA_real_
  list(value = est, lo = est - 1.96 * se, hi = est + 1.96 * se, estimate = TRUE,
       n_sources = sum(ok), per_source_mean = m)
}

# Weighted (1/w) or hop diameter / radius. Radius is only defined for a
# connected graph (NA otherwise).
cn_diameter_radius <- function(g, weighted = TRUE) {
  wts <- if (weighted) cn_dist_weights(g) else NA
  diam <- tryCatch(igraph::diameter(g, directed = FALSE, unconnected = TRUE, weights = wts),
                   error = function(e) NA_real_)
  rad <- tryCatch({
    if (igraph::components(g)$no == 1L) {
      ecc <- tryCatch(igraph::eccentricity(g, weights = wts),
                      error = function(e) if (weighted) NULL else igraph::eccentricity(g))
      if (is.null(ecc)) NA_real_ else min(ecc)
    } else NA_real_
  }, error = function(e) NA_real_)
  list(diameter = diam, radius = rad)
}

# =============================================================================
cn_network_metrics <- function(net) {

  g <- net$graph
  if (is.null(g) || igraph::vcount(g) == 0L)
    return(data.frame(Metric = character(0), Value = character(0)))

  nv   <- igraph::vcount(g)
  ne   <- igraph::ecount(g)
  comp <- igraph::components(g)
  deg  <- igraph::degree(g)
  w    <- igraph::E(g)$weight

  gini <- function(x) {
    x <- sort(x[is.finite(x)])
    n <- length(x)
    if (n < 2 || sum(x) == 0) return(NA_real_)
    (2 * sum(seq_len(n) * x) / (n * sum(x))) - (n + 1) / n
  }

  # P0-2: weighted (1/w) and hop path lengths, both reported and labelled.
  apl_w <- tryCatch(cn_mean_path_length(g, weighted = TRUE),  error = function(e) list(value = NA_real_, estimate = FALSE))
  apl_h <- tryCatch(cn_mean_path_length(g, weighted = FALSE), error = function(e) list(value = NA_real_, estimate = FALSE))
  fmt_apl <- function(a) {
    if (is.null(a$value) || is.na(a$value)) return("\u2014")
    if (isTRUE(a$estimate))
      sprintf("%.3f (estimate, 95%% CI %.3f\u2013%.3f, %d sampled sources)", a$value, a$lo, a$hi, a$n_sources)
    else formatC(a$value, format = "f", digits = 3)
  }

  # Freeman Centralizations
  centr_d <- tryCatch(igraph::centr_degree(g, normalized = TRUE)$centralization, error = function(e) NA_real_)
  centr_b <- tryCatch({
    if (nv <= 1500L) igraph::centr_betw(g, normalized = TRUE)$centralization
    else NA_real_
  }, error = function(e) NA_real_)
  centr_c <- tryCatch({
    if (nv <= 1500L && comp$no == 1L) igraph::centr_clo(g, normalized = TRUE)$centralization
    else NA_real_
  }, error = function(e) NA_real_)

  # Cut-vertices
  n_cut_v <- sum(net$nodes$is_cut_vertex %||% FALSE)

  fmt <- function(x, d = 3) if (is.na(x)) "\u2014" else formatC(x, format = "f", digits = d)

  data.frame(
    Metric = c(
      "Nodes", "Edges", "Density",
      "Components", "Largest component (%)",
      "Global clustering (transitivity)",
      "Degree assortativity",
      "Average path length (weighted, 1/w)",
      "Average path length (hops)",
      "Mean degree", "Max degree",
      "Degree Gini (inequality)",
      "Degree Centralization (Freeman)",
      "Betweenness Centralization",
      "Closeness Centralization",
      "Critical Brokers (Cut-Vertices)",
      sprintf("Communities (%s)", net$stats$community_algorithm %||% "Leiden"), "Modularity",
      "Mean edge weight",
      "Edges pruned by backbone filter",
      "Mega-collaborations folded to hubs"
    ),
    Value = c(
      format(nv, big.mark = ","),
      format(ne, big.mark = ","),
      fmt(igraph::edge_density(g), 5),
      format(comp$no, big.mark = ","),
      fmt(100 * max(comp$csize) / nv, 1),
      fmt(igraph::transitivity(g, type = "global")),
      fmt(suppressWarnings(igraph::assortativity_degree(g, directed = FALSE))),
      fmt_apl(apl_w),
      fmt_apl(apl_h),
      fmt(mean(deg), 2),
      format(max(deg), big.mark = ","),
      fmt(gini(deg)),
      fmt(centr_d, 3),
      fmt(centr_b, 3),
      fmt(centr_c, 3),
      format(n_cut_v, big.mark = ","),
      format(net$stats$n_communities %||% NA, big.mark = ","),
      fmt(net$stats$modularity %||% NA_real_),
      fmt(mean(w)),
      format(net$stats$n_backbone_removed %||% 0L, big.mark = ","),
      format(net$stats$n_oversize_papers %||% 0L, big.mark = ",")
    ),
    stringsAsFactors = FALSE
  )
}

# C-8 (audit High): exact Dirac / Ore check. Previously Ore was "verified" on
# <= 500 random vertex pairs, so a single violating non-adjacent pair could be
# missed and the graph declared "Guaranteed Hamiltonian". This helper is exact
# for every graph it evaluates: for each vertex u, the smallest deg(u) + deg(v)
# over non-neighbours v is attained at the lowest-degree non-neighbour, which is
# found by scanning the degree-sorted vertex list - at most deg(u) + 2 steps.
# Total cost O(n log n + m), memory O(n + m). Degrees are taken on the
# simplified undirected graph (loops / parallel edges must not inflate them).
cn_ore_dirac_exact <- function(g, max_n = 200000L) {
  out <- list(n = 0L, dirac_met = FALSE, ore_met = FALSE, ore_min_sum = NA_integer_,
              ore_pair = c(NA_integer_, NA_integer_), n_nonadj_pairs = NA_real_,
              exact = FALSE, min_deg = 0L, dirac_threshold = 0L, reason = "")
  if (is.null(g)) { out$reason <- "no graph"; return(out) }
  nv <- igraph::vcount(g); out$n <- nv
  if (nv < 3L) {
    # Dirac/Ore require n >= 3; neither applies (not a proof either way).
    out$exact <- TRUE; out$reason <- "n < 3 (theorems require n >= 3)"
    out$dirac_threshold <- as.integer(ceiling(nv / 2))
    return(out)
  }
  if (nv > max_n) { out$reason <- paste0("n = ", nv, " exceeds exact-check limit ", max_n); return(out) }
  gs <- if (igraph::is_directed(g)) igraph::as_undirected(g, mode = "collapse") else g
  gs <- igraph::simplify(gs, remove.multiple = TRUE, remove.loops = TRUE)
  deg <- as.integer(igraph::degree(gs))
  out$min_deg <- min(deg)
  out$dirac_threshold <- as.integer(ceiling(nv / 2))
  out$dirac_met <- out$min_deg >= out$dirac_threshold
  out$n_nonadj_pairs <- nv * (nv - 1) / 2 - igraph::ecount(gs)
  adj <- igraph::as_adj_list(gs, mode = "all")
  ord <- order(deg)
  best <- .Machine$integer.max; bu <- NA_integer_; bv <- NA_integer_
  for (u in seq_len(nv)) {
    k <- min(nv, deg[u] + 2L)
    cand <- ord[seq_len(k)]
    cand <- cand[cand != u & !(cand %in% as.integer(adj[[u]]))]
    if (length(cand)) {
      s <- deg[u] + deg[cand[1L]]
      if (s < best) { best <- s; bu <- u; bv <- cand[1L] }
    }
  }
  out$exact <- TRUE
  if (is.na(bu)) {            # complete graph: Ore holds vacuously
    out$ore_min_sum <- NA_integer_; out$ore_met <- TRUE
  } else {
    out$ore_min_sum <- as.integer(best); out$ore_pair <- c(bu, bv)
    out$ore_met <- best >= nv
  }
  out
}

# =============================================================================
# Hamiltonian & Topological Diagnostics
#
# Analyzes Hamiltonian cycle existence, sufficient conditions (Dirac, Ore),
# necessary conditions (2-connectivity, absence of cut vertices/bridges),
# Eulerian status, girth, diameter, and radius.
# =============================================================================
cn_hamiltonian_diagnostics <- function(net) {

  g <- net$graph
  if (is.null(g) || igraph::vcount(g) == 0L)
    return(data.frame(Property = character(0), Status = character(0)))

  nv   <- igraph::vcount(g)
  ne   <- igraph::ecount(g)
  comp <- igraph::components(g)
  deg  <- igraph::degree(g)
  min_deg <- if (length(deg)) min(deg) else 0L

  # Articulation points (cut-vertices)
  cut_v <- which(net$nodes$is_cut_vertex %||% FALSE)
  n_cut_v <- length(cut_v)

  # Bridges (cut-edges)
  bridges_count <- tryCatch({
    bic <- igraph::biconnected_components(g)
    sum(lengths(bic$edges) == 1L)
  }, error = function(e) NA_integer_)

  # Dirac's / Ore's conditions (C-8): exact check over ALL non-adjacent pairs.
  od <- tryCatch(cn_ore_dirac_exact(g), error = function(e) NULL)
  ore_exact <- isTRUE(od$exact)
  dirac_threshold <- if (!is.null(od)) od$dirac_threshold else ceiling(nv / 2)
  dirac_met <- ore_exact && isTRUE(od$dirac_met)
  ore_met   <- ore_exact && isTRUE(od$ore_met)
  ore_min_sum <- if (ore_exact) od$ore_min_sum else NA_integer_
  if (ore_exact && nv >= 3L) min_deg <- od$min_deg

  # Biconnectivity
  is_biconnected <- (comp$no == 1L && n_cut_v == 0L && nv >= 3L)

  # Eulerian status
  has_eul_cycle <- tryCatch(igraph::has_eulerian_cycle(g), error = function(e) FALSE)
  has_eul_path  <- tryCatch(igraph::has_eulerian_path(g), error = function(e) FALSE)

  # Girth (length of shortest cycle)
  girth_val <- tryCatch(igraph::girth(g)$girth, error = function(e) NA_real_)

  # Diameter & Radius
  # P0-2: igraph's default would treat tie strength as a distance. Report the
  # weighted (1/w) and the hop versions explicitly.
  dr_w <- cn_diameter_radius(g, weighted = TRUE)
  dr_h <- cn_diameter_radius(g, weighted = FALSE)
  fnum <- function(x) if (is.na(x)) "-" else trimws(formatC(x, format = "fg", digits = 4))

  # Overall Hamiltonian Assessment
  ham_verdict <- if (isTRUE(dirac_met)) {
    "Guaranteed Hamiltonian (Dirac's condition proven exactly)"
  } else if (isTRUE(ore_met)) {
    "Guaranteed Hamiltonian (Ore's condition proven exactly over all non-adjacent pairs)"
  } else if (isTRUE(comp$no > 1L)) {
    paste0("Provably Non-Hamiltonian (Graph is disconnected: ", comp$no, " components)")
  } else if (isTRUE(min_deg < 2L)) {
    paste0("Provably Non-Hamiltonian (Degree < 2 vertex exists; delta = ", min_deg, ")")
  } else if (isTRUE(n_cut_v > 0L)) {
    paste0("Provably Non-Hamiltonian (Contains ", n_cut_v, " articulation point / cut vertices)")
  } else if (!is.na(bridges_count) && isTRUE(bridges_count > 0L)) {
    paste0("Provably Non-Hamiltonian (Contains ", bridges_count, " bridge edge(s))")
  } else {
    paste0("Feasible Candidate (2-connected, bridge-free, degree >= 2; heuristic / not proven",
           if (!ore_exact && nv >= 3L) "; Dirac/Ore not evaluated exactly" else "", ")")
  }

  data.frame(
    Property = c(
      "Hamiltonian Feasibility",
      "Dirac's Condition",
      "Ore's Condition",
      "Biconnectivity (2-Connected)",
      "Articulation Points (Cut-Vertices)",
      "Bridges (Cut-Edges)",
      "Eulerian Circuit",
      "Eulerian Path",
      "Girth (Shortest Cycle)",
      "Graph Diameter (hops)",
      "Graph Diameter (weighted, 1/w)",
      "Graph Radius (hops)",
      "Graph Radius (weighted, 1/w)"
    ),
    Status = c(
      ham_verdict,
      if (nv < 3L) "N/A (requires n >= 3)" else if (!ore_exact) paste0("Not evaluated (heuristic / not proven: ", od$reason %||% "error", ")") else if (isTRUE(dirac_met)) "Satisfied (proven exactly; Guaranteed Cycle)" else paste0("Not met (delta = ", min_deg, ", need >= ", dirac_threshold, ")"),
      if (nv < 3L) "N/A (requires n >= 3)" else if (!ore_exact) paste0("Not evaluated (heuristic / not proven: ", od$reason %||% "error", ")") else if (isTRUE(ore_met)) (if (is.na(ore_min_sum)) "Satisfied (proven exactly; complete graph, no non-adjacent pairs)" else paste0("Satisfied (proven exactly over all ", format(od$n_nonadj_pairs, big.mark = ",", scientific = FALSE), " non-adjacent pairs; min sum = ", ore_min_sum, ")")) else paste0("Not met (exact: min sum = ", ore_min_sum, ", need >= ", nv, ")"),
      if (isTRUE(is_biconnected)) "Yes (Robust, no single cut vertex)" else "No (Vulnerable to single point of failure)",
      if (isTRUE(n_cut_v == 0L)) "0 (Robust)" else paste0(n_cut_v, " critical broker(s)"),
      if (is.na(bridges_count)) "-" else if (bridges_count == 0L) "0 (Bridge-free)" else paste0(bridges_count, " bridge edge(s)"),
      if (isTRUE(has_eul_cycle)) "Yes (All vertices have even degree)" else "No",
      if (isTRUE(has_eul_path)) "Yes (At most 2 vertices with odd degree)" else "No",
      if (is.na(girth_val) || is.infinite(girth_val) || girth_val == 0) "None (Acyclic/Forest)" else as.character(girth_val),
      fnum(dr_h$diameter),
      fnum(dr_w$diameter),
      fnum(dr_h$radius),
      fnum(dr_w$radius)
    ),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Centrality Leaderboard: Top authors by multiple centralities
# =============================================================================
cn_centrality_leaderboard <- function(net, top_n = 15L) {
  nd <- net$nodes
  if (is.null(nd) || nrow(nd) == 0L) return(data.frame())

  # Rank primarily by betweenness, then strength
  ord <- order(nd$betweenness %||% 0, nd$strength %||% 0, decreasing = TRUE)
  nd_top <- nd[head(ord, top_n), , drop = FALSE]

  roles <- vapply(seq_len(nrow(nd_top)), function(i) {
    is_cut <- isTRUE(nd_top$is_cut_vertex[i])
    btw <- nd_top$betweenness[i] %||% 0
    deg <- nd_top$degree[i] %||% 0
    if (is_cut) "Critical Broker (Cut-Vertex)"
    else if (btw > 0.05 && deg >= 5) "Strategic Bridge"
    else if (deg >= 8) "Core Hub"
    else "Collaborator"
  }, character(1))

  data.frame(
    Rank = seq_len(nrow(nd_top)),
    Author = nd_top$label,
    Degree = nd_top$degree,
    Strength = nd_top$strength,
    Betweenness = formatC(nd_top$betweenness %||% 0, format = "f", digits = 4),
    Closeness = formatC(nd_top$closeness %||% 0, format = "f", digits = 4),
    Harmonic = formatC(nd_top$harmonic %||% 0, format = "f", digits = 4),
    PageRank = formatC(nd_top$pagerank %||% 0, format = "f", digits = 4),
    Coreness = nd_top$coreness %||% 0,
    Role = roles,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Centrality Scatter / Correlation Plot
# Visualizes Betweenness Centrality vs Closeness Centrality
# =============================================================================
cn_centrality_scatter_plot <- function(net) {
  nd <- net$nodes
  if (is.null(nd) || nrow(nd) == 0L) {
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No network data available", size = 5) +
      ggplot2::theme_void()
    return(p)
  }

  df <- nd
  df$btw <- df$betweenness %||% 0
  df$clo <- df$closeness %||% 0
  df$size_wt <- pmax(df$strength %||% 1, 1)

  # Top brokers to label
  top_brokers <- head(order(df$btw, decreasing = TRUE), 8)
  df$label_author <- ""
  df$label_author[top_brokers] <- df$label[top_brokers]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = clo, y = btw)) +
    ggplot2::geom_point(ggplot2::aes(size = size_wt, color = group), alpha = 0.75) +
    ggplot2::geom_text(ggplot2::aes(label = label_author),
                       size = 3.2, fontface = "bold", vjust = -0.8, hjust = 0.5, check_overlap = TRUE) +
    ggplot2::scale_size_continuous(range = c(2.5, 9), name = "Weighted Strength") +
    ggplot2::scale_color_manual(values = c("Queried Target" = "#e74c3c",
                                          "Associated Entity" = "#3498db",
                                          "Mega-collaboration" = "#9b59b6")) +
    ggplot2::labs(
      title = "Centrality Map: Gatekeepers vs. Core Hubs",
      subtitle = "Betweenness Centrality (Brokering/Bottlenecks) vs. Closeness Centrality (Dissemination Speed)",
      x = "Closeness Centrality (Normalized)",
      y = "Betweenness Centrality (Normalized)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, color = "#2c3e50"),
      plot.subtitle = ggplot2::element_text(size = 10, color = "#7f8c8d"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}

`%||%` <- function(a, b) if (is.null(a)) b else a


# =============================================================================
# Ego-network / brokerage metrics for the queried author(s).
#
# These are the numbers that actually say something about a researcher's
# collaboration position, as opposed to raw degree.
# =============================================================================
cn_ego_metrics <- function(net, targets) {

  g <- net$graph
  if (is.null(g) || !length(targets))
    return(data.frame())

  hit <- which(cn_match_targets(igraph::V(g)$name, targets))
  if (!length(hit)) return(data.frame())

  out <- lapply(hit, function(v) {
    nb  <- igraph::neighbors(g, v)
    k   <- length(nb)
    if (k == 0L) return(NULL)
    ego <- igraph::induced_subgraph(g, c(v, as.integer(nb)))
    alt <- igraph::induced_subgraph(g, as.integer(nb))

    # Burt's effective size, unweighted approximation:
    #   n - 2t/n , where t = ties among alters
    eff <- k - (2 * igraph::ecount(alt)) / max(k, 1)

    data.frame(
      Entity            = igraph::V(g)$name[v],
      Collaborators     = k,
      Strength          = round(igraph::strength(g, v, weights = igraph::E(g)$weight), 3),
      `Ego density`     = round(igraph::edge_density(alt), 4),
      `Effective size`  = round(eff, 2),
      `Redundancy`      = round(k - eff, 2),
      `Constraint`      = round(igraph::constraint(g, v, weights = igraph::E(g)$weight), 4),
      `Local clustering`= round(igraph::transitivity(g, type = "local", vids = v), 4),
      Community         = net$nodes$community[match(igraph::V(g)$name[v], net$nodes$id)],
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, Filter(Negate(is.null), out))
}


# =============================================================================
# Bibliometric collaboration metrics computed straight off the publication
# table, independent of what survived the render filters.
# =============================================================================
cn_bibliometrics <- function(df, target_col = "Authors", target_delim = ",",
                             year_col = "Year", cite_col = "Citations") {

  tk <- cn_tokenise(df, target_col, target_delim)
  if (is.null(tk)) return(data.frame())

  paper_ids   <- sort(unique(tk$pid))       # rows of df that have >= 1 entity
  k_per_paper <- tabulate(match(tk$pid, paper_ids))
  n_papers    <- length(k_per_paper)
  n_authors   <- tk$n_ent

  # Fractional (1/k) productivity credit, the standard correction for
  # hyperauthorship.
  frac <- tapply(1 / k_per_paper[match(tk$pid, sort(unique(tk$pid)))],
                 tk$code, sum)

  single <- sum(k_per_paper == 1L)

  years <- if (year_col %in% names(df)) suppressWarnings(as.numeric(df[[year_col]])) else NULL
  cites <- if (cite_col %in% names(df)) suppressWarnings(as.numeric(df[[cite_col]])) else NULL
  # C-13: align citations with k_per_paper (one entry per paper that has
  # entities, in paper_ids order) instead of dividing the full column and
  # letting R recycle the shorter vector.
  cites_k <- if (!is.null(cites)) cites[paper_ids] else NULL

  rows <- list(
    c("Publications", format(n_papers, big.mark = ",")),
    c("Distinct entities", format(n_authors, big.mark = ",")),
    c("Mean entities per publication", sprintf("%.2f", mean(k_per_paper))),
    c("Median entities per publication", format(stats::median(k_per_paper))),
    c("Max entities on one publication", format(max(k_per_paper), big.mark = ",")),
    c("Single-entity publications", sprintf("%d (%.1f%%)", single, 100 * single / n_papers)),
    c("Hyperauthored (>100 entities)",
      sprintf("%d (%.1f%%)", sum(k_per_paper > 100), 100 * mean(k_per_paper > 100))),
    c("Collaboration coefficient",
      sprintf("%.3f", 1 - sum(1 / k_per_paper) / n_papers)),
    c("Degree of collaboration (Subramanyam)",
      sprintf("%.3f", 1 - single / n_papers)),
    c("Mean fractional credit", sprintf("%.3f", mean(frac, na.rm = TRUE)))
  )

  if (!is.null(cites) && any(is.finite(cites))) {
    rows <- c(rows, list(
      c("Total citations", format(sum(cites, na.rm = TRUE), big.mark = ",")),
      c("Citations per author-adjusted paper",
        sprintf("%.2f", sum(cites_k / k_per_paper, na.rm = TRUE) / n_papers))
    ))
  }
  if (!is.null(years) && any(is.finite(years))) {
    rows <- c(rows, list(
      c("Year span", sprintf("%d\u2013%d", min(years, na.rm = TRUE), max(years, na.rm = TRUE)))
    ))
  }

  data.frame(Metric = vapply(rows, `[`, character(1), 1),
             Value  = vapply(rows, `[`, character(1), 2),
             stringsAsFactors = FALSE)
}


# =============================================================================
# Slider bound calculation.
#
# The old observer recomputed the full k^2 self-join purely to find the max
# edge frequency and max component size, on *unfiltered* data, every time any
# of three inputs changed - i.e. it paid the most expensive operation in the
# app twice per interaction. This does it from the sparse projection, honours
# the paper-size cap, and returns everything the UI needs in one pass.
# =============================================================================
cn_slider_bounds <- function(df, target_col, target_delim = ",",
                             size_max = 100L, weight_scheme = "count",
                             oversize_mode = "star") {

  out <- list(max_node_occurrence = 1L, max_edge_frequency = 1L,
              max_cluster_size = 2L, max_paper_size = 1L, n_entities = 0L)

  tk <- cn_tokenise(df, target_col, target_delim)
  if (is.null(tk)) return(out)

  occ <- tabulate(tk$code, nbins = tk$n_ent)
  ks  <- tabulate(match(tk$pid, sort(unique(tk$pid))))

  out$max_node_occurrence <- max(occ, 1L)
  out$max_paper_size      <- max(ks, 1L)
  out$n_entities          <- tk$n_ent

  # P0-1: must use the same oversize_mode as the build, otherwise hub mode
  # yields components larger than the slider max and the fused block is
  # silently deleted by the default window.
  proj <- cn_project(tk$pid, tk$code, tk$n_ent, 1L, size_max,
                     weight_scheme = "count", oversize_mode = oversize_mode)
  e <- proj$edges
  if (!length(e$i)) return(out)

  out$max_edge_frequency <- max(e$cnt, 1L)

  g <- igraph::graph_from_edgelist(cbind(e$i, e$j), directed = FALSE)
  out$max_cluster_size <- max(igraph::components(g)$csize, 2L)
  out
}

# P0-5b: run the WHOLE build under a local seed. Besides the explicit seeding of communities/layout, igraph
# routines (ARPACK eigenvector centrality, layouts, Leiden) draw from R's RNG; wrapping the build makes every
# result reproducible for a given seed AND leaves the caller's .Random.seed untouched.
build_collaboration_network <- function(df, ..., seed = 42L) {
  with_seed(seed, .cn_build_collaboration_network_impl(df, ..., seed = seed))
}

# -----------------------------------------------------------------------------
# P1-6: sensitivity bands. Rebuild the network under alternative identity policies and counting modes and report,
# for every author that is top-N under ANY configuration, the min-max rank across configurations and in how many
# configurations they are top-N. Counting modes: "full" (count weights, oversize papers expanded as cliques),
# "fractional" (Newman 1/(n-1), cliques), "hub" (Newman, oversize papers as hub nodes; CollabNET default).
# Bounded: no communities, grid layout, at most max_papers rows. Authors are matched across policies by their
# 'initials' identity key of the node label (the coarsest key), so the bands are conservative.
# -----------------------------------------------------------------------------
CN_COUNTING_MODES <- list(full = c(weight = "count", oversize = "clique"), fractional = c(weight = "newman", oversize = "clique"),
                          hub = c(weight = "newman", oversize = "star"))
cn_sensitivity <- function(df, target_col = "Authors", target_delim = ";", metric = c("betweenness", "degree", "strength"),
                           top_n = 10L, policies = c("exact", "normalised", "initials"), modes = names(CN_COUNTING_MODES),
                           authors_per_pub = c(1, 100), max_papers = 5000L, seed = 42L) {
  metric <- match.arg(metric)
  if (!is.data.frame(df) || !nrow(df)) stop("no data", call. = FALSE)
  if (nrow(df) > max_papers) stop(sprintf("sensitivity mode is limited to %d papers (have %d); narrow the filters", max_papers, nrow(df)), call. = FALSE)
  runs <- list()
  for (pol in policies) for (md in modes) {
    cm <- CN_COUNTING_MODES[[md]]
    n <- suppressWarnings(build_collaboration_network(df, target_col = target_col, target_delim = target_delim, authors_per_pub = authors_per_pub,
           weight_scheme = cm[["weight"]], oversize_mode = cm[["oversize"]], identity_policy = pol, cluster_size_range = c(1, Inf),
           detect_communities = FALSE, compute_centrality = metric == "betweenness", layout_engine = "grid", build_tooltips = FALSE, seed = seed))
    nd <- n$nodes; if (!nrow(nd)) next
    nd <- nd[!grepl("^\\[collaboration", nd$id), , drop = FALSE]                 # hub placeholders are not people
    v <- nd[[metric]]; r <- rank(-v, ties.method = "min")
    runs[[paste(pol, md, sep = "/")]] <- data.frame(config = paste(pol, md, sep = "/"), policy = pol, mode = md,
      key = cn_identity_key(nd$id, "initials")$key, label = nd$id, value = v, rank = r, stringsAsFactors = FALSE)
  }
  all <- do.call(rbind, runs)
  all <- all[order(all$config, all$rank), ]; all <- all[!duplicated(all[c("config", "key")]), ]
  keys <- unique(all$key[all$rank <= top_n])
  sub <- all[all$key %in% keys, ]
  nconf <- length(runs)
  out <- do.call(rbind, lapply(split(sub, sub$key), function(d) data.frame(
    author = d$label[which.min(d$rank)], min_rank = min(d$rank), max_rank = max(d$rank), median_rank = stats::median(d$rank),
    configs_in_top_n = sum(d$rank <= top_n), configs_present = nrow(d), stringsAsFactors = FALSE)))
  out$max_rank[out$configs_present < nconf] <- NA                               # absent in some configuration
  out <- out[order(-out$configs_in_top_n, out$median_rank), ]; rownames(out) <- NULL
  per_mode <- do.call(rbind, lapply(split(all, all$config), function(d) data.frame(config = d$config[1], top = paste(head(d$label[order(d$rank)], top_n), collapse = "; "))))
  list(metric = metric, top_n = top_n, n_configs = nconf, configs = names(runs), bands = out,
       robust_top_n = sum(out$configs_in_top_n == nconf), per_config_top = per_mode)
}

# -----------------------------------------------------------------------------
# P2-2: time slicing. Ego trajectory of one author over sliding year windows [y - window + 1, y]: per window the
# number of papers, degree, strength, betweenness (1/w, normalised), Burt's constraint and effective size. The
# author is matched by identity key (P1-1). Bounded: at most max_windows windows, no communities, grid layout.
# -----------------------------------------------------------------------------
cn_year_window <- function(df, year_col = "Year", from = NULL, to = NULL) {
  y <- suppressWarnings(as.integer(as.character(df[[year_col]])))
  keep <- !is.na(y) & (is.null(from) | y >= (from %||% -Inf)) & (is.null(to) | y <= (to %||% Inf))
  df[keep, , drop = FALSE]
}
cn_ego_trajectory <- function(df, author, target_col = "Authors", target_delim = ";", year_col = "Year", window = 3L,
                              step = 1L, identity_policy = "normalised", max_windows = 40L, ...) {
  if (!year_col %in% names(df)) stop("year column not found", call. = FALSE)
  y <- suppressWarnings(as.integer(as.character(df[[year_col]])))
  if (all(is.na(y))) stop("no usable years", call. = FALSE)
  ends <- seq(min(y, na.rm = TRUE) + window - 1L, max(y, na.rm = TRUE), by = step)
  if (!length(ends)) ends <- max(y, na.rm = TRUE)
  ends <- utils::tail(ends, max_windows)
  akey <- cn_identity_key(author, identity_policy)$key
  rows <- lapply(ends, function(e) {
    d <- df[!is.na(y) & y >= e - window + 1L & y <= e, , drop = FALSE]
    base <- data.frame(window_start = e - window + 1L, window_end = e, papers_in_window = nrow(d), author_papers = 0L,
                       degree = NA_integer_, strength = NA_real_, betweenness = NA_real_, constraint = NA_real_, effective_size = NA_real_, nodes = 0L)
    if (!nrow(d)) return(base)
    n <- suppressWarnings(build_collaboration_network(d, target_col = target_col, target_delim = target_delim, identity_policy = identity_policy,
           cluster_size_range = c(1, Inf), detect_communities = FALSE, layout_engine = "grid", build_tooltips = FALSE, ...))
    base$nodes <- nrow(n$nodes)
    hit <- which(cn_identity_key(n$nodes$id, identity_policy)$key == akey)
    tk <- cn_tokenise(d, target_col, target_delim, identity_policy = identity_policy)
    if (!is.null(tk)) base$author_papers <- length(unique(tk$pid[tk$code == match(akey, levels(factor(cn_identity_key(tk$label, identity_policy)$key)))]))
    if (length(hit)) { nd <- n$nodes[hit[1], ]
      base$degree <- nd$degree; base$strength <- nd$strength; base$betweenness <- nd$betweenness
      base$constraint <- nd$constraint; base$effective_size <- nd$effective_size }
    base
  })
  out <- do.call(rbind, rows); attr(out, "author") <- author; attr(out, "window") <- window; out
}

# C2-1: background (browser Web Worker) layout. "auto" hands graphs with at
# least this many nodes to the worker; below it R/igraph FR/DrL is used (in
# Shinylive that already runs inside the webR worker). Threshold from the
# C2-1 measurements (TEST_RESULTS_C2.md).
CN_WORKER_LAYOUT_MIN_NODES <- 1500L

cn_use_worker_layout <- function(engine, n_nodes) {
  identical(engine, "worker") ||
    (identical(engine %||% "auto", "auto") && isTRUE(n_nodes >= CN_WORKER_LAYOUT_MIN_NODES))
}

# Payload for the collabnet_worker_layout custom message (positions are
# integer indices into ids on the JS side; ids/from/to are plain vectors).
cn_worker_layout_msg <- function(nodes, edges, seed = 42L, el = "network_filtered", iterations = NULL) {
  n <- nrow(nodes)
  list(el = el, ids = as.character(nodes$id),
       from = as.character(edges$from), to = as.character(edges$to),
       seed = as.integer(seed %||% 42L),
       iterations = as.integer(iterations %||% if (n > 3000L) 200L else 300L))
}

# C2-1: the browser reports worker coordinates back (input$network_worker_positions,
# list(ids, x, y)); server-side geometry (neighbourhood radius) uses them when
# they describe exactly the current node set.
cn_merge_worker_positions <- function(nodes, wp) {
  if (is.null(wp) || is.null(nodes) || !nrow(nodes)) return(nodes)
  ids <- as.character(unlist(wp$ids)); x <- as.numeric(unlist(wp$x)); y <- as.numeric(unlist(wp$y))
  if (length(ids) != nrow(nodes) || length(x) != length(ids) || length(y) != length(ids) ||
      !setequal(ids, as.character(nodes$id)) || anyNA(x) || anyNA(y)) return(nodes)
  m <- match(as.character(nodes$id), ids)
  nodes$x <- x[m]; nodes$y <- y[m]
  nodes
}
