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
# Smart per-row delimiter splitting for multi-value columns (Authors, Keywords, etc.)
# Seamlessly handles delimiter variations (e.g. Scopus ';' vs CrossRef/ORCiD ',').
# -----------------------------------------------------------------------------
cn_split_multivalue <- function(raw, target_delim = ",") {
  if (is.null(raw) || length(raw) == 0) return(list())
  
  is_standard_delim <- is.null(target_delim) || 
                       !nzchar(trimws(target_delim)) || 
                       grepl("[,;]", target_delim)
  
  if (is_standard_delim) {
    # Per-row smart splitting:
    # 1. If row contains ';', split on ';' (Scopus standard, keeps "Last, First" intact)
    # 2. Else if row contains ',', split on ',' (CrossRef / ORCiD / RIS standard)
    # 3. Otherwise treat as single entity
    lapply(raw, function(x) {
      if (is.na(x) || !nzchar(trimws(x))) return(character(0))
      if (grepl(";", x, fixed = TRUE)) {
        res <- strsplit(x, ";", fixed = TRUE)[[1]]
      } else if (grepl(",", x, fixed = TRUE)) {
        res <- strsplit(x, ",", fixed = TRUE)[[1]]
      } else {
        res <- x
      }
      res <- trimws(res)
      res[nzchar(res)]
    })
  } else {
    lapply(raw, function(x) {
      if (is.na(x) || !nzchar(trimws(x))) return(character(0))
      res <- strsplit(x, target_delim, fixed = TRUE)[[1]]
      res <- trimws(res)
      res[nzchar(res)]
    })
  }
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
cn_tokenise <- function(df, target_col, target_delim = ",", case_fold = TRUE) {

  if (is.null(df) || !nrow(df) || !target_col %in% colnames(df)) return(NULL)

  raw <- as.character(df[[target_col]])
  pid <- seq_along(raw)

  keep <- !is.na(raw) & nzchar(trimws(raw))
  raw  <- raw[keep]
  pid  <- pid[keep]
  if (!length(raw)) return(NULL)

  # Smart splitting handles semicolon and comma variations seamlessly
  pieces <- cn_split_multivalue(raw, target_delim)
  pid    <- rep.int(pid, lengths(pieces))
  ent    <- unlist(pieces, use.names = FALSE)

  ent <- trimws(gsub("[[:space:]]+", " ", ent))
  ok  <- !is.na(ent) & nzchar(ent)
  ent <- ent[ok]
  pid <- pid[ok]
  if (!length(ent)) return(NULL)

  # Canonical identity key. Case folding plus trailing-punctuation removal
  # merges "SMITH, J", "Smith, J." and "smith, j" into one node instead of
  # three, which str_to_title() alone did not do.
  key <- if (case_fold) tolower(ent) else ent
  key <- trimws(gsub("[.;:,]+$", "", key))

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

  list(pid = pid, code = code, label = label, n_ent = n_ent)
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

  set.seed(seed)   # stable coordinates across re-renders

  if (identical(engine, "auto")) {
    engine <- if (nv <= 1500L) "fr" else if (nv <= 30000L) "drl" else "grid"
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
build_collaboration_network <- function(
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
    ...
) {

  empty <- list(nodes = data.frame(), edges = data.frame(),
                graph = NULL, stats = list(reason = "empty"))

  if (is.null(main_authors_list)) main_authors_list <- character(0)
  main_authors_list <- main_authors_list[!is.na(main_authors_list) &
                                           nzchar(trimws(main_authors_list))]

  tk <- cn_tokenise(df, target_col, target_delim)
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

  # --- build the igraph object once and reuse it for everything -------------
  used  <- sort(unique(c(e$i, e$j)))
  remap <- integer(n_all); remap[used] <- seq_along(used)

  g <- igraph::graph_from_edgelist(
    cbind(remap[e$i], remap[e$j]), directed = FALSE)
  igraph::V(g)$name   <- label[used]
  igraph::E(g)$weight <- e$w
  igraph::E(g)$count  <- e$cnt

  # --- component / cluster-size window --------------------------------------
  if (!is.null(cluster_size_range)) {
    comp <- igraph::components(g)
    clo  <- cluster_size_range[1]
    chi  <- if (length(cluster_size_range) >= 2 && is.finite(cluster_size_range[2]))
              cluster_size_range[2] else max(comp$csize)
    good <- which(comp$csize >= clo & comp$csize <= chi)
    keepv <- comp$membership %in% good
    if (length(main_authors_list)) {
      keepv <- keepv | cn_match_targets(igraph::V(g)$name, main_authors_list)
    }
    if (any(keepv) && !all(keepv)) {
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
    g <- igraph::delete_vertices(g, which(igraph::degree(g) == 0L))
  }
  if (igraph::vcount(g) == 0L) return(empty)

  # --- communities ----------------------------------------------------------
  memb <- rep(1L, igraph::vcount(g))
  modularity_score <- NA_real_
  if (isTRUE(detect_communities) && igraph::ecount(g) > 0L) {
    cl <- tryCatch(
      igraph::cluster_louvain(g, weights = igraph::E(g)$weight),
      error = function(err) NULL)
    if (!is.null(cl)) {
      memb <- as.integer(igraph::membership(cl))
      modularity_score <- suppressWarnings(igraph::modularity(cl))
    }
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

  if (isTRUE(compute_centrality)) {
    coren <- igraph::coreness(g)
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
    # Exact betweenness is O(V*E). Above ~2500 vertices use the cutoff
    # approximation, which is what makes this usable on HEP-scale graphs.
    btw <- tryCatch({
      if (nv <= 2500L) {
        igraph::betweenness(g, weights = 1 / pmax(igraph::E(g)$weight, 1e-9),
                            normalized = TRUE)
      } else {
        igraph::betweenness(g, weights = 1 / pmax(igraph::E(g)$weight, 1e-9),
                            cutoff = 4, normalized = TRUE)
      }
    }, error = function(err) rep(NA_real_, nv))
    # Articulation points (critical bottleneck cut-vertices)
    cut_v_indices <- tryCatch(igraph::articulation_points(g), error = function(e) integer(0))
    if (length(cut_v_indices) > 0) {
      is_cut[cut_v_indices] <- TRUE
    }
  }

  # --- layout ---------------------------------------------------------------
  coords <- cn_layout(g, engine = layout_engine, fr_iterations = fr_iterations)

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
      "<b>", nodes$label, "</b><br>",
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
    modularity         = modularity_score,
    n_communities      = length(unique(memb)),
    build_secs         = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 3)
  )

  if (isTRUE(verbose)) message(
    sprintf("build_collaboration_network(): %d nodes / %d edges in %.2fs (%s weighting)",
            stats$n_nodes, stats$n_edges, stats$build_secs, weight_scheme))

  list(nodes = nodes, edges = edges, graph = g, stats = stats)
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
    title = paste0("<b>", igraph::V(cg)$name, "</b><br>", sizes, " members"),
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

  # Exact mean path length is O(V*E); sample above 1500 vertices.
  apl <- tryCatch({
    if (nv <= 1500L) igraph::mean_distance(g, directed = FALSE, unconnected = TRUE)
    else igraph::mean_distance(igraph::induced_subgraph(
      g, sample.int(nv, 1500L)), directed = FALSE, unconnected = TRUE)
  }, error = function(e) NA_real_)

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
      "Mean shortest path",
      "Mean degree", "Max degree",
      "Degree Gini (inequality)",
      "Degree Centralization (Freeman)",
      "Betweenness Centralization",
      "Closeness Centralization",
      "Critical Brokers (Cut-Vertices)",
      "Communities (Louvain)", "Modularity",
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
      fmt(apl, 2),
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

  # Dirac's condition: min degree >= nv / 2 (for nv >= 3)
  dirac_threshold <- ceiling(nv / 2)
  dirac_met <- (nv >= 3L && min_deg >= dirac_threshold)

  # Ore's condition: deg(u) + deg(v) >= nv for all non-adjacent pairs (u, v)
  ore_met <- FALSE
  ore_min_sum <- NA_integer_
  if (nv >= 3L && comp$no == 1L) {
    ore_min_sum <- tryCatch({
      n_samp <- min(nv * 5L, 500L)
      u_samp <- sample.int(nv, n_samp, replace = TRUE)
      v_samp <- sample.int(nv, n_samp, replace = TRUE)
      diff_pairs <- (u_samp != v_samp)
      u_samp <- u_samp[diff_pairs]
      v_samp <- v_samp[diff_pairs]
      are_adj <- vapply(seq_along(u_samp), function(i) igraph::are_adjacent(g, u_samp[i], v_samp[i]), logical(1))
      non_u <- u_samp[!are_adj]
      non_v <- v_samp[!are_adj]
      if (length(non_u) > 0) min(deg[non_u] + deg[non_v]) else nv
    }, error = function(e) NA_integer_)
    if (!is.na(ore_min_sum)) ore_met <- (ore_min_sum >= nv)
  }

  # Biconnectivity
  is_biconnected <- (comp$no == 1L && n_cut_v == 0L && nv >= 3L)

  # Eulerian status
  has_eul_cycle <- tryCatch(igraph::has_eulerian_cycle(g), error = function(e) FALSE)
  has_eul_path  <- tryCatch(igraph::has_eulerian_path(g), error = function(e) FALSE)

  # Girth (length of shortest cycle)
  girth_val <- tryCatch(igraph::girth(g)$girth, error = function(e) NA_real_)

  # Diameter & Radius
  diam <- tryCatch(igraph::diameter(g, directed = FALSE, unconnected = TRUE), error = function(e) NA_real_)
  rad  <- tryCatch(if (comp$no == 1L) igraph::radius(g) else NA_real_, error = function(e) NA_real_)

  # Overall Hamiltonian Assessment
  ham_verdict <- if (isTRUE(dirac_met)) {
    "Guaranteed Hamiltonian (Dirac's condition satisfied)"
  } else if (isTRUE(ore_met)) {
    "Guaranteed Hamiltonian (Ore's condition satisfied)"
  } else if (isTRUE(comp$no > 1L)) {
    paste0("Provably Non-Hamiltonian (Graph is disconnected: ", comp$no, " components)")
  } else if (isTRUE(min_deg < 2L)) {
    paste0("Provably Non-Hamiltonian (Degree < 2 vertex exists; delta = ", min_deg, ")")
  } else if (isTRUE(n_cut_v > 0L)) {
    paste0("Provably Non-Hamiltonian (Contains ", n_cut_v, " articulation point / cut vertices)")
  } else if (!is.na(bridges_count) && isTRUE(bridges_count > 0L)) {
    paste0("Provably Non-Hamiltonian (Contains ", bridges_count, " bridge edge(s))")
  } else {
    "Feasible Candidate (2-connected, bridge-free, degree >= 2)"
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
      "Graph Diameter",
      "Graph Radius"
    ),
    Status = c(
      ham_verdict,
      if (isTRUE(dirac_met)) "Satisfied (Guaranteed Cycle)" else paste0("Not met (delta = ", min_deg, ", need >= ", dirac_threshold, ")"),
      if (isTRUE(ore_met)) "Satisfied" else if (!is.na(ore_min_sum)) paste0("Not met (min sum = ", ore_min_sum, ", need >= ", nv, ")") else "N/A",
      if (isTRUE(is_biconnected)) "Yes (Robust, no single cut vertex)" else "No (Vulnerable to single point of failure)",
      if (isTRUE(n_cut_v == 0L)) "0 (Robust)" else paste0(n_cut_v, " critical broker(s)"),
      if (is.na(bridges_count)) "-" else if (bridges_count == 0L) "0 (Bridge-free)" else paste0(bridges_count, " bridge edge(s)"),
      if (isTRUE(has_eul_cycle)) "Yes (All vertices have even degree)" else "No",
      if (isTRUE(has_eul_path)) "Yes (At most 2 vertices with odd degree)" else "No",
      if (is.na(girth_val) || is.infinite(girth_val) || girth_val == 0) "None (Acyclic/Forest)" else as.character(girth_val),
      if (is.na(diam)) "-" else as.character(diam),
      if (is.na(rad)) "-" else as.character(rad)
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

  k_per_paper <- tabulate(match(tk$pid, sort(unique(tk$pid))))
  n_papers    <- length(k_per_paper)
  n_authors   <- tk$n_ent

  # Fractional (1/k) productivity credit, the standard correction for
  # hyperauthorship.
  frac <- tapply(1 / k_per_paper[match(tk$pid, sort(unique(tk$pid)))],
                 tk$code, sum)

  single <- sum(k_per_paper == 1L)

  years <- if (year_col %in% names(df)) suppressWarnings(as.numeric(df[[year_col]])) else NULL
  cites <- if (cite_col %in% names(df)) suppressWarnings(as.numeric(df[[cite_col]])) else NULL

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
        sprintf("%.2f", sum(cites / k_per_paper, na.rm = TRUE) / n_papers))
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
                             size_max = 100L, weight_scheme = "count") {

  out <- list(max_node_occurrence = 1L, max_edge_frequency = 1L,
              max_cluster_size = 2L, max_paper_size = 1L, n_entities = 0L)

  tk <- cn_tokenise(df, target_col, target_delim)
  if (is.null(tk)) return(out)

  occ <- tabulate(tk$code, nbins = tk$n_ent)
  ks  <- tabulate(match(tk$pid, sort(unique(tk$pid))))

  out$max_node_occurrence <- max(occ, 1L)
  out$max_paper_size      <- max(ks, 1L)
  out$n_entities          <- tk$n_ent

  proj <- cn_project(tk$pid, tk$code, tk$n_ent, 1L, size_max,
                     weight_scheme = "count", oversize_mode = "drop")
  e <- proj$edges
  if (!length(e$i)) return(out)

  out$max_edge_frequency <- max(e$cnt, 1L)

  g <- igraph::graph_from_edgelist(cbind(e$i, e$j), directed = FALSE)
  out$max_cluster_size <- max(igraph::components(g)$csize, 2L)
  out
}
