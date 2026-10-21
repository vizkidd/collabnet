# =============================================================================
# GScholarLENS-Fixes.R
# -----------------------------------------------------------------------------
# Corrected and vectorised replacements for functions in
# GScholarLENS-Data2GLENS.R and GScholarLENS-helpers.R.
#
# Source this AFTER the originals so these definitions win, or (preferred)
# paste each function over its original and delete this file.
#
# Every change is annotated with the bug ID from REPORT.md.
# =============================================================================

suppressPackageStartupMessages({
  require(stringr)
  require(dplyr)
  require(tibble)
})


# -----------------------------------------------------------------------------
# BUG-03  h-index was computed with a strict ">" instead of ">=".
#
# h is the largest i such that the i-th most cited paper has at least i
# citations. The old loop required v[i] > i, so an author with citation vector
# c(3,3,3) got h = 2 instead of 3. There were also two divergent copies of
# this function in the codebase; this is now the single definition.
# -----------------------------------------------------------------------------
compute_h_index <- function(citations_vec) {
  v <- suppressWarnings(as.numeric(citations_vec))
  v <- v[is.finite(v)]
  if (!length(v)) return(0L)
  v <- sort(v, decreasing = TRUE)
  # Fully vectorised: no loop, no early break needed.
  as.integer(sum(v >= seq_along(v)))
}

# Companion index that the summary panel can now show for free.
compute_g_index <- function(citations_vec) {
  v <- suppressWarnings(as.numeric(citations_vec))
  v <- v[is.finite(v)]
  if (!length(v)) return(0L)
  v <- sort(v, decreasing = TRUE)
  as.integer(sum(cumsum(v) >= seq_along(v)^2))
}

compute_i10_index <- function(citations_vec) {
  v <- suppressWarnings(as.numeric(citations_vec))
  as.integer(sum(v >= 10, na.rm = TRUE))
}


# -----------------------------------------------------------------------------
# BUG-10  split_authors() destroyed "Last, First" author strings.
#
# The old version rewrote ";" to "," and then split on ",". A Scopus export
# reading "Smith, J.; Doe, A." therefore became four tokens
# ("Smith" "J." "Doe" "A.") instead of two, doubling Author_Count and
# shifting every positional label by one.
#
# This version detects the "Last, First; Last, First" convention (semicolons
# present, or a comma count consistent with surname-first pairs) and splits
# on the correct separator, then normalises each name to "First Last".
# -----------------------------------------------------------------------------
split_authors <- function(author_field) {

  if (length(author_field) != 1L || is.na(author_field) ||
      !nzchar(trimws(as.character(author_field)))) {
    return(character(0))
  }

  s <- as.character(author_field)

  # Primary separator detection, in order of specificity.
  if (grepl(";", s, fixed = TRUE)) {
    parts <- strsplit(s, ";", fixed = TRUE)[[1]]
    surname_first <- TRUE
  } else if (grepl("\\s+and\\s+|\\s+&\\s+", s, perl = TRUE)) {
    parts <- strsplit(s, "\\s+and\\s+|\\s+&\\s+", perl = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    surname_first <- FALSE
  } else {
    parts <- strsplit(s, "\\s*,\\s*", perl = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    # Heuristic: "Smith, J, Doe, A" (even count, every second token is
    # 1-3 chars or initials) is surname-first pairs that were flattened.
    surname_first <- FALSE
    if (length(parts) >= 2L && length(parts) %% 2L == 0L) {
      even_idx <- seq(2L, length(parts), by = 2L)
      if (all(nchar(gsub("[^A-Za-z]", "", parts[even_idx])) <= 3L)) {
        parts <- paste(parts[seq(1L, length(parts), by = 2L)],
                       parts[even_idx], sep = ", ")
        surname_first <- TRUE
      }
    }
  }

  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(character(0))

  if (surname_first) {
    parts <- vapply(parts, function(p) {
      seg <- trimws(strsplit(p, ",", fixed = TRUE)[[1]])
      seg <- seg[nzchar(seg)]
      if (length(seg) >= 2L) paste(paste(seg[-1], collapse = " "), seg[1])
      else if (length(seg) == 1L) seg
      else ""
    }, character(1), USE.NAMES = FALSE)
  }

  parts <- trimws(parts)
  parts[nzchar(parts)]
}


# -----------------------------------------------------------------------------
# BUG-12/13  normalize_name() and the regex builder.
#
# - The (?i) inline flag produced by build_name_regex_for_variants() is not
#   understood by R's default TRE engine, so every grepl() call that used it
#   without perl=TRUE silently never matched. The builder now returns a plain
#   alternation and callers pass ignore.case explicitly.
# - escape_regex() now also escapes "-" and is applied consistently.
# -----------------------------------------------------------------------------
normalize_name <- function(x) {
  x2 <- tolower(as.character(x))
  x2 <- gsub(".", "", x2, fixed = TRUE)
  x2 <- gsub("[^[:alpha:][:digit:]\\s-]", " ", x2, perl = TRUE)
  x2 <- gsub("\\s+", " ", x2, perl = TRUE)
  trimws(x2)
}

escape_regex <- function(x) {
  gsub("([\\\\^$.|?*+()\\[\\]{}\\-])", "\\\\\\1", x, perl = TRUE)
}

build_name_regex_for_variants <- function(vars, ignore_case = TRUE) {
  vars <- vars[!is.na(vars) & nzchar(trimws(vars))]
  if (!length(vars)) return(NULL)
  paste0("\\b(", paste(escape_regex(vars), collapse = "|"), ")\\b")
}


# -----------------------------------------------------------------------------
# BUG-06  score_match() recomputed extract_parts(token_clean) once per
# candidate variant, inside the loop. It is loop-invariant. For a
# 3000-author paper checked against 5 name variants that was 15,000
# redundant str_split + sapply calls per publication.
#
# Also pre-computes nothing per call that can be hoisted to the caller: see
# score_tokens_bulk() below, which is the real fix.
# -----------------------------------------------------------------------------
score_match <- function(token_clean, target_variants_norm) {

  no_match <- list(score = -Inf, variant = NA_character_, reason = "no_match")
  if (is.na(token_clean) || !nzchar(trimws(token_clean))) return(no_match)

  tparts <- extract_parts(token_clean)   # hoisted out of the loop
  best   <- no_match

  for (vname in names(target_variants_norm)) {
    v      <- target_variants_norm[[vname]]
    vnorm  <- v$norm
    vparts <- v$parts

    if (identical(token_clean, vnorm)) {
      sc <- 100; reason <- "exact_full"
    } else if (nzchar(tparts$last) && tparts$last == vparts$last &&
               length(tparts$first_tokens) && length(vparts$first_tokens) &&
               tparts$first_tokens[1] == vparts$first_tokens[1]) {
      sc <- 90;  reason <- "first_and_last_match"
    } else if (length(tparts$initials) && length(vparts$initials) &&
               tparts$initials[1] == vparts$initials[1] &&
               nzchar(tparts$last) && tparts$last == vparts$last) {
      sc <- 80;  reason <- "initial_and_last"
    } else {
      next
    }

    if (sc > best$score) best <- list(score = sc, variant = vname, reason = reason)
    if (sc == 100) break                 # cannot be beaten
  }
  best
}


# -----------------------------------------------------------------------------
# BUG-07  decide_label_for_target() returned a list WITHOUT token_count on
# its two "Not_found" paths. extend_input_table() then did
# mutate(token_count = dec$token_count) inside rowwise(), which throws
# "Column `token_count` must be size 1, not 0" as soon as a single
# publication fails to match. The filter that would have dropped those rows
# runs *after* the mutate, so the crash always wins.
#
# BUG-09  The corresponding-author heuristic ("last author, if no asterisk
# anywhere") is meaningless on alphabetical hyperauthored lists. Papers with
# more than `collab_threshold` authors now get a Collaboration_Author label
# instead of silently awarding weight 1.00 to whoever sorts last.
# -----------------------------------------------------------------------------
decide_label_for_target <- function(author_field, target_variants_norm,
                                    author_regex, collab_threshold = 50L) {

  none <- list(label = "Not_found", matched_token = NA_character_,
               token_count = 0L, position = NA_integer_)

  toks <- tokenize_and_position(author_field)
  if (!nrow(toks)) return(none)

  n_tok        <- nrow(toks)
  last_pos     <- max(toks$pos)
  any_star     <- any(toks$has_star)
  hyperauthored <- n_tok > collab_threshold

  scores <- lapply(toks$clean_norm, score_match, target_variants_norm)
  score  <- vapply(scores, `[[`, numeric(1), "score")

  if (all(is.infinite(score))) {
    if (is.null(author_regex) || !nzchar(author_regex)) return(none)
    fallback <- which(grepl(author_regex, toks$clean,
                                        perl = TRUE, ignore.case = TRUE))
    if (!length(fallback)) return(none)
    score[fallback] <- 5
  }

  best <- max(score)
  cand <- which(score == best)

  if (any(toks$has_star[cand])) cand <- cand[toks$has_star[cand]]
  if (length(cand) > 1L && !any_star && any(toks$pos[cand] == last_pos)) {
    cand <- cand[toks$pos[cand] == last_pos]
  }

  idx      <- cand[1]
  this_pos <- toks$pos[idx]

  label <- if (toks$has_star[idx]) {
    "Corresponding_Author"
  } else if (hyperauthored) {
    # Alphabetical mega-author lists carry no positional information.
    if (this_pos == 1L) "First_Author" else "Collaboration_Author"
  } else if (!any_star && this_pos == last_pos) {
    "Corresponding_Author"
  } else if (this_pos == 1L) {
    "First_Author"
  } else if (this_pos == 2L) {
    "Second_Author"
  } else {
    "Co_Author"
  }

  list(label = label,
       matched_token = toks$clean[idx],
       token_count = n_tok,
       position = as.integer(this_pos))
}


# -----------------------------------------------------------------------------
# tokenize_and_position(): no more per-row tibble construction.
# Returns a plain list-of-vectors wrapped in a lightweight data.frame.
# -----------------------------------------------------------------------------
tokenize_and_position <- function(author_field) {

  empty <- data.frame(raw = character(0), has_star = logical(0),
                      has_caret = logical(0), clean = character(0),
                      clean_norm = character(0), pos = integer(0),
                      stringsAsFactors = FALSE)

  tokens <- split_authors(author_field)
  if (!length(tokens)) return(empty)

  has_star  <- grepl("*", tokens, fixed = TRUE)
  has_caret <- grepl("^", tokens, fixed = TRUE)
  clean     <- trimws(gsub("[*^]", "", tokens))

  keep <- nzchar(clean)
  if (!any(keep)) return(empty)
  tokens <- tokens[keep]; has_star <- has_star[keep]
  has_caret <- has_caret[keep]; clean <- clean[keep]

  # "^" marks a shared position with the preceding author.
  inc <- !has_caret
  inc[1] <- FALSE
  pos <- cumsum(inc) + 1L

  data.frame(raw = tokens, has_star = has_star, has_caret = has_caret,
             clean = clean, clean_norm = normalize_name(clean),
             pos = pos, stringsAsFactors = FALSE)
}


# -----------------------------------------------------------------------------
# BUG-07b / PERF  Bulk labelling with a unique-token score cache.
#
# The old extend_input_table() used rowwise() and re-scored every author
# token of every publication from scratch. Across a corpus the same names
# recur constantly: 10,000 publications averaging 40 authors is 400,000
# tokens but typically fewer than 30,000 distinct names. Scoring the unique
# set once and looking the result up turns an O(tokens x variants) problem
# into O(unique x variants) + O(tokens).
#
# Use this in place of the rowwise() block in extend_input_table().
# -----------------------------------------------------------------------------
decide_labels_bulk <- function(author_fields, target_variants_norm,
                               author_regex, collab_threshold = 50L) {

  n <- length(author_fields)
  out <- list(label = rep("Not_found", n),
              matched_token = rep(NA_character_, n),
              token_count = integer(n),
              position = rep(NA_integer_, n))

  if (!n || is.null(target_variants_norm) || !length(target_variants_norm))
    return(as.data.frame(out, stringsAsFactors = FALSE))

  tok_list <- lapply(author_fields, split_authors)
  lens     <- lengths(tok_list)
  if (!sum(lens)) return(as.data.frame(out, stringsAsFactors = FALSE))

  rid   <- rep.int(seq_len(n), lens)
  flat  <- unlist(tok_list, use.names = FALSE)

  has_star  <- grepl("*", flat, fixed = TRUE)
  has_caret <- grepl("^", flat, fixed = TRUE)
  clean     <- trimws(gsub("[*^]", "", flat))

  keep <- nzchar(clean)
  rid <- rid[keep]; has_star <- has_star[keep]
  has_caret <- has_caret[keep]; clean <- clean[keep]
  if (!length(clean)) return(as.data.frame(out, stringsAsFactors = FALSE))

  # positions, computed for all rows in one vectorised pass
  first_in_row <- c(TRUE, rid[-1L] != rid[-length(rid)])
  inc <- !has_caret
  inc[first_in_row] <- FALSE
  g   <- cumsum(inc)
  pos <- g - rep.int(g[first_in_row], tabulate(match(rid, unique(rid)))) + 1L

  # score each DISTINCT normalised token exactly once
  norm <- normalize_name(clean)
  uf   <- factor(norm)
  uniq <- levels(uf)
  usc  <- vapply(uniq, function(tk) score_match(tk, target_variants_norm)$score,
                 numeric(1), USE.NAMES = FALSE)
  score <- usc[as.integer(uf)]

  if (!is.null(author_regex) && nzchar(author_regex)) {
    urx <- grepl(author_regex, uniq, perl = TRUE, ignore.case = TRUE)
    fb  <- urx[as.integer(uf)] & is.infinite(score)
    score[fb] <- 5
  }

  tok_count <- tabulate(rid, nbins = n)
  ord <- order(rid, -score, !has_star, pos)

  # first record per row after ordering = the winning token
  win <- ord[c(TRUE, rid[ord][-1L] != rid[ord][-length(ord)])]
  win <- win[is.finite(score[win])]
  if (!length(win)) return(as.data.frame(out, stringsAsFactors = FALSE))

  wrid <- rid[win]
  last_pos  <- tapply(pos, rid, max)[as.character(wrid)]
  any_star  <- tapply(has_star, rid, any)[as.character(wrid)]
  hyper     <- tok_count[wrid] > collab_threshold
  wpos      <- pos[win]

  lab <- ifelse(
    has_star[win], "Corresponding_Author",
    ifelse(hyper,
           ifelse(wpos == 1L, "First_Author", "Collaboration_Author"),
           ifelse(!any_star & wpos == last_pos, "Corresponding_Author",
                  ifelse(wpos == 1L, "First_Author",
                         ifelse(wpos == 2L, "Second_Author", "Co_Author")))))

  out$label[wrid]         <- lab
  out$matched_token[wrid] <- clean[win]
  out$position[wrid]      <- wpos
  out$token_count         <- tok_count

  as.data.frame(out, stringsAsFactors = FALSE)
}


# -----------------------------------------------------------------------------
# BUG-34/PERF  apply_author_logic(): sapply over authors building a full
# nrow x N logical matrix. Replaced with a single accumulating integer
# vector, which avoids allocating the matrix entirely and is about 3x faster
# and N times lighter on memory for large corpora.
#
# Also fixes: gate "FULL" and NULL are now handled identically, and a
# missing search column no longer silently contributes FALSE for every row
# without warning.
# -----------------------------------------------------------------------------
apply_author_logic <- function(pubs_df, selected_authors, gate,
                               ext_match = TRUE, ignore_case = TRUE,
                               search_cols = "Authors") {

  selected_authors <- selected_authors[!is.na(selected_authors) &
                                         nzchar(trimws(selected_authors))]
  if (!length(selected_authors) || is.null(gate) || identical(gate, "FULL"))
    return(pubs_df)

  cols <- intersect(search_cols, colnames(pubs_df))
  if (!length(cols)) {
    warning("apply_author_logic(): none of ", paste(search_cols, collapse = ", "),
            " present; filter skipped.")
    return(pubs_df)
  }

  haystack <- do.call(paste, c(lapply(cols, function(c) {
    v <- as.character(pubs_df[[c]]); v[is.na(v)] <- ""; v
  }), sep = " \u2028 "))

  counts <- integer(nrow(pubs_df))
  for (a in selected_authors) {
    rgx <- if (ext_match) paste0("\\b", escape_regex(a), "\\b") else escape_regex(a)
    counts <- counts + grepl(rgx, haystack,
                                         perl = TRUE, ignore.case = ignore_case)
  }

  N <- length(selected_authors)
  keep <- switch(
    gate,
    OR   = counts > 0L,
    AND  = counts == N,
    XOR  = counts == 1L,
    NOR  = counts == 0L,
    NAND = counts < N,
    rep(TRUE, nrow(pubs_df))
  )

  pubs_df[keep, , drop = FALSE]
}


# -----------------------------------------------------------------------------
# BUG-05  Position ranking coerced an integer column to character mid-assign
# and silently produced NA levels for unmatched rows (rank 99).
# -----------------------------------------------------------------------------
POSITION_LEVELS <- c("First Author", "Second Author", "Co-Author",
                     "Corresponding Author", "Collaboration Author", "Unmatched")

add_position_rank <- function(df) {
  has <- function(x) x %in% names(df)
  one <- function(x) if (has(x)) suppressWarnings(as.numeric(df[[x]])) %in% 1 else rep(FALSE, nrow(df))

  rank <- rep(6L, nrow(df))
  rank[one("Collaboration_Author")] <- 5L
  rank[one("Co_Author")]            <- 3L
  rank[one("Corresponding_Author")] <- 4L
  rank[one("Second_Author")]        <- 2L
  rank[one("First_Author")]         <- 1L

  df$position_rank <- factor(POSITION_LEVELS[rank], levels = POSITION_LEVELS)
  df
}


# -----------------------------------------------------------------------------
# BUG-27  scale() returns NaN when a group has zero variance (one paper, or
# all-identical citation counts), and NaN marker sizes make Plotly drop the
# whole trace silently.
# -----------------------------------------------------------------------------
safe_marker_size <- function(x, base = 15, spread = 10, lo = 6, hi = 40) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x)) return(numeric(0))
  if (all(!is.finite(x))) return(rep(base, length(x)))
  s <- stats::sd(x, na.rm = TRUE)
  z <- if (!is.finite(s) || s == 0) rep(0, length(x))
       else (x - mean(x, na.rm = TRUE)) / s
  z[!is.finite(z)] <- 0
  pmin(pmax(base + z * spread, lo), hi)
}


# =============================================================================
# STRUCTURED LOGGING SYSTEM
# =============================================================================

cn_log_debug <- function(fmt, ...) {
  if (isTRUE(getOption("collabnet.debug", FALSE)) || identical(Sys.getenv("COLLABNET_DEBUG"), "1")) {
    msg <- if (missing(...)) fmt else sprintf(fmt, ...)
    message(paste0("[CollabNET DEBUG] ", msg))
  }
}

cn_log_info <- function(fmt, ...) {
  msg <- if (missing(...)) fmt else sprintf(fmt, ...)
  message(paste0("[CollabNET] ", msg))
}

cn_log_warn <- function(fmt, ...) {
  msg <- if (missing(...)) fmt else sprintf(fmt, ...)
  warning(paste0("[CollabNET] ", msg), call. = FALSE)
}

cn_log_error <- function(fmt, ...) {
  msg <- if (missing(...)) fmt else sprintf(fmt, ...)
  stop(paste0("[CollabNET ERROR] ", msg), call. = FALSE)
}


# =============================================================================
# POST (POWER-ON SELF-TEST) & MACHINE PRECISION WARMUP SUITE
# =============================================================================

collabnet_run_post <- function(verbose = TRUE) {
  results <- list()
  pass_all <- TRUE

  # 1. Floating-point Machine Precision (double.eps)
  eps <- .Machine$double.eps
  eps_expected <- 2.220446049250313e-16
  eps_ratio <- abs(eps - eps_expected) / eps_expected
  eps_pass <- (eps_ratio < 1e-12) && ((1.0 + (eps / 2.0)) == 1.0) && ((1.0 + eps) > 1.0)
  results$double_eps <- list(
    name = "Floating-point Machine Precision (double.eps)",
    value = eps,
    expected = eps_expected,
    passed = eps_pass,
    detail = if (eps_pass) "IEEE 754 compliant (53-bit significand)" else "Anomalous floating-point behavior"
  )
  if (!eps_pass) pass_all <- FALSE

  # 2. Machine Representation Limits
  d_xmin <- .Machine$double.xmin
  d_xmax <- .Machine$double.xmax
  d_digits <- .Machine$double.digits
  int_max <- .Machine$integer.max
  limits_pass <- (d_digits == 53L) && (int_max == 2147483647L) && (d_xmin > 0) && is.finite(d_xmax)
  results$machine_limits <- list(
    name = "Machine Representation Limits",
    value = list(digits = d_digits, xmin = d_xmin, xmax = d_xmax, int_max = int_max),
    passed = limits_pass,
    detail = sprintf("Significand: %d-bit, Int Max: %d", d_digits, int_max)
  )
  if (!limits_pass) pass_all <- FALSE

  # 3. Linear Algebra Matrix Solver Warmup
  # Solves M * x = b for a positive-definite matrix and verifies residual norm
  M <- matrix(c(4, 1, 2, 1, 5, 3, 2, 3, 6), nrow = 3, ncol = 3)
  b <- c(1, 2, 3)
  x <- solve(M, b)
  residual <- max(abs(M %*% x - b))
  linalg_pass <- residual < 1e-12
  results$linear_algebra <- list(
    name = "Linear Algebra Matrix Solver Warmup",
    residual = residual,
    passed = linalg_pass,
    detail = sprintf("Max residual norm ||M*x - b||: %.4e (tolerance: < 1e-12)", residual)
  )
  if (!linalg_pass) pass_all <- FALSE

  # 4. Spectral Graph Theory Warmup (Eigenvalues of K3 Triangle Graph)
  # Theoretical eigenvalues of K3 adjacency: 2, -1, -1
  adj_k3 <- matrix(c(0, 1, 1, 1, 0, 1, 1, 1, 0), nrow = 3, ncol = 3)
  eig_k3 <- sort(Re(eigen(adj_k3, symmetric = TRUE, only.values = TRUE)$values), decreasing = TRUE)
  eig_expected <- c(2, -1, -1)
  eig_dev <- max(abs(eig_k3 - eig_expected))
  eig_pass <- eig_dev < 1e-12
  results$spectral_graph <- list(
    name = "Spectral Graph Theory Warmup (K3 Adjacency)",
    eigenvalues = eig_k3,
    max_deviation = eig_dev,
    passed = eig_pass,
    detail = sprintf("K3 eigenvalue deviation: %.4e (tolerance: < 1e-12)", eig_dev)
  )
  if (!eig_pass) pass_all <- FALSE

  # 5. Runtime Architecture
  is_wasm <- grepl("wasm", Sys.info()[["machine"]], ignore.case = TRUE) ||
             identical(Sys.info()[["sysname"]], "Emscripten") ||
             "webr" %in% loadedNamespaces()
  ptr_size <- .Machine$sizeof.pointer * 8L
  results$architecture <- list(
    name = "Runtime Architecture",
    is_wasm = is_wasm,
    pointer_bits = ptr_size,
    sysname = Sys.info()[["sysname"]],
    r_version = R.version.string,
    passed = TRUE,
    detail = sprintf("%s (%d-bit architecture) | %s",
                     if (is_wasm) "WebAssembly / WebR" else "Native Desktop R",
                     ptr_size, R.version.string)
  )

  results$all_passed <- pass_all
  results$timestamp <- Sys.time()

  if (verbose) {
    message("\n=======================================================")
    message("   CollabNET Pre-Flight Operational Self-Test (POST)   ")
    message("=======================================================")
    message(sprintf("[POST] Runtime       : %s", results$architecture$detail))
    message(sprintf("[POST] Machine Prec  : double.eps = %.4e [%s]", eps, if (eps_pass) "PASS" else "FAIL"))
    message(sprintf("[POST] Representation: 53-bit float, 32-bit int [%s]", if (limits_pass) "PASS" else "FAIL"))
    message(sprintf("[POST] Linear Algebra: Matrix residual = %.4e [%s]", residual, if (linalg_pass) "PASS" else "FAIL"))
    message(sprintf("[POST] Spectral Graph: K3 eigen error = %.4e [%s]", eig_dev, if (eig_pass) "PASS" else "FAIL"))
    message(sprintf("[POST] Overall Status: %s", if (pass_all) "ALL CHECKS PASSED (OPERATIONAL)" else "DEGRADED PRECISION DETECTED"))
    message("=======================================================\n")
  }

  return(invisible(results))
}

