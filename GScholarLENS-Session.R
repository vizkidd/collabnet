# GScholarLENS-Session.R
# Security helpers (HTML/JS escaping, DOI link validation) and the
# CollabNET session file format (.cnet v2, gzipped JSON).

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Escaping helpers (P0-8a)
# ---------------------------------------------------------------------------

# Escape any user/API supplied string before it is concatenated into HTML.
cn_html_escape <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  htmltools::htmlEscape(x, attribute = TRUE)
}

# Append a line to the HTML log. `msg` is escaped; `color` is restricted to a
# safe CSS colour token. Returns the new log string.
cn_log_append <- function(log_text, ..., color = NULL) {
  msg <- cn_html_escape(paste(..., collapse = " "))
  if (!is.null(color) && grepl("^(#[0-9A-Fa-f]{3,8}|[a-z]+)$", color)) {
    msg <- paste0("<span style='color: ", color, ";'>", msg, "</span>")
  }
  if (is.null(log_text) || length(log_text) == 0 || is.na(log_text) || !nzchar(log_text)) return(msg)
  paste(log_text, msg, sep = "<br>")
}

cn_doi_pattern <- "^10\\.\\d{4,9}/\\S+$"

cn_is_valid_doi <- function(doi) {
  !is.null(doi) && length(doi) == 1 && !is.na(doi) && grepl(cn_doi_pattern, doi, perl = TRUE)
}

# Return a safe https://doi.org/ URL or NA when the DOI is not well formed.
cn_safe_doi_url <- function(doi) {
  doi <- trimws(as.character(doi))
  doi <- sub("^(https?://(dx\\.)?doi\\.org/|doi:)", "", doi, ignore.case = TRUE)
  vapply(doi, function(d) {
    if (!cn_is_valid_doi(d)) return(NA_character_)
    paste0("https://doi.org/", gsub("%2F", "/", utils::URLencode(d, reserved = TRUE), fixed = TRUE))
  }, character(1), USE.NAMES = FALSE)
}

# Escaped <a> tag for a DOI, or escaped plain text if the DOI is invalid.
cn_doi_link_html <- function(doi) {
  url <- cn_safe_doi_url(doi)
  ifelse(is.na(url), cn_html_escape(doi),
         paste0("<a href=\"", cn_html_escape(url), "\" target=\"_blank\" rel=\"noopener noreferrer\">",
                cn_html_escape(doi), "</a>"))
}

# Serialise an R value to a JS literal (no string interpolation of raw data).
cn_js_value <- function(x) {
  if (is.null(x)) return("null")
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null"))
}

# Build `fn(arg1, arg2, ...)` with every argument JSON encoded.
cn_js_call <- function(fn, ...) {
  stopifnot(grepl("^[A-Za-z_$][A-Za-z0-9_$.]*$", fn))
  args <- vapply(list(...), cn_js_value, character(1))
  paste0(fn, "(", paste(args, collapse = ", "), ");")
}

# JS for window.fetchAllCitationCounts with JSON-encoded arguments (B-1).
cn_build_citation_js <- function(doi, semanticscholar_key = NULL, try_sources = character(0),
                                 input_id = "api_counts_ready", request_id = NULL) {
  cn_js_call("window.fetchAllCitationCounts",
             as.character(doi),
             I(as.character(try_sources)),
             list(semanticscholar = if (is.null(semanticscholar_key)) NULL else as.character(semanticscholar_key)),
             as.character(input_id),
             if (is.null(request_id)) NULL else as.character(request_id))
}

# ---------------------------------------------------------------------------
# .cnet v2 session files (P0-8d)
#
# Format: gzip-compressed UTF-8 JSON
# {
#   "format": "collabnet-session", "version": 2, "created_utc": "...Z",
#   "app": {"commit", "R", "igraph", "webR"},
#   "source": {...query metadata...}, "retrieved_utc": "...",
#   "settings": {"filters", "counting_mode", "weight_scheme", "oversize_mode",
#                "community": {"algorithm","seed","resolution"}, "layout": {...}},
#   "data": {"columns": [{"name","type"}], "rows": [[...], ...]}
# }
# Only atomic column types are allowed: logical, integer, double, character,
# Date (ISO-8601 string). Reading never evaluates or deserialises R objects.
# Legacy (<= v1) .cnet files were R serialisation (saveRDS) streams; they are
# REJECTED because readRDS() on untrusted input is a code-execution risk
# (cf. CVE-2024-27322) — re-export them as CSV from a trusted R session.
# ---------------------------------------------------------------------------

CNET_FORMAT <- "collabnet-session"
CNET_VERSION <- 2L
CNET_MAX_BYTES <- 50 * 1024^2     # uncompressed JSON
CNET_MAX_ROWS <- 1e6
CNET_MAX_COLS <- 500L
CNET_ALLOWED_TYPES <- c("logical", "integer", "double", "character", "Date")

cnet_error <- function(msg, class = "cnet_invalid") {
  stop(structure(class = c(class, "cnet_error", "error", "condition"),
                 list(message = msg, call = NULL)))
}

cnet_sanitize_colnames <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[[:cntrl:]]", "", x)
  x <- gsub("[<>\"'`]", "", x)
  x <- trimws(substr(x, 1, 256))
  x[!nzchar(x)] <- paste0("V", which(!nzchar(x)))
  make.unique(x, sep = "_")
}

cnet_col_type <- function(v) {
  if (inherits(v, "Date")) return("Date")
  if (is.factor(v)) return("character")
  if (is.logical(v)) return("logical")
  if (is.integer(v)) return("integer")
  if (is.double(v)) return("double")
  if (is.character(v)) return("character")
  NA_character_
}

cnet_app_info <- function() {
  commit <- tryCatch({
    if (exists("collabnet_commit", inherits = TRUE)) get("collabnet_commit", inherits = TRUE)
    else { cm <- suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)); if (length(cm)) cm[1] else NA }
  }, error = function(e) NA)
  pkgs <- c("shiny", "igraph", "Matrix", "jsonlite", "dplyr", "visNetwork", "DT", "plotly")
  pv <- vapply(pkgs, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_), "")
  list(collabnet_version = if (exists("COLLABNET_VERSION", inherits = TRUE)) get("COLLABNET_VERSION", inherits = TRUE) else NA_character_,
       commit = as.character(commit %||% NA),
       build = if (exists("collabnet_build_info", inherits = TRUE)) get("collabnet_build_info", inherits = TRUE) else NULL,
       R = paste(R.version$major, R.version$minor, sep = "."),
       packages = as.list(pv[!is.na(pv)]),
       igraph = tryCatch(as.character(utils::packageVersion("igraph")), error = function(e) NA_character_),
       webR = if (nzchar(Sys.getenv("WEBR_VERSION"))) Sys.getenv("WEBR_VERSION") else
         if (exists("is_WASM", inherits = TRUE) && isTRUE(get("is_WASM", inherits = TRUE))) "unknown" else NULL)
}

# Build the v2 session list (exported for tests).
cnet_build <- function(state) {
  df <- state$data
  if (!is.data.frame(df)) cnet_error("state$data must be a data.frame")
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  cols <- vector("list", ncol(df))
  for (j in seq_along(df)) {
    v <- df[[j]]
    if (is.list(v)) {   # list columns: flatten to "; "-joined text
      v <- vapply(v, function(e) paste(as.character(unlist(e)), collapse = "; "), character(1))
    }
    tp <- cnet_col_type(v)
    if (is.na(tp)) { v <- as.character(v); tp <- "character" }
    if (tp == "Date") v <- format(v, "%Y-%m-%d")
    if (is.factor(v)) v <- as.character(v)
    df[[j]] <- v
    cols[[j]] <- list(name = names(df)[j], type = tp)
  }
  names(df) <- cnet_sanitize_colnames(names(df))
  for (j in seq_along(cols)) cols[[j]]$name <- names(df)[j]
  list(format = CNET_FORMAT, version = CNET_VERSION,
       created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
       app = cnet_app_info(),
       source = state$source %||% list(),
       dedup = state$dedup %||% NULL,
       retrieved_utc = state$retrieved_utc %||% NULL,
       settings = cnet_encode_inf(state$settings %||% list()),
       data = list(columns = cols, rows = df))
}

# JSON has no Inf: numeric settings containing +/-Inf (e.g. cluster_size_range = c(1, Inf)) are stored as strings
# "Inf"/"-Inf" and restored on read, so a replay uses exactly the saved filters.
cnet_encode_inf <- function(x) {
  if (is.list(x)) return(lapply(x, cnet_encode_inf))
  if (is.numeric(x) && any(is.infinite(x))) return(ifelse(is.infinite(x), ifelse(x > 0, "Inf", "-Inf"), format(x, digits = 17)))
  x
}
cnet_decode_inf <- function(x) {
  if (is.list(x)) return(lapply(x, cnet_decode_inf))
  if (is.character(x) && length(x) && any(x %in% c("Inf", "-Inf")) &&
      all(x %in% c("Inf", "-Inf") | !is.na(suppressWarnings(as.numeric(x))))) return(suppressWarnings(as.numeric(x)))
  x
}

cnet_write <- function(path, state) {
  s <- cnet_build(state)
  rows <- s$data$rows; s$data$rows <- NULL
  head_json <- jsonlite::toJSON(s, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  rows_json <- jsonlite::toJSON(rows, dataframe = "values", na = "null", digits = I(17), auto_unbox = FALSE)
  if (nrow(rows) == 0) rows_json <- "[]"
  # splice rows into data object
  # splice by position, NOT sub(): backslashes in the data (e.g. LaTeX titles) are escapes in a sub() replacement
  h <- as.character(head_json); stopifnot(endsWith(h, "}}"))
  txt <- paste0(substr(h, 1L, nchar(h) - 2L), ",\"rows\":", as.character(rows_json), "}}")
  con <- gzfile(path, "wb", compression = 9)
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(txt)), con)
  invisible(path)
}

cnet_read_raw <- function(path, max_bytes = CNET_MAX_BYTES) {
  con <- gzfile(path, "rb")
  on.exit(close(con))
  chunks <- list(); total <- 0
  repeat {
    b <- readBin(con, "raw", n = 1024^2)
    if (!length(b)) break
    total <- total + length(b)
    if (total > max_bytes) cnet_error(sprintf("Session file exceeds the %.0f MB uncompressed limit.", max_bytes / 1024^2), "cnet_oversize")
    chunks[[length(chunks) + 1]] <- b
  }
  do.call(c, chunks)
}

cnet_read <- function(path, max_bytes = CNET_MAX_BYTES, max_rows = CNET_MAX_ROWS, max_cols = CNET_MAX_COLS) {
  raw <- cnet_read_raw(path, max_bytes)
  if (length(raw) >= 2 && (identical(raw[1:2], charToRaw("X\n")) || identical(raw[1:2], charToRaw("A\n")) ||
                           identical(raw[1:2], charToRaw("B\n")) || (length(raw) >= 4 && identical(raw[1:4], charToRaw("RDX3"))))) {
    cnet_error("This is a legacy CollabNET session (.cnet v1 / R serialisation). It cannot be opened safely because loading R serialised objects can execute code. Re-export it with a current CollabNET version, or open it in a trusted R session and save the table as CSV.", "cnet_legacy_rds")
  }
  first <- raw[!raw %in% charToRaw(" \t\r\n")][1]
  if (is.na(first) || first != charToRaw("{")) cnet_error("Not a CollabNET v2 session file (expected JSON).")
  s <- tryCatch(jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE),
                error = function(e) cnet_error(paste("Malformed session JSON:", conditionMessage(e))))
  if (!is.list(s) || !identical(s$format, CNET_FORMAT)) cnet_error("Missing or wrong 'format' field (expected 'collabnet-session').")
  if (!is.numeric(s$version) || length(s$version) != 1 || s$version != CNET_VERSION)
    cnet_error(sprintf("Unsupported session version '%s' (this app reads version %d).", paste(s$version, collapse = ","), CNET_VERSION), "cnet_version")
  for (f in c("created_utc", "data")) if (is.null(s[[f]])) cnet_error(sprintf("Required field '%s' missing.", f))
  cols <- s$data$columns; rows <- s$data$rows
  if (!is.list(cols) || !length(cols)) cnet_error("data.columns missing or empty.")
  if (!is.list(rows)) cnet_error("data.rows missing.")
  if (length(cols) > max_cols) cnet_error(sprintf("Too many columns (%d > %d).", length(cols), max_cols), "cnet_oversize")
  if (length(rows) > max_rows) cnet_error(sprintf("Too many rows (%d > %d).", length(rows), max_rows), "cnet_oversize")
  nm <- vapply(cols, function(c) if (is.character(c$name) && length(c$name) == 1) c$name else NA_character_, character(1))
  tp <- vapply(cols, function(c) if (is.character(c$type) && length(c$type) == 1) c$type else NA_character_, character(1))
  if (anyNA(nm)) cnet_error("Every column needs a string 'name'.")
  bad <- is.na(tp) | !tp %in% CNET_ALLOWED_TYPES
  if (any(bad)) cnet_error(sprintf("Column '%s' has a non-allowed type '%s'.", nm[bad][1], tp[bad][1]), "cnet_type")
  nc <- length(cols)
  out <- vector("list", nc)
  for (j in seq_len(nc)) {
    vals <- lapply(rows, function(r) {
      if (!is.list(r) || length(r) != nc) cnet_error("Row length does not match column count.")
      x <- r[[j]]
      if (is.null(x)) return(NA)
      if (is.list(x) || length(x) != 1 || !is.atomic(x)) cnet_error(sprintf("Non-atomic value in column '%s'.", nm[j]), "cnet_type")
      x
    })
    v <- unlist(vals, use.names = FALSE)
    if (is.null(v)) v <- logical(0)
    out[[j]] <- switch(tp[j],
      logical   = { if (!is.logical(v)) cnet_error(sprintf("Column '%s' is not logical.", nm[j]), "cnet_type"); as.logical(v) },
      integer   = { if (!(is.numeric(v) || is.logical(v)) || any(v != round(v), na.rm = TRUE)) cnet_error(sprintf("Column '%s' is not integer.", nm[j]), "cnet_type"); as.integer(v) },
      double    = { if (!(is.numeric(v) || is.logical(v))) cnet_error(sprintf("Column '%s' is not numeric.", nm[j]), "cnet_type"); as.double(v) },
      character = { if (!(is.character(v) || all(is.na(v)))) cnet_error(sprintf("Column '%s' is not text.", nm[j]), "cnet_type"); as.character(v) },
      Date      = as.Date(as.character(v), format = "%Y-%m-%d"))
  }
  names(out) <- cnet_sanitize_colnames(nm)
  df <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  if (!length(rows)) df <- df[0, , drop = FALSE]
  settings <- if (is.null(s$settings) || !length(s$settings)) list() else
    cnet_decode_inf(jsonlite::fromJSON(jsonlite::toJSON(s$settings, auto_unbox = TRUE, null = "null", digits = NA), simplifyVector = TRUE))
  list(data = df, settings = settings, source = s$source %||% list(),
       app = s$app %||% list(), dedup = s$dedup %||% NULL, created_utc = s$created_utc, retrieved_utc = s$retrieved_utc,
       version = as.integer(s$version))
}

# Collect the analysis settings to persist in a .cnet v2 file from Shiny inputs.
cnet_settings_from_input <- function(input) {
  g <- function(id) tryCatch(input[[id]], error = function(e) NULL)
  list(
    filters = list(year_slider = g("year_slider"), selected_source = g("selected_source"),
                   author_list = g("author_list"), author_logic_gate = g("author_logic_gate"),
                   node_freq_range = g("node_freq_range"), edge_freq_range = g("edge_freq_range"),
                   cluster_size_range = g("cluster_size_range"), net_col_filtered = g("net_col_filtered"),
                   prune_leaves = g("prune_leaves"), use_backbone = g("use_backbone"), backbone_alpha = g("backbone_alpha")),
    counting_mode = g("counting_mode") %||% g("count_mode"),
    identity_policy = g("identity_policy"),
    authors_per_pub = g("custom_max_keywords"),
    drop_isolates = g("drop_isolates") %||% FALSE,
    betweenness = if (isTRUE(g("betweenness_approx"))) list(method = "sample", k = 200L) else list(method = "exact"),
    weight_scheme = g("weight_scheme"),
    # C1-4: Sh-index author-position weights + Newman toggle (replayed by C1-5 "Replay full session")
    sh = if (exists("sh_current_weights", mode = "function")) {   # D1-4: weights actually in use (never NA)
           cw <- sh_current_weights(g); list(weights = as.list(cw$weights), newman = cw$newman)
         } else list(weights = if (exists("SH_DEFAULT_WEIGHTS")) as.list(vapply(names(SH_DEFAULT_WEIGHTS), function(k)
                as.numeric(g(paste0("sh_w_", k)) %||% SH_DEFAULT_WEIGHTS[[k]])[1], numeric(1))) else NULL,
              newman = isTRUE(g("sh_newman"))),
    oversize_mode = g("oversize_mode"),
    community = list(detect = g("detect_communities"), algorithm = g("community_algorithm"),
                     seed = g("community_seed"), resolution = g("community_resolution")),
    layout = list(engine = g("layout_engine"), fr_iterations = g("fr_iterations"),
                  max_render_edges = g("max_render_edges"), graph_view = g("graph_view"),
                  node_size_metric = g("node_size_metric"), node_color_metric = g("node_color_metric"))
  )
}

# B-8: split the DOI textarea (a single string) into individual identifiers: newline / comma / semicolon / whitespace
# separated, trimmed, empty and duplicate entries removed (case-insensitive DOI comparison).
split_doi_textarea <- function(txt) {
  if (is.null(txt) || !length(txt)) return(character(0))
  x <- trimws(unlist(strsplit(paste(txt, collapse = "\n"), "[\\s,;]+", perl = TRUE)))
  x <- x[!is.na(x) & nzchar(x)]
  x[!duplicated(tolower(x))]
}

# P1-3/P1-5: CollabNET release identifier (also shown in the About dialog and in the methods text).
COLLABNET_VERSION <- "2.2.0-dev (Phase 6B)"

# -----------------------------------------------------------------------------
# P1-4: methods paragraph. Writes the exact analysis settings as prose for a paper's methods section.
# settings = cnet_settings_from_input(input); stats = net$stats; dedup = rv$dedup_report; source = query metadata.
# -----------------------------------------------------------------------------
cn_methods_text <- function(settings = list(), stats = list(), dedup = NULL, source = list(), retrieved_utc = NULL,
                            app = cnet_app_info()) {
  `%|%` <- function(a, b) if (is.null(a) || !length(a) || all(is.na(a))) b else a
  f <- settings$filters %|% list(); cm <- settings$community %|% list()
  rng <- function(x) if (is.null(x) || !length(x)) "none" else paste(format(x), collapse = "-")
  src_bits <- c(if (length(source$orcid) && nzchar(paste(source$orcid, collapse = ""))) paste0("ORCID iD(s) ", paste(source$orcid, collapse = ", ")),
                if (length(source$scopus_id) && nzchar(paste(source$scopus_id, collapse = ""))) paste0("Scopus author ID(s) ", paste(source$scopus_id, collapse = ", ")),
                if (!is.null(source$doi_count) && source$doi_count > 0) paste0(source$doi_count, " DOI(s)"),
                if (!is.null(source$query)) paste0("query \"", source$query, "\""))
  p <- c(
    sprintf("Publication records were collected with CollabNET %s%s (R %s, igraph %s%s)%s.",
            app$collabnet_version %|% "", if (!is.null(app$commit) && !is.na(app$commit)) paste0(", commit ", app$commit) else "",
            app$R %|% "?", app$igraph %|% "?", if (!is.null(app$webR)) paste0(", webR ", app$webR) else "",
            if (length(src_bits)) paste0(" from ", paste(src_bits, collapse = "; ")) else ""),
    if (!is.null(retrieved_utc)) sprintf("Data were retrieved on %s (UTC).", retrieved_utc),
    if (!is.null(dedup)) sprintf("Duplicate records were removed (%d of %d: %d identical rows, %d by normalised DOI, %d by normalised title and year).",
                                 dedup$n_in - dedup$n_out, dedup$n_in, dedup$exact_rows %|% 0, dedup$by_doi %|% 0, dedup$by_title_year %|% 0),
    sprintf("Author identities were resolved with the '%s' policy%s%s.", stats$identity_policy %|% settings$identity_policy %|% "normalised",
            switch(stats$identity_policy %|% settings$identity_policy %|% "normalised",
                   normalised = " (author IDs when aligned with the author list, otherwise names normalised for case, accents, punctuation and name order)",
                   initials = " (surname plus first initial; may merge homonyms)", exact = " (exact strings, case-folded)", ""),
            if (!is.null(stats$identity_merged_nodes)) sprintf("; %d node(s) merged %d raw name variants", stats$identity_merged_nodes, stats$identity_merged_raw_forms) else ""),
    sprintf("A co-authorship network was built with %s edge weights%s and %s handling of papers outside the author-count window %s; edge co-occurrence filter %s, node occurrence filter %s.",
            switch(stats$weight_scheme %|% settings$weight_scheme %|% "newman", newman = "Newman (2001) fractional (sum over papers of 1/(n-1))", count = "full-count",
                   jaccard = "Jaccard", stats$weight_scheme %|% "?"),
            "", stats$oversize_mode %|% settings$oversize_mode %|% "star", rng(settings$authors_per_pub), rng(f$edge_freq_range), rng(f$node_freq_range)),
    sprintf("Component-size filtering was %s; isolated authors (solo publications only) were %s.",
            if (isTRUE(stats$component_filter$active)) sprintf("applied for display (range %s, %d node(s) hidden)", rng(stats$component_filter$range), stats$component_filter$removed_nodes %|% 0) else "not applied (all components kept)",
            if (isTRUE(stats$drop_isolates %|% settings$drop_isolates)) "excluded" else "kept"),
    "Shortest-path metrics (betweenness, closeness, average path length, diameter) use distance = 1/weight; unweighted hop counts are reported alongside.",
    sprintf("Betweenness centrality was computed %s.", if (grepl("^approx", stats$betweenness_method %|% "")) paste0("approximately: ", stats$betweenness_method) else "exactly (Brandes algorithm)"),
    if (isTRUE(cm$detect %|% TRUE)) sprintf("Communities were detected with %s (modularity objective, resolution %s, random seed %s)%s.",
            stats$community_algorithm %|% cm$algorithm %|% "Leiden", format(stats$community_resolution %|% cm$resolution %|% 1),
            format(stats$seed %|% cm$seed %|% 42), if (!is.null(stats$modularity) && is.finite(stats$modularity)) sprintf("; modularity Q = %.3f", stats$modularity) else ""),
    if (!is.null(stats$n_nodes)) sprintf("The resulting network had %d nodes and %d edges.", stats$n_nodes, stats$n_edges %|% 0),
    if (length(f$year_slider)) sprintf("Publications from %s were included.", rng(f$year_slider)),
    if (exists("sh_weights_description", mode = "function"))
      sh_weights_description(unlist(settings$sh$weights) %||% SH_DEFAULT_WEIGHTS, isTRUE(settings$sh$newman)),
    "Journal bands are OpenAlex citedness bands (Q1-Q4 by fixed cut-offs on 2-year mean citedness), not JCR quartiles."
  )
  p <- p[!vapply(p, is.null, logical(1)) & nzchar(unlist(lapply(p, function(z) z %|% "")))]
  gsub("  +", " ", paste(unlist(p), collapse = " "))
}

# -----------------------------------------------------------------------------
# C1-5: .cnet load modes. "data" = data table + provenance only (current UI settings kept); "replay" = data +
# every saved setting pushed back into the UI controls. cnet_replay_plan() maps the saved settings
# (cnet_settings_from_input() layout) to input ids + widget types in dependency order; settings without a
# control are returned in $unapplied (never dropped silently).
# -----------------------------------------------------------------------------
CNET_CONTROL_MAP <- list(   # path in settings -> c(input id, widget type); order = application order
  list(c("filters", "net_col_filtered"), "net_col_filtered", "select"),
  list("identity_policy", "identity_policy", "select"),
  list("weight_scheme", "weight_scheme", "select"),
  list("oversize_mode", "oversize_mode", "select"),
  list("authors_per_pub", "custom_max_keywords", "slider"),
  list(c("filters", "selected_source"), "selected_source", "radio"),
  list(c("filters", "author_logic_gate"), "author_logic_gate", "radio"),
  list(c("filters", "year_slider"), "year_slider", "slider"),
  list(c("filters", "node_freq_range"), "node_freq_range", "slider"),
  list(c("filters", "edge_freq_range"), "edge_freq_range", "slider"),
  list(c("filters", "cluster_size_range"), "cluster_size_range", "slider"),
  list(c("filters", "prune_leaves"), "prune_leaves", "checkbox"),
  list(c("filters", "use_backbone"), "use_backbone", "checkbox"),
  list(c("filters", "backbone_alpha"), "backbone_alpha", "slider"),
  list(c("community", "detect"), "detect_communities", "checkbox"),
  list(c("community", "algorithm"), "community_algorithm", "select"),
  list(c("community", "seed"), "community_seed", "numeric"),
  list(c("community", "resolution"), "community_resolution", "numeric"),
  list(c("layout", "engine"), "layout_engine", "select"),
  list(c("layout", "fr_iterations"), "fr_iterations", "numeric"),
  list(c("layout", "max_render_edges"), "max_render_edges", "numeric"),
  list(c("layout", "graph_view"), "graph_view", "radio"),
  list(c("layout", "node_size_metric"), "node_size_metric", "select"),
  list(c("layout", "node_color_metric"), "node_color_metric", "select"),
  list(c("sh", "newman"), "sh_newman", "checkbox")
)

cnet_replay_plan <- function(settings, saved_app = list(), current_version = COLLABNET_VERSION) {
  get_path <- function(x, p) { for (k in p) { if (!is.list(x) || is.null(x[[k]])) return(NULL); x <- x[[k]] }; x }
  steps <- list(); used <- character(); unapplied <- character()
  for (m in CNET_CONTROL_MAP) {
    used <- c(used, paste(m[[1]], collapse = "."))
    v <- get_path(settings, m[[1]])
    if (is.null(v) || !length(v)) next
    steps[[length(steps) + 1L]] <- list(id = m[[2]], type = m[[3]], value = v, key = paste(m[[1]], collapse = "."))
  }
  bm <- get_path(settings, c("betweenness", "method"))
  if (!is.null(bm)) steps[[length(steps) + 1L]] <- list(id = "betweenness_approx", type = "checkbox", value = identical(unlist(bm)[1], "sample"), key = "betweenness")
  used <- c(used, "betweenness")
  # Sh-index weights (one numeric input per position)
  w <- get_path(settings, c("sh", "weights"))
  if (length(w)) for (k in names(w)) {
    key <- paste0("sh.weights.", k)
    if (exists("SH_DEFAULT_WEIGHTS") && k %in% names(SH_DEFAULT_WEIGHTS)) {
      steps[[length(steps) + 1L]] <- list(id = paste0("sh_w_", k), type = "numeric", value = unlist(w[[k]]), key = key)
    } else unapplied <- c(unapplied, paste0(key, " (unknown weight)"))
  }
  used <- c(used, "sh.weights")
  # Everything else that was saved but has no control: list it.
  flat <- function(x, pre = "") unlist(lapply(names(x), function(k) {
    p <- if (nzchar(pre)) paste0(pre, ".", k) else k
    if (is.list(x[[k]]) && !is.null(names(x[[k]])) && !p %in% used) flat(x[[k]], p) else p }))
  for (p in flat(settings)) if (!p %in% used && !any(startsWith(p, paste0(used, "."))) && !is.null(get_path(settings, strsplit(p, ".", fixed = TRUE)[[1]])))
    unapplied <- c(unapplied, paste0(p, " (no UI control)"))
  ver <- saved_app$collabnet_version %||% NULL
  list(steps = steps, unapplied = unapplied,
       version_note = if (!is.null(ver) && !identical(ver, current_version)) sprintf("saved with CollabNET %s, replayed in %s", ver, current_version) else NULL)
}

# Push a plan into the UI. Every target input is frozen first (freezeReactiveValue: readers see req()-failure until
# the browser echoes the new value), so downstream reactives rebuild once with the complete settings instead of
# after every single control update. Returns ids applied / failed.
cnet_apply_replay <- function(session, input, plan) {
  applied <- character(); failed <- character()
  for (s in plan$steps) tryCatch(shiny::freezeReactiveValue(input, s$id), error = function(e) NULL)
  for (s in plan$steps) {
    ok <- tryCatch({
      v <- s$value
      switch(s$type,
        slider   = shiny::updateSliderInput(session, s$id, value = as.numeric(unlist(v))),
        select   = shiny::updateSelectInput(session, s$id, selected = as.character(unlist(v))),
        radio    = shiny::updateRadioButtons(session, s$id, selected = as.character(unlist(v))[1]),
        checkbox = shiny::updateCheckboxInput(session, s$id, value = isTRUE(as.logical(unlist(v))[1])),
        numeric  = shiny::updateNumericInput(session, s$id, value = as.numeric(unlist(v))[1]))
      TRUE }, error = function(e) FALSE)
    if (isTRUE(ok)) applied <- c(applied, s$id) else failed <- c(failed, paste0(s$key, " (update failed)"))
  }
  list(applied = applied, failed = c(plan$unapplied, failed), version_note = plan$version_note)
}

cnet_replay_message <- function(res) {
  paste0("Session replayed: ", length(res$applied), " setting(s) applied to the controls",
         if (length(res$applied)) paste0(" (", paste(res$applied, collapse = ", "), ")") else "", ".",
         if (length(res$failed)) paste0(" Not applied: ", paste(res$failed, collapse = "; "), ".") else "",
         if (!is.null(res$version_note)) paste0(" Note: ", res$version_note, ".") else "")
}

# Server wiring: after a .cnet upload has been imported (rv$glens_full_table set), either keep the current settings
# ("data") or replay the saved ones ("replay"). input$cnet_load_mode comes from the import modal. delay_fn defers
# the replay until the import's own slider/bounds resets have reached the browser (shinyjs::delay in the app).
register_cnet_replay <- function(input, session, rv, delay_fn = function(ms, expr) expr, notify = shiny::showNotification) {
  shiny::observeEvent(rv$glens_full_table, {
    st <- shiny::isolate(rv$cnet_loaded_settings)
    if (is.null(st)) return()
    mode <- shiny::isolate(input$cnet_load_mode) %||% "data"
    app <- shiny::isolate(rv$cnet_loaded_app) %||% list()
    rv$cnet_loaded_settings <- NULL; rv$cnet_loaded_app <- NULL
    if (!identical(mode, "replay")) {
      rv$cnet_last_replay <- list(mode = "data")
      notify("Session data loaded; your current settings were kept (mode: Load data only).", type = "message", duration = 6)
      return()
    }
    plan <- cnet_replay_plan(st, app)
    delay_fn(1500, {
      res <- cnet_apply_replay(session, input, plan)
      rv$cnet_last_replay <- c(list(mode = "replay"), res)
      notify(cnet_replay_message(res), type = if (length(res$failed)) "warning" else "message", duration = 15)
    })
  }, priority = -10, ignoreInit = TRUE)
}
