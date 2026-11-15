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

