# CollabNET browser-native MCP dispatcher
# IMPORTANT: source this with local=TRUE inside server(), after
# net_data_filtered_debounced has been defined. It uses the live server
# function's lexical scope (rv, input, session, glens_year_filtered_rx, etc.).
# Read-only tools only; no arbitrary R evaluation, file writes, or UI mutation.

mcp_default <- function(value, fallback) {
  if (is.null(value) || length(value) == 0L || isTRUE(all(is.na(value)))) fallback else value
}

mcp_char_array <- function(value, key, max_items = 30L, max_chars = 500L) {
  if (is.null(value)) return(NULL)
  if (is.list(value)) value <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (!is.character(value)) value <- as.character(value)
  if (length(value) > max_items || anyNA(value)) {
    stop(sprintf("'%s' must contain at most %d non-missing strings.", key, max_items), call. = FALSE)
  }
  value <- trimws(value)
  if (any(!nzchar(value)) || any(nchar(value, type = "chars") > max_chars)) {
    stop(sprintf("'%s' contains an empty or overlong string.", key), call. = FALSE)
  }
  value
}

mcp_require_string <- function(args, key, allow_empty = FALSE, max_chars = 2000L) {
  value <- args[[key]]
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("'%s' must be a single string.", key), call. = FALSE)
  }
  value <- trimws(value)
  if (!allow_empty && !nzchar(value)) {
    stop(sprintf("'%s' must not be empty.", key), call. = FALSE)
  }
  if (nchar(value, type = "chars") > max_chars) {
    stop(sprintf("'%s' exceeds the %d-character limit.", key, max_chars), call. = FALSE)
  }
  value
}

mcp_int_arg <- function(args, key, default, minimum, maximum) {
  value <- args[[key]]
  if (is.null(value) || length(value) == 0L) return(as.integer(default))
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || !is.finite(value) || value != floor(value)) {
    stop(sprintf("'%s' must be an integer.", key), call. = FALSE)
  }
  if (value < minimum || value > maximum) {
    stop(sprintf("'%s' must be between %d and %d.", key, minimum, maximum), call. = FALSE)
  }
  as.integer(value)
}

mcp_scalar <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  if (inherits(x, c("POSIXct", "POSIXlt", "Date"))) {
    return(as.character(x[1L]))
  }
  if (is.factor(x)) x <- as.character(x)
  if (is.list(x)) {
    if (length(x) == 1L) return(mcp_scalar(x[[1L]]))
    return(lapply(x, mcp_scalar))
  }
  if (length(x) > 1L) return(lapply(seq_along(x), function(i) mcp_scalar(x[i])))
  if (is.atomic(x) && is.na(x)) return(NULL)
  if (is.numeric(x) && !is.finite(x)) return(NULL)
  if (is.atomic(x)) return(unname(x))
  as.character(x)
}

mcp_records <- function(df, max_rows = 100L, columns = NULL) {
  if (!is.data.frame(df)) return(list())
  if (!is.null(columns)) {
    columns <- intersect(as.character(columns), names(df))
    df <- df[, columns, drop = FALSE]
  }
  if (nrow(df) == 0L || ncol(df) == 0L) return(list())
  n <- min(nrow(df), as.integer(max_rows))
  lapply(seq_len(n), function(i) {
    row <- lapply(names(df), function(col) mcp_scalar(df[[col]][i]))
    names(row) <- names(df)
    row
  })
}

mcp_clean_df <- function(df, max_rows = 100L) {
  if (!is.data.frame(df)) return(list())
  mcp_records(df, max_rows = max_rows)
}

mcp_empty_df <- function(x) !is.data.frame(x) || nrow(x) == 0L || ncol(x) == 0L

mcp_get_table <- function(dataset = "year_filtered") {
  dataset <- as.character(dataset)[1L]
  if (!dataset %in% c("full", "extended", "year_filtered")) {
    stop("dataset must be one of: full, extended, year_filtered.", call. = FALSE)
  }
  cur_rv <- if (exists("rv", inherits = TRUE)) rv else glens_env$rv
  cur_rx <- if (exists("glens_year_filtered_rx", inherits = TRUE)) glens_year_filtered_rx else glens_env$glens_year_filtered_rx
  if (dataset == "full") {
    out <- tryCatch(shiny::isolate(cur_rv$glens_full_table), error = function(e) NULL)
  } else if (dataset == "extended") {
    out <- tryCatch(shiny::isolate(cur_rv$glens_etable_final), error = function(e) NULL)
  } else {
    # This calls the app's actual reactive filter pipeline; it does not edit it.
    out <- tryCatch(shiny::isolate(cur_rx()), error = function(e) NULL)
  }
  if (!is.data.frame(out)) data.frame() else out
}

mcp_get_active_net <- function() {
  # Reuse the active/debounced graph already used by the CollabNET UI.
  # In particular, do not call build_collaboration_network() here.
  cur_net_fn <- if (exists("net_data_filtered_debounced", inherits = TRUE)) net_data_filtered_debounced else glens_env$net_data_filtered_debounced
  tryCatch(shiny::isolate(cur_net_fn()), error = function(e) NULL)
}

mcp_default_column <- function(df, candidates) {
  found <- candidates[candidates %in% names(df)]
  if (length(found)) found[[1L]] else if (ncol(df)) names(df)[[1L]] else NULL
}

mcp_display_names <- function(columns) {
  if (!length(columns)) return(character())
  tryCatch({
    out <- cn_display_colnames(columns)
    if (length(out) == length(columns)) as.character(out) else columns
  }, error = function(e) columns)
}

mcp_search_text <- function(value) {
  if (is.null(value) || length(value) == 0L || all(is.na(value))) return("")
  tryCatch(paste(as.character(value), collapse = " "), error = function(e) "")
}

mcp_dataset_schema <- function() {
  cur_rv <- if (exists("rv", inherits = TRUE)) rv else glens_env$rv
  tables <- list(
    full = tryCatch(shiny::isolate(cur_rv$glens_full_table), error = function(e) data.frame()),
    extended = tryCatch(shiny::isolate(cur_rv$glens_etable_final), error = function(e) data.frame()),
    year_filtered = mcp_get_table("year_filtered")
  )
  core <- c("Title", "Authors", "Year", "doi", "DOI", "Citations", "User_Journal", "Source")
  schemas <- lapply(names(tables), function(table_name) {
    df <- tables[[table_name]]
    cols <- if (is.data.frame(df)) names(df) else character()
    types <- if (length(cols)) vapply(df, function(x) paste(class(x), collapse = "/"), character(1)) else character()
    display <- mcp_display_names(cols)
    list(
      dataset = table_name,
      rows = if (is.data.frame(df)) nrow(df) else 0L,
      column_count = length(cols),
      columns = lapply(seq_along(cols), function(i) {
        list(name = cols[[i]], display_name = display[[i]], type = types[[i]], is_common_bibliographic_field = cols[[i]] %in% core)
      })
    )
  })
  names(schemas) <- names(tables)
  list(datasets = schemas)
}

mcp_dataset_summary <- function(dataset) {
  df <- mcp_get_table(dataset)
  if (mcp_empty_df(df)) {
    return(list(dataset = dataset, loaded = FALSE, rows = 0L, columns = 0L,
                year_span = NULL, distinct_doi_count = 0L, distinct_author_count = NULL,
                missing_by_column = list()))
  }

  missing_counts <- vapply(df, function(x) {
    tryCatch({
      s <- as.character(x)
      sum(is.na(x) | is.na(s) | !nzchar(trimws(s)), na.rm = TRUE)
    }, error = function(e) sum(is.na(x), na.rm = TRUE))
  }, numeric(1))
  miss_ord <- order(missing_counts, decreasing = TRUE)
  miss_ord <- head(miss_ord, 10L)
  missing <- lapply(miss_ord, function(i) {
    list(column = names(missing_counts)[i], missing = as.integer(missing_counts[i]),
         percent = round(100 * missing_counts[i] / max(1L, nrow(df)), 2))
  })

  year_col <- intersect(c("Year", "year", "PY", "Publication_Year", "Publication Year"), names(df))
  year_span <- NULL
  if (length(year_col)) {
    y <- suppressWarnings(as.numeric(as.character(df[[year_col[[1L]]]])))
    y <- y[is.finite(y)]
    if (length(y)) year_span <- list(min = min(y), max = max(y), year_column = year_col[[1L]])
  }

  doi_col <- intersect(c("doi", "DOI", "Doi"), names(df))
  doi_count <- 0L
  if (length(doi_col)) {
    doi_values <- trimws(as.character(df[[doi_col[[1L]]]]))
    doi_values <- doi_values[!is.na(doi_values) & nzchar(doi_values)]
    if (exists("extract_pure_doi", mode = "function")) {
      doi_values <- vapply(doi_values, function(x) {
        tryCatch(extract_pure_doi(x), error = function(e) NA_character_)
      }, character(1))
    }
    doi_values <- unique(tolower(doi_values[!is.na(doi_values) & nzchar(doi_values)]))
    doi_count <- length(doi_values)
  }

  author_col <- intersect(c("Authors", "Author", "author", "Author(s)"), names(df))
  author_count <- NULL
  if (length(author_col)) {
    authors_raw <- as.character(df[[author_col[[1L]]]])
    authors_raw <- authors_raw[!is.na(authors_raw) & nzchar(trimws(authors_raw))]
    delim <- tryCatch(shiny::isolate(rv$detected_mv_cols[[author_col[[1L]]]]), error = function(e) NULL)
    if (is.null(delim) || !nzchar(trimws(delim))) delim <- ","
    author_tokens <- tryCatch(cn_split_multivalue(authors_raw, target_delim = delim), error = function(e) strsplit(authors_raw, delim, fixed = TRUE))
    author_tokens <- unique(tolower(trimws(unlist(author_tokens, use.names = FALSE))))
    author_tokens <- author_tokens[nzchar(author_tokens)]
    author_count <- length(author_tokens)
  }

  list(
    dataset = dataset, loaded = TRUE, rows = nrow(df), columns = ncol(df),
    year_span = year_span,
    distinct_doi_count = as.integer(doi_count),
    distinct_author_count = author_count,
    missing_by_column = missing
  )
}

mcp_dispatch_tool <- function(tool_name, args) {
  if (is.null(args) || !is.list(args)) args <- list()

  switch(tool_name,
    collabnet_get_status = {
      full <- tryCatch(shiny::isolate(rv$glens_full_table), error = function(e) data.frame())
      extended <- tryCatch(shiny::isolate(rv$glens_etable_final), error = function(e) data.frame())
      filtered <- mcp_get_table("year_filtered")
      net <- mcp_get_active_net()
      graph <- if (is.list(net)) net$graph else NULL
      has_graph <- !is.null(graph) && inherits(graph, "igraph") && igraph::vcount(graph) > 0L
      list(
        app = "CollabNET", mode = "browser_session_read_only",
        dataset_loaded = is.data.frame(full) && nrow(full) > 0L,
        full_rows = if (is.data.frame(full)) nrow(full) else 0L,
        extended_rows = if (is.data.frame(extended)) nrow(extended) else 0L,
        filtered_rows = if (is.data.frame(filtered)) nrow(filtered) else 0L,
        is_processing = isTRUE(tryCatch(shiny::isolate(rv$is_glens_exec), error = function(e) FALSE)),
        network_enabled = isTRUE(input$enable_network),
        network_ready = has_graph,
        network_nodes = if (has_graph) igraph::vcount(graph) else 0L,
        network_edges = if (has_graph) igraph::ecount(graph) else 0L,
        ai_read_access_enabled = isTRUE(input$collabnet_mcp_enabled),
        active_network_column = mcp_scalar(tryCatch(shiny::isolate(input$net_col_filtered), error = function(e) NULL)),
        scope = "This tab only; no data is uploaded by the dispatcher itself. Tool results are sent to the connected in-page host."
      )
    },

    collabnet_get_dataset_schema = mcp_dataset_schema(),

    collabnet_get_dataset_summary = {
      dataset <- as.character(mcp_default(args$dataset, "year_filtered"))[[1L]]
      mcp_dataset_summary(dataset)
    },

    collabnet_search_publications = {
      query <- tolower(mcp_require_string(args, "query", max_chars = 300L))
      dataset <- as.character(mcp_default(args$dataset, "year_filtered"))[[1L]]
      df <- mcp_get_table(dataset)
      if (mcp_empty_df(df)) stop("The requested dataset is not loaded or has no rows.", call. = FALSE)

      requested_fields <- mcp_char_array(args$fields, "fields", max_items = 30L, max_chars = 200L)
      if (!is.null(requested_fields)) {
        search_cols <- intersect(unique(requested_fields), names(df))
        if (!length(search_cols)) stop("None of the requested search fields exist in this dataset.", call. = FALSE)
      } else {
        preferred <- c("Title", "title", "Authors", "Author", "Year", "doi", "DOI", "User_Journal", "Journal", "Source", "Citations", "Abstract", "Keywords")
        search_cols <- intersect(preferred, names(df))
        if (!length(search_cols)) {
          chars <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
          search_cols <- head(chars, 12L)
        }
      }
      if (!length(search_cols)) stop("No searchable columns are available.", call. = FALSE)

      year_from <- args$year_from
      year_to <- args$year_to
      year_col <- intersect(c("Year", "year", "PY", "Publication_Year", "Publication Year"), names(df))
      keep <- rep(TRUE, nrow(df))
      if (!is.null(year_from) || !is.null(year_to)) {
        if (!length(year_col)) stop("A year filter was requested, but this table has no recognized year column.", call. = FALSE)
        y <- suppressWarnings(as.numeric(as.character(df[[year_col[[1L]]]])))
        if (!is.null(year_from)) {
          lo <- suppressWarnings(as.numeric(year_from))
          if (length(lo) != 1L || !is.finite(lo) || lo < 1000 || lo > 3000) stop("year_from must be a year from 1000 to 3000.", call. = FALSE)
          keep <- keep & !is.na(y) & y >= lo
        }
        if (!is.null(year_to)) {
          hi <- suppressWarnings(as.numeric(year_to))
          if (length(hi) != 1L || !is.finite(hi) || hi < 1000 || hi > 3000) stop("year_to must be a year from 1000 to 3000.", call. = FALSE)
          keep <- keep & !is.na(y) & y <= hi
        }
      }
      candidates <- which(keep)
      matched <- vapply(candidates, function(i) {
        any(vapply(search_cols, function(col) {
          grepl(query, tolower(mcp_search_text(df[[col]][i])), fixed = TRUE)
        }, logical(1)))
      }, logical(1))
      hits <- candidates[matched]
      total <- length(hits)
      offset <- mcp_int_arg(args, "offset", 0L, 0L, 100000L)
      limit <- mcp_int_arg(args, "limit", 20L, 1L, 50L)
      selected_idx <- if (offset >= total) integer() else hits[seq.int(offset + 1L, min(total, offset + limit))]
      output_cols <- if (!is.null(requested_fields)) intersect(requested_fields, names(df)) else {
        # Return a bounded, useful subset to avoid large AI context payloads.
        out <- intersect(c("Title", "title", "Authors", "Author", "Year", "doi", "DOI", "User_Journal", "Journal", "Source", "Citations", "Abstract", "Keywords"), names(df))
        if (!length(out)) out <- head(names(df), 12L)
        head(out, 15L)
      }
      list(dataset = dataset, query = query, searched_columns = search_cols,
           total_matches = total, offset = offset, limit = limit,
           returned = length(selected_idx), records = mcp_records(df[selected_idx, , drop = FALSE], max_rows = limit, columns = output_cols))
    },

    collabnet_get_bibliometrics = {
      dataset <- as.character(mcp_default(args$dataset, "year_filtered"))[[1L]]
      df <- mcp_get_table(dataset)
      if (mcp_empty_df(df)) stop("The requested dataset is not loaded or has no rows.", call. = FALSE)
      default_entity <- if ("Authors" %in% names(df)) "Authors" else {
        detected <- tryCatch(names(shiny::isolate(rv$detected_mv_cols)), error = function(e) character())
        detected <- intersect(detected, names(df))
        if (length(detected)) detected[[1L]] else names(df)[[1L]]
      }
      entity_col <- as.character(mcp_default(args$entity_column, default_entity))[[1L]]
      year_col <- as.character(mcp_default(args$year_column, "Year"))[[1L]]
      cite_col <- as.character(mcp_default(args$citation_column, "Citations"))[[1L]]
      if (!entity_col %in% names(df)) stop(sprintf("entity_column '%s' does not exist.", entity_col), call. = FALSE)
      delim <- tryCatch(shiny::isolate(rv$detected_mv_cols[[entity_col]]), error = function(e) NULL)
      if (is.null(delim) || !nzchar(trimws(delim))) delim <- ","
      metrics <- cn_bibliometrics(df, target_col = entity_col, target_delim = delim,
                                  year_col = year_col, cite_col = cite_col)
      list(dataset = dataset, entity_column = entity_col, delimiter = delim,
           metrics = mcp_clean_df(metrics, max_rows = 100L))
    },

    collabnet_get_network_status = {
      net <- mcp_get_active_net()
      graph <- if (is.list(net)) net$graph else NULL
      ready <- !is.null(graph) && inherits(graph, "igraph") && igraph::vcount(graph) > 0L
      stats <- if (is.list(net) && is.list(net$stats)) net$stats else list()
      stat_keys <- intersect(c("n_communities", "modularity", "n_backbone_removed", "n_oversize_papers", "n_capped", "n_node_capped", "weight_scheme", "oversize_mode", "target_col"), names(stats))
      stats_out <- stats[stat_keys]
      list(enabled = isTRUE(input$enable_network), ready = ready,
           nodes = if (ready) igraph::vcount(graph) else 0L,
           edges = if (ready) igraph::ecount(graph) else 0L,
           node_table_rows = if (is.data.frame(net$nodes)) nrow(net$nodes) else 0L,
           edge_table_rows = if (is.data.frame(net$edges)) nrow(net$edges) else 0L,
           stats = stats_out,
           network_column = mcp_scalar(tryCatch(shiny::isolate(input$net_col_filtered), error = function(e) NULL)),
           filters = list(node_frequency = mcp_scalar(tryCatch(shiny::isolate(input$node_freq_range), error = function(e) NULL)),
                          edge_frequency = mcp_scalar(tryCatch(shiny::isolate(input$edge_freq_range), error = function(e) NULL)),
                          publication_size = mcp_scalar(tryCatch(shiny::isolate(input$custom_max_keywords), error = function(e) NULL))))
    },

    collabnet_get_network_metrics = {
      net <- mcp_get_active_net()
      graph <- if (is.list(net)) net$graph else NULL
      if (is.null(graph) || !inherits(graph, "igraph") || igraph::vcount(graph) == 0L) stop("No active network. Enable Network and wait for the graph to finish building.", call. = FALSE)
      metrics <- cn_network_metrics(net)
      stats <- if (is.list(net$stats)) net$stats else list()
      stat_keys <- intersect(c("n_communities", "modularity", "n_backbone_removed", "n_oversize_papers", "n_capped", "n_node_capped", "weight_scheme", "oversize_mode", "target_col"), names(stats))
      list(metrics = mcp_clean_df(metrics, max_rows = 100L), nodes = igraph::vcount(graph), edges = igraph::ecount(graph), graph_stats = stats[stat_keys])
    },

    collabnet_get_centrality_leaderboard = {
      net <- mcp_get_active_net()
      graph <- if (is.list(net)) net$graph else NULL
      if (is.null(graph) || !inherits(graph, "igraph") || igraph::vcount(graph) == 0L) stop("No active network.", call. = FALSE)
      top_n <- mcp_int_arg(args, "top_n", 20L, 1L, 100L)
      out <- cn_centrality_leaderboard(net, top_n = top_n)
      list(top_n = top_n, rows = mcp_clean_df(out, max_rows = 100L))
    },

    collabnet_get_node_metrics = {
      node_id <- mcp_require_string(args, "node_id", max_chars = 500L)
      net <- mcp_get_active_net()
      graph <- if (is.list(net)) net$graph else NULL
      nd <- if (is.list(net)) net$nodes else NULL
      if (is.null(graph) || !inherits(graph, "igraph") || !is.data.frame(nd) || nrow(nd) == 0L) stop("No active graph nodes are available.", call. = FALSE)
      id_col <- if ("id" %in% names(nd)) "id" else if ("label" %in% names(nd)) "label" else NULL
      if (is.null(id_col)) stop("The active node table has no ID column.", call. = FALSE)
      hits <- which(as.character(nd[[id_col]]) == node_id)
      if (!length(hits)) hits <- which(tolower(as.character(nd[[id_col]])) == tolower(node_id))
      if (!length(hits)) stop(sprintf("Node '%s' was not found in the active graph.", node_id), call. = FALSE)
      if (length(hits) > 1L) stop(sprintf("Node ID '%s' is ambiguous; use an exact case-sensitive ID.", node_id), call. = FALSE)
      list(node = mcp_records(nd[hits, , drop = FALSE], max_rows = 1L)[[1L]])
    },

    collabnet_get_neighbors = {
      node_id <- mcp_require_string(args, "node_id", max_chars = 500L)
      hops <- mcp_int_arg(args, "hops", 1L, 1L, 2L)
      limit <- mcp_int_arg(args, "limit", 50L, 1L, 100L)
      min_weight <- args$min_weight
      if (!is.null(min_weight)) {
        min_weight <- suppressWarnings(as.numeric(min_weight))
        if (length(min_weight) != 1L || !is.finite(min_weight) || min_weight < 0) stop("min_weight must be a non-negative number.", call. = FALSE)
      }
      net <- mcp_get_active_net()
      graph <- if (is.list(net)) net$graph else NULL
      nd <- if (is.list(net)) net$nodes else NULL
      if (is.null(graph) || !inherits(graph, "igraph") || igraph::vcount(graph) == 0L) stop("No active graph.", call. = FALSE)
      vertex_names <- igraph::V(graph)$name
      start <- which(vertex_names == node_id)
      if (!length(start)) start <- which(tolower(vertex_names) == tolower(node_id))
      if (!length(start)) stop(sprintf("Node '%s' was not found in the active graph.", node_id), call. = FALSE)
      if (length(start) > 1L) stop("Node ID is ambiguous; provide an exact case-sensitive ID.", call. = FALSE)
      d <- as.numeric(igraph::distances(graph, v = igraph::V(graph)[start], to = igraph::V(graph), mode = "all", weights = NA))
      idx <- which(is.finite(d) & d >= 1 & d <= hops)
      # min_weight is defined only for direct edges to avoid implying that a
      # two-hop path has one meaningful edge weight.
      if (hops == 1L && !is.null(min_weight) && length(idx)) {
        edge_df <- igraph::as_data_frame(graph, what = "edges")
        weights <- if ("weight" %in% names(edge_df)) as.numeric(edge_df$weight) else rep(1, nrow(edge_df))
        keep <- vapply(idx, function(j) {
          e <- which((edge_df$from == vertex_names[start] & edge_df$to == vertex_names[j]) |
                     (edge_df$to == vertex_names[start] & edge_df$from == vertex_names[j]))
          length(e) > 0L && max(weights[e], na.rm = TRUE) >= min_weight
        }, logical(1))
        idx <- idx[keep]
      }
      idx <- idx[order(d[idx], vertex_names[idx])]
      if (length(idx) > limit) idx <- idx[seq_len(limit)]
      node_rows <- match(vertex_names[idx], as.character(nd$id %||% nd$label))
      rows <- lapply(seq_along(idx), function(k) {
        j <- idx[[k]]
        edge_weight <- NULL
        edge_count <- NULL
        if (d[j] == 1) {
          edge_df <- igraph::as_data_frame(graph, what = "edges")
          e <- which((edge_df$from == vertex_names[start] & edge_df$to == vertex_names[j]) |
                     (edge_df$to == vertex_names[start] & edge_df$from == vertex_names[j]))
          if (length(e)) {
            if ("weight" %in% names(edge_df)) edge_weight <- mcp_scalar(edge_df$weight[e[[1L]]])
            if ("count" %in% names(edge_df)) edge_count <- mcp_scalar(edge_df$count[e[[1L]]])
          }
        }
        data <- if (!is.na(node_rows[[k]]) && is.data.frame(nd)) mcp_records(nd[node_rows[[k]], , drop = FALSE], max_rows = 1L)[[1L]] else list(id = vertex_names[j])
        data$distance <- as.integer(d[j])
        data$edge_weight_to_target <- edge_weight
        data$edge_count_to_target <- edge_count
        data
      })
      list(target = vertex_names[start], hops = hops, returned = length(rows), neighbors = rows,
           note = if (hops == 2L && !is.null(min_weight)) "min_weight is only applied for hops=1." else NULL)
    },

    collabnet_get_communities = {
      min_size <- mcp_int_arg(args, "min_size", 2L, 1L, 100000L)
      limit <- mcp_int_arg(args, "limit", 50L, 1L, 100L)
      net <- mcp_get_active_net()
      graph <- if (is.list(net)) net$graph else NULL
      nd <- if (is.list(net)) net$nodes else NULL
      if (is.null(graph) || !inherits(graph, "igraph") || is.null(nd) || !is.data.frame(nd) || nrow(nd) == 0L) stop("No active graph nodes are available.", call. = FALSE)
      if (!"community" %in% names(nd)) stop("The active graph has no community labels; enable community detection and rebuild the graph.", call. = FALSE)
      if (!"id" %in% names(nd)) stop("The active graph node table has no id column.", call. = FALSE)
      nd <- nd[!is.na(nd$community), , drop = FALSE]
      groups <- split(nd, as.character(nd$community), drop = TRUE)
      summaries <- lapply(names(groups), function(community_id) {
        members <- groups[[community_id]]
        if (nrow(members) < min_size) return(NULL)
        if ("strength" %in% names(members)) members <- members[order(members$strength, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
        else members <- members[order(members$id), , drop = FALSE]
        list(community = community_id, size = nrow(members), representative = as.character(members$id[[1L]]),
             top_members = head(as.character(members$id), 8L),
             total_strength = if ("strength" %in% names(members)) sum(members$strength, na.rm = TRUE) else NULL)
      })
      summaries <- Filter(Negate(is.null), summaries)
      if (length(summaries)) summaries <- summaries[order(vapply(summaries, `[[`, numeric(1), "size"), decreasing = TRUE)]
      all_count <- length(summaries)
      list(total_communities_matching = all_count, min_size = min_size,
           returned = min(all_count, limit), communities = head(summaries, limit))
    },

    collabnet_get_ego_metrics = {
      targets <- mcp_char_array(args$targets, "targets", max_items = 20L, max_chars = 500L)
      if (is.null(targets) || !length(targets)) {
        stop("targets must be an array of 1–20 non-empty node names/IDs.", call. = FALSE)
      }
      net <- mcp_get_active_net()
      graph <- if (is.list(net)) net$graph else NULL
      if (is.null(graph) || !inherits(graph, "igraph") || igraph::vcount(graph) == 0L) stop("No active graph.", call. = FALSE)
      out <- cn_ego_metrics(net, targets)
      list(targets = targets, rows = mcp_clean_df(out, max_rows = 100L))
    },

    collabnet_get_graph_diagnostics = {
      net <- mcp_get_active_net()
      graph <- if (is.list(net)) net$graph else NULL
      if (is.null(graph) || !inherits(graph, "igraph") || igraph::vcount(graph) == 0L) stop("No active graph.", call. = FALSE)
      # The existing UI runs this calculation too. These extra caps protect a
      # browser WASM runtime from an accidental expensive request on huge graphs.
      if (igraph::vcount(graph) > 5000L || igraph::ecount(graph) > 25000L) {
        stop("Graph diagnostics are capped at 5,000 nodes / 25,000 edges in the browser MCP adapter. Narrow the graph filters and retry.", call. = FALSE)
      }
      out <- cn_hamiltonian_diagnostics(net)
      list(nodes = igraph::vcount(graph), edges = igraph::ecount(graph), diagnostics = mcp_clean_df(out, max_rows = 100L))
    },

    collabnet_classify_identifier = {
      raw_id <- mcp_require_string(args, "raw_id", max_chars = 2000L)
      if (!exists("detect_identifier_type", mode = "function")) stop("CollabNET identifier classifier is not loaded.", call. = FALSE)
      out <- detect_identifier_type(raw_id)
      mcp_scalar(out)
    },

    collabnet_normalize_doi = {
      raw_id <- mcp_require_string(args, "raw_id", max_chars = 2000L)
      if (!exists("extract_pure_doi", mode = "function")) stop("CollabNET DOI extractor is not loaded.", call. = FALSE)
      doi <- tryCatch(extract_pure_doi(raw_id), error = function(e) NA_character_)
      if (length(doi) != 1L || is.na(doi) || !nzchar(trimws(doi))) {
        list(input = raw_id, is_doi = FALSE, doi = NULL, normalized = NULL,
             message = "No DOI found. The original value was not modified.")
      } else {
        doi <- sub("[.,;]+$", "", trimws(doi))
        # Lowercase only the comparison key; retain source spelling as well.
        list(input = raw_id, is_doi = TRUE, doi = doi, normalized = tolower(doi),
             normalization = "Extracted with CollabNET's DOI helper; trimmed trailing punctuation and lowercased the comparison key.")
      }
    },

    stop(sprintf("Tool '%s' is not registered.", tool_name), call. = FALSE)
  )
}
