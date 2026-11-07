suppressPackageStartupMessages(require(httr))
suppressPackageStartupMessages(require(jsonlite))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(purrr))
suppressPackageStartupMessages(require(stringr))
if (!exists("is_WASM") || !isTRUE(is_WASM)) {
  suppressPackageStartupMessages(require(httr2, quietly = TRUE))
}

# SECURITY (P0-8c / audit D-2): the Elsevier API key is sent ONLY as the
# X-ELS-APIKey request header, never in the URL query string (URLs end up in
# logs, proxies, browser history and error messages).
scopus_auth_headers <- function(api_key) {
  c(`X-ELS-APIKey` = as.character(api_key), Accept = "application/json")
}
scopus_base_request <- function(url, api_key) {
  httr2::request(url) %>%
    httr2::req_headers(!!!as.list(scopus_auth_headers(api_key))) %>%
    httr2::req_url_query(httpAccept = "application/json")
}

# ---------------------------------------------------------------------------
# C2 (audit D-3 / D-4): resilient, injectable Scopus Search paging.
#   fetch_fn(url, query, api_key) -> list(status = int, headers = named list
#   (lower-case names), body = character(1)). status NA = transport error.
#   sleep_fn(seconds) is injectable so tests run with no network / no waiting.
# ---------------------------------------------------------------------------
SCOPUS_SEARCH_URL <- "https://api.elsevier.com/content/search/scopus"

scopus_http_get <- function(url, query, api_key) {
  local_is_WASM <- grepl(pattern = "wasm", x = Sys.info()["machine"])
  if (local_is_WASM || !requireNamespace("httr2", quietly = TRUE)) {
    req_url <- paste0(url, "?", paste(names(query), vapply(query, function(v) URLencode(as.character(v), reserved = TRUE), ""), sep = "=", collapse = "&"))
    con <- NULL
    out <- tryCatch({
      con <- url(req_url, headers = scopus_auth_headers(api_key))
      list(status = 200L, headers = list(), body = paste(readLines(con, warn = FALSE), collapse = "\n"))
    }, error = function(e) {
      m <- conditionMessage(e)
      code <- suppressWarnings(as.integer(regmatches(m, regexpr("[1-5][0-9][0-9]", m))))
      list(status = if (length(code)) code else NA_integer_, headers = list(), body = m)
    }, finally = { if (!is.null(con) && inherits(con, "connection")) try(close(con), silent = TRUE) })
    return(out)
  }
  tryCatch({
    req <- scopus_base_request(url, api_key) %>%   # key only in X-ELS-APIKey header (P0-8c)
      httr2::req_url_query(!!!query) %>%
      httr2::req_error(is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)
    h <- as.list(httr2::resp_headers(resp)); names(h) <- tolower(names(h))
    list(status = as.integer(httr2::resp_status(resp)), headers = h,
         body = tryCatch(httr2::resp_body_string(resp, encoding = "UTF-8"), error = function(e) ""))
  }, error = function(e) list(status = NA_integer_, headers = list(), body = conditionMessage(e)))
}

# Seconds to wait before retrying, from Retry-After (delta-seconds or HTTP-date)
# or X-RateLimit-Reset (Unix epoch seconds, per dev.elsevier.com/support.html).
# Returns NA when no usable header is present.
scopus_retry_wait <- function(headers, now = Sys.time()) {
  h <- headers; if (is.null(h)) return(NA_real_)
  names(h) <- tolower(names(h))
  ra <- h[["retry-after"]]
  if (!is.null(ra) && length(ra) && nzchar(ra <- trimws(as.character(ra[[1]])))) {
    if (grepl("^[0-9]+(\\.[0-9]+)?$", ra)) return(max(0, as.numeric(ra)))
    lc <- Sys.getlocale("LC_TIME"); on.exit(try(Sys.setlocale("LC_TIME", lc), silent = TRUE), add = TRUE)
    try(Sys.setlocale("LC_TIME", "C"), silent = TRUE)
    t <- as.POSIXct(ra, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
    if (!is.na(t)) return(max(0, as.numeric(difftime(t, now, units = "secs"))))
  }
  rs <- h[["x-ratelimit-reset"]]
  if (!is.null(rs) && length(rs)) {
    rs <- suppressWarnings(as.numeric(as.character(rs[[1]])))
    if (!is.na(rs)) return(max(0, rs - as.numeric(now)))
  }
  NA_real_
}

# One request with exponential backoff + jitter for 429 / 5xx / transport errors.
# Never stop()s: returns list(ok, status, body, attempts, waits, message).
scopus_request_with_retry <- function(query, api_key, fetch_fn = scopus_http_get, sleep_fn = Sys.sleep,
                                      max_attempts = 5, base_delay = 1, max_delay = 60, jitter = 0.25,
                                      url = SCOPUS_SEARCH_URL) {
  waits <- numeric(0); last <- NULL
  for (attempt in seq_len(max_attempts)) {
    last <- fetch_fn(url, query, api_key)
    st <- suppressWarnings(as.integer(last$status))
    if (!is.na(st) && st == 200L)
      return(list(ok = TRUE, status = 200L, body = last$body, attempts = attempt, waits = waits, message = "OK"))
    retryable <- is.na(st) || st == 429L || st >= 500L
    if (!retryable || attempt == max_attempts) break
    w <- scopus_retry_wait(last$headers)
    if (is.na(w)) w <- base_delay * 2^(attempt - 1) * (1 + stats::runif(1, 0, jitter))
    if (w > max_delay) break  # e.g. weekly QUOTA_EXCEEDED: do not block for hours/days
    waits <- c(waits, w)
    sleep_fn(w)
  }
  st <- suppressWarnings(as.integer(last$status))
  h <- last$headers; if (!is.null(h)) names(h) <- tolower(names(h))
  els <- if (!is.null(h[["x-els-status"]])) paste0(" [", h[["x-els-status"]], "]") else ""
  msg <- if (!is.na(st) && st == 401L) "Authorization Error (401): API key lacks permissions."
         else sprintf("API Error (%s)%s after %d attempt(s)", if (is.na(st)) "network" else st, els, length(waits) + 1L)
  list(ok = FALSE, status = st, body = last$body, attempts = length(waits) + 1L, waits = waits, message = msg)
}

# Pages one search query with CURSOR pagination (audit D-4): first request
# cursor=*, then the returned search-results$cursor$`@next`. Standard start=
# paging is capped at 5,000 items (dev.elsevier.com/api_key_settings.html), so
# `start` is never sent. Stops at opensearch:totalResults, on an empty page,
# when the cursor does not advance, or at max_results (quota guard).
# Returns list(entries = list of data.frames, status, message, complete, total).
# On a final HTTP failure the pages already fetched are KEPT (audit D-3).
scopus_paged_search <- function(search_query, api_key, fetch_fn = scopus_http_get, sleep_fn = Sys.sleep,
                                cancel_fn = function() FALSE, count = 25, max_results = 5000,
                                max_pages = 2000, ...) {
  entries_out <- list(); n <- 0; cursor <- "*"; seen <- character(0); total <- NA_real_
  done <- function(status, message, complete) list(entries = entries_out, status = status, message = message,
                                                   complete = complete, total = total)
  if (max_results <= 0) return(done("capped", "max_results reached", FALSE))
  for (page in seq_len(max_pages)) {
    if (isTRUE(cancel_fn())) return(done("cancelled", "Cancelled by user.", FALSE))
    r <- scopus_request_with_retry(list(httpAccept = "application/json", query = search_query,
                                        count = min(count, max_results - n), cursor = cursor, view = "COMPLETE"),
                                   api_key, fetch_fn = fetch_fn, sleep_fn = sleep_fn, ...)
    if (!r$ok) return(done(if (identical(r$status, 401L)) "unauthorized" else "partial", r$message, FALSE))
    sr <- tryCatch(jsonlite::fromJSON(r$body, flatten = TRUE)$`search-results`, error = function(e) NULL)
    if (is.null(sr)) return(done("partial", "Unparseable Scopus response", FALSE))
    if (is.na(total)) total <- suppressWarnings(as.numeric(sr$`opensearch:totalResults`))
    entries <- sr$entry
    # an empty result set comes back as a single entry carrying only an "error" field
    if (is.data.frame(entries) && "error" %in% names(entries) && !("dc:title" %in% names(entries)) && !("dc:identifier" %in% names(entries))) entries <- NULL
    if (is.null(entries) || NROW(entries) < 1) break
    if (n + NROW(entries) > max_results) entries <- entries[seq_len(max(0, max_results - n)), , drop = FALSE]
    entries_out <- append(entries_out, list(entries)); n <- n + NROW(entries)
    if (!is.na(total) && n >= total) break
    if (n >= max_results) return(done("capped", sprintf("Stopped at max_results = %d of %s", as.integer(max_results), total), FALSE))
    nxt <- sr$cursor$`@next`
    if (is.null(nxt) || !nzchar(nxt) || nxt %in% c(seen, cursor)) break   # cursor missing or not advancing
    seen <- c(seen, cursor); cursor <- nxt
  }
  done("ok", "OK", TRUE)
}

get_scopus_data_orcid <- function(orcid, rv = NULL, api_key = NULL, fetch_fn = scopus_http_get, sleep_fn = Sys.sleep,
                                  cancel_fn = function() !fs::file_exists(file.path("run.lock")),
                                  max_results = getOption("collabnet.scopus_max_results", 20000)) {
  if(is.null(api_key) || trimws(api_key) == ""){ stop("SCOPUS API Key is missing.") }
  api_key <- gsub("[^[:alnum:]]", "", api_key)
  
  # Process and Clean ORCIDs
  clean_orcids <- trimws(orcid)
  clean_orcids <- clean_orcids[clean_orcids != ""] 
  if(length(clean_orcids) == 0) return(tibble::tibble())
  
  # --- NEW: CHUNKING LOGIC ---
  chunk_size <- 30
  id_chunks <- split(clean_orcids, ceiling(seq_along(clean_orcids) / chunk_size))
  
  n_fetched <- 0; fetch_status <- "ok"; fetch_message <- "OK"
  
  all_entries <- list()
  
  for (chunk in id_chunks) {
    search_query <- paste0("ORCID(", chunk, ")", collapse = " OR ")
    
    res <- scopus_paged_search(search_query, api_key, fetch_fn = fetch_fn, sleep_fn = sleep_fn,
                               cancel_fn = cancel_fn, max_results = max(0, max_results - n_fetched))
    all_entries <- append(all_entries, res$entries)
    n_fetched <- n_fetched + sum(vapply(res$entries, NROW, 0L))
    if (identical(res$status, "cancelled")) return(NULL)
    if (identical(res$status, "capped")) { fetch_status <- "capped"; fetch_message <- res$message; break }
    if (!identical(res$status, "ok")) {
      if (identical(res$status, "unauthorized") && length(all_entries) == 0) stop(res$message)
      fetch_status <- res$status; fetch_message <- res$message
      warning("Scopus fetch incomplete; returning ", n_fetched, " record(s) already fetched. ", res$message, call. = FALSE)
      break
    }
    if (n_fetched >= max_results) { fetch_status <- "capped"; fetch_message <- sprintf("Stopped at max_results = %d", as.integer(max_results)); break }
  } # End of chunk loop
  
  combined_entries <- dplyr::bind_rows(all_entries)
  if(nrow(combined_entries) == 0) return(structure(tibble::tibble(), scopus_status = fetch_status, scopus_message = fetch_message))
  
  final_tibble <- combined_entries %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      Title = if("dc:title" %in% names(.)) `dc:title` else NA_character_,
      Authors = purrr::map_chr(author, ~ {
        if (is.null(.x) || length(.x) == 0) return(NA_character_)
        if ("given-name" %in% names(.x) && "surname" %in% names(.x)) paste(.x$`given-name`, .x$surname, collapse = ", ")
        else if ("authname" %in% names(.x)) sapply(.x$authname, function(name) {
          name_parts <- unlist(strsplit(name, " "))
          if (length(name_parts) > 1) paste(paste(name_parts[-1], collapse = " "), name_parts[1]) else name
        }) %>% paste(collapse = ", ") else NA_character_
      }),
      ORCIDs = purrr::map_chr(author, ~ {
        if (is.null(.x) || length(.x) == 0 || !"orcid" %in% names(.x)) return(NA_character_)
        valid_orcids <- .x$orcid[!is.na(.x$orcid) & trimws(.x$orcid) != ""]
        if (length(valid_orcids) > 0) paste(paste0("https://orcid.org/", valid_orcids), collapse = ", ") else NA_character_
      }),
      Author_Count = purrr::map_int(author, ~ ifelse(is.null(.x), 0L, as.integer(nrow(.x)))),
      Citations    = if("citedby-count" %in% names(.)) as.numeric(`citedby-count`) else NA_real_,
      Journal      = if("prism:publicationName" %in% names(.)) `prism:publicationName` else NA_character_,
      Publisher    = if("dc:publisher" %in% names(.)) `dc:publisher` else NA_character_,
      Year         = if("prism:coverDate" %in% names(.)) substr(`prism:coverDate`, 1, 4) else NA_character_,
      # SCOPUS_ID    = if("dc:identifier" %in% names(.)) gsub(x = `dc:identifier`, pattern = "SCOPUS_ID:", replacement = "", fixed = TRUE) else NA_character_
      SCOPUS_ID = purrr::map_chr(author, ~ {
        if (is.null(.x) || length(.x) == 0) return(NA_character_)
        if ("authid" %in% names(.x)) paste(.x$`authid`, collapse = ", ") else NA_character_
      })
    ) %>% dplyr::select(Title, Authors, ORCIDs, Author_Count, Citations, Journal, Publisher, Year, SCOPUS_ID, ORCIDs)
  
  attr(final_tibble, "scopus_status") <- fetch_status; attr(final_tibble, "scopus_message") <- fetch_message
  return(final_tibble)
}

get_scopus_data_id <- function(scopus_id, api_key = NULL, queue = NULL, fetch_fn = scopus_http_get, sleep_fn = Sys.sleep,
                                  cancel_fn = function() !fs::file_exists(file.path("run.lock")),
                                  max_results = getOption("collabnet.scopus_max_results", 20000)) {
  if(is.null(api_key) || trimws(api_key) == ""){ stop("SCOPUS API Key is missing.") }
  api_key <- gsub("[^[:alnum:]]", "", api_key)
  
  clean_ids <- trimws(scopus_id)
  clean_ids <- gsub("2-s2\\.0-", "", clean_ids) 
  clean_ids <- clean_ids[clean_ids != ""]        
  if(length(clean_ids) == 0) return(tibble::tibble())
  
  chunk_size <- 30
  id_chunks <- split(clean_ids, ceiling(seq_along(clean_ids) / chunk_size))
  total_ids <- length(clean_ids)
  
  n_fetched <- 0; fetch_status <- "ok"; fetch_message <- "OK"
  
  all_entries <- list() 
  ids_processed <- 0 # Keep track of progress
  
  # Loop through each chunk
  for (chunk in id_chunks) {
    
    # --- FIRE PROGRESS MESSAGE TO SHINY UI ---
    if (!is.null(queue)) {
      ids_processed <- min(ids_processed + length(chunk), total_ids)
      
      # Pass the signal and msg (payload) so ipc::Producer$fire does not error on missing msg
      queue$producer$fire(
        signal = "update_progress", 
        msg = list(current = ids_processed, total = total_ids),
        obj = list(current = ids_processed, total = total_ids)
      )
    }
    # -----------------------------------------
    
    search_query <- paste0("AU-ID(", chunk, ")", collapse = " OR ")
    res <- scopus_paged_search(search_query, api_key, fetch_fn = fetch_fn, sleep_fn = sleep_fn,
                               cancel_fn = cancel_fn, max_results = max(0, max_results - n_fetched))
    all_entries <- append(all_entries, res$entries)
    n_fetched <- n_fetched + sum(vapply(res$entries, NROW, 0L))
    if (identical(res$status, "cancelled")) return(NULL)
    if (identical(res$status, "capped")) { fetch_status <- "capped"; fetch_message <- res$message; break }
    if (!identical(res$status, "ok")) {
      if (identical(res$status, "unauthorized") && length(all_entries) == 0) stop(res$message)
      fetch_status <- res$status; fetch_message <- res$message
      warning("Scopus fetch incomplete; returning ", n_fetched, " record(s) already fetched. ", res$message, call. = FALSE)
      break
    }
    if (n_fetched >= max_results) { fetch_status <- "capped"; fetch_message <- sprintf("Stopped at max_results = %d", as.integer(max_results)); break }
  } # End of Chunk loop
  
  combined_entries <- dplyr::bind_rows(all_entries)
  
  # print(str(combined_entries))
  
  if(nrow(combined_entries) == 0) return(structure(tibble::tibble(), scopus_status = fetch_status, scopus_message = fetch_message))
  
  final_tibble <- combined_entries %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      Title = if("dc:title" %in% names(.)) `dc:title` else NA_character_,
      Authors = purrr::map_chr(author, ~ {
        if (is.null(.x) || length(.x) == 0) return(NA_character_)
        if ("given-name" %in% names(.x) && "surname" %in% names(.x)) paste(.x$`given-name`, .x$surname, collapse = ", ")
        else if ("authname" %in% names(.x)) sapply(.x$authname, function(name) {
          name_parts <- unlist(strsplit(name, " "))
          if (length(name_parts) > 1) paste(paste(name_parts[-1], collapse = " "), name_parts[1]) else name
        }) %>% paste(collapse = ", ") else NA_character_
      }),
      ORCIDs = purrr::map_chr(author, ~ {
        if (is.null(.x) || length(.x) == 0 || !"orcid" %in% names(.x)) return(NA_character_)
        valid_orcids <- .x$orcid[!is.na(.x$orcid) & trimws(.x$orcid) != ""]
        if (length(valid_orcids) > 0) paste(paste0("https://orcid.org/", valid_orcids), collapse = ", ") else NA_character_
      }),
      Author_Count = purrr::map_int(author, ~ ifelse(is.null(.x), 0L, as.integer(nrow(.x)))),
      Citations    = if("citedby-count" %in% names(.)) as.numeric(`citedby-count`) else NA_real_,
      Journal      = if("prism:publicationName" %in% names(.)) `prism:publicationName` else NA_character_,
      Publisher    = if("dc:publisher" %in% names(.)) `dc:publisher` else NA_character_,
      Year         = if("prism:coverDate" %in% names(.)) substr(`prism:coverDate`, 1, 4) else NA_character_,
      # SCOPUS_ID    = if("dc:identifier" %in% names(.)) gsub(x = `dc:identifier`, pattern = "SCOPUS_ID:", replacement = "", fixed = TRUE) else NA_character_
      SCOPUS_ID = purrr::map_chr(author, ~ {
        if (is.null(.x) || length(.x) == 0) return(NA_character_)
        if ("authid" %in% names(.x)) paste(.x$`authid`, collapse = ", ") else NA_character_
      }),
    ) %>% dplyr::select(Title, Authors, ORCIDs, Author_Count, Citations, Journal, Publisher, Year, SCOPUS_ID, ORCIDs)
  
  attr(final_tibble, "scopus_status") <- fetch_status; attr(final_tibble, "scopus_message") <- fetch_message
  return(final_tibble)
}

get_author_mapping <- function(id, id_type = c("ORCID", "AU-ID"), api_key) {
  id_type <- match.arg(id_type)
  api_key <- gsub("[^[:alnum:]]", "", api_key)
  
  # Clean the ID just in case
  clean_id <- trimws(id)
  
  # The Author Search API is much lighter than the Document Search API
  search_query <- paste0(id_type, "(", clean_id, ")")
  
  req_url <- "https://api.elsevier.com/content/search/author"
  
  # Use tryCatch to prevent a single bad ID from crashing the whole pre-check
  mapping <- tryCatch({
    resp <- scopus_base_request(req_url, api_key) %>%
      httr2::req_url_query(query = search_query) %>%
      httr2::req_error(is_error = ~ FALSE) %>% # Handle errors manually
      httr2::req_perform()
    
    if (httr2::resp_status(resp) == 200) {
      data <- httr2::resp_body_json(resp)
      entry <- data$`search-results`$entry[[1]]
      
      # Extract the AU-ID (It usually comes formatted as "AUTHOR_ID:12345")
      au_id <- if (!is.null(entry$`dc:identifier`)) {
        gsub("AUTHOR_ID:", "", entry$`dc:identifier`)
      } else { NA_character_ }
      
      # Extract the ORCID
      orcid_val <- if (!is.null(entry$orcid)) entry$orcid else NA_character_
      
      tibble::tibble(
        Searched_Type = id_type,
        Searched_ID = clean_id,
        Mapped_AUID = au_id,
        Mapped_ORCID = orcid_val
      )
    } else {
      NULL # API failed or ID not found
    }
  }, error = function(e) {
    NULL # Network error
  })
  
  return(mapping)
}

# get_complete_scopus_data <- function(api_key = NULL, orcid, rv, output, session = NULL) {
#   
#   if(is.null(api_key) || trimws(api_key) == ""){
#     message("SCOPUS API Key is missing...Skipping")
#     return()
#   }
#   
#   # Clean the key just in case
#   api_key <- gsub("[^[:alnum:]]", "", api_key)
#   
#   if (!is.null(session)) {
#     shinyWidgets::updateProgressBar(
#       session, id = "prog_scopus", value = 0, total = 100,
#       title = "Locating Author in Scopus...", status = "info"
#     )
#   }
#   
#   # --- Build Base Request using httr2 ---
#   # We use query parameters instead of headers based on your successful curl test
#   base_req <- request("https://api.elsevier.com/content/search/scopus") %>%
#     req_url_query(
#       apiKey = api_key,
#       httpAccept = "application/json"
#     ) %>%
#     # httr2 defaults to aborting on non-200 status, which is good, 
#     # but we can customize the error message
#     req_error(is_error = function(resp) resp_status(resp) >= 400)
#   
#   all_entries <- list()
#   start_idx <- 0
#   total_results <- NA
#   
#   repeat {
#     if (!is.null(rv) && rv$is_cancelled) {
#       stop("Process cancelled by user.")
#     }
#     
#     message(paste("Fetching results starting at:", start_idx))
#     
#     # Add pagination, search parameters, AND error handling to the base request
#     current_req <- base_req %>%
#       req_url_query(
#         query = paste0("ORCID(", orcid, ")"),
#         count = 25,              
#         start = start_idx,       
#         view  = "COMPLETE"
#       ) %>%
#       req_error(is_error = ~ FALSE) # CRITICAL: Allows us to manually handle 401s
#     
#     # Perform the request
#     resp_pubs <- req_perform(current_req)
#     status <- resp_status(resp_pubs)
#     
#     # --- SMART ERROR HANDLING ---
#     if (status == 401) {
#       network_issue <- detect_vpn(rv, output)
#       
#       if (!is.null(network_issue)) {
#         stop_msg <- sprintf(
#           "Authorization Error (401): %s.", 
#           network_issue
#         )
#         stop(stop_msg)
#       } else {
#         rv$log_text <- paste(
#           rv$log_text,
#           "Authorization Error (401): API key lacks permissions. (Are you connected to the institutional network?)",
#           sep = "\n"
#         )
#         output$log <- renderText({ rv$log_text })
#         stop("Authorization Error (401)")
#       }
#     } else if (status != 200) {
#       # Handle other standard API errors (400, 429, 500, etc.)
#       rv$log_text <- paste(
#         rv$log_text,
#         paste("API Error:", status, resp_body_string(resp_pubs)),
#         sep = "\n"
#       )
#       output$log <- renderText({ rv$log_text })
#       stop(paste("API Error:", status, resp_body_string(resp_pubs)))
#     }
#     # ----------------------------
#     
#     # Extract body as string, then parse with jsonlite
#     pubs_raw <- resp_body_string(resp_pubs, encoding = "UTF-8") %>% 
#       fromJSON(flatten = TRUE)
#     
#     if (is.na(total_results)) {
#       total_results <- as.numeric(pubs_raw$`search-results`$`opensearch:totalResults`)
#       if (is.na(total_results) || total_results == 0) total_results <- 1 # Failsafe
#     }
#     
#     entries  <- pubs_raw$`search-results`$entry
#     
#     # Break loop if no more entries are found
#     if (is.null(entries) || length(entries) == 0) {
#       if (!is.null(session)) {
#         shinyWidgets::updateProgressBar(session, id = "prog_scopus", value = total_results, total = total_results,
#                                         title = "Scopus fetching complete!", status = "success")
#       }
#       break
#     }
#     
#     # Save the current batch of entries
#     all_entries <- append(all_entries, list(entries))
#     
#     if (!is.null(session)) {
#       current_fetched <- min(start_idx + nrow(entries), total_results)
#       pct <- round((current_fetched / total_results) * 100)
#       
#       shinyWidgets::updateProgressBar(
#         session,
#         id = "prog_scopus",
#         value = current_fetched,
#         total = total_results,
#         title = sprintf("Fetching Scopus Data: %d%% (%d / %d records)", pct, current_fetched, total_results),
#         status = if(pct == 100) "success" else "primary"
#       )
#     }
#     
#     # If the returned batch has less than 25 items, we've reached the end
#     if (nrow(entries) < 1) break
#     
#     # Increment the start index for the next page
#     # start_idx <- start_idx + 25
#     start_idx <- start_idx + nrow(entries)
#   }
#   
#   # repeat {
#   #   if (!is.null(rv) && rv$is_cancelled) {
#   #     stop("Process cancelled by user.")
#   #   }
#   #   
#   #   message(paste("Fetching results starting at:", start_idx))
#   #   
#   #   # Add pagination and search parameters to the base request
#   #   # Using ORCID directly to bypass the Author API 401 errors
#   #   current_req <- base_req %>%
#   #     req_url_query(
#   #       query = paste0("ORCID(", orcid, ")"),
#   #       count = 25,              # Must be 25 or less for COMPLETE view!
#   #       start = start_idx,       # Offset for pagination
#   #       view  = "COMPLETE"
#   #     )
#   #   
#   #   # Perform the request
#   #   resp_pubs <- req_perform(current_req)
#   #   
#   #   # Extract body as string, then parse with jsonlite (maintains your original flatten structure)
#   #   pubs_raw <- resp_body_string(resp_pubs, encoding = "UTF-8") %>% 
#   #     fromJSON(flatten = TRUE)
#   #   
#   #   if (is.na(total_results)) {
#   #     total_results <- as.numeric(pubs_raw$`search-results`$`opensearch:totalResults`)
#   #     if (is.na(total_results) || total_results == 0) total_results <- 1 # Failsafe
#   #   }
#   #   
#   #   entries  <- pubs_raw$`search-results`$entry
#   #   
#   #   # Break loop if no more entries are found
#   #   if (is.null(entries) || length(entries) == 0) {
#   #     if (!is.null(session)) {
#   #       shinyWidgets::updateProgressBar(session, id = "prog_scopus", value = total_results, total = total_results,
#   #                                       title = "Scopus fetching complete!", status = "success")
#   #     }
#   #     break
#   #   }
#   #   
#   #   # Save the current batch of entries
#   #   all_entries <- append(all_entries, list(entries))
#   #   
#   #   if (!is.null(session)) {
#   #     current_fetched <- min(start_idx + nrow(entries), total_results)
#   #     pct <- round((current_fetched / total_results) * 100)
#   #     
#   #     shinyWidgets::updateProgressBar(
#   #       session,
#   #       id = "prog_scopus",
#   #       value = current_fetched,
#   #       total = total_results,
#   #       title = sprintf("Fetching Scopus Data: %d%% (%d / %d records)", pct, current_fetched, total_results),
#   #       status = if(pct == 100) "success" else "primary"
#   #     )
#   #   }
#   #   
#   #   # If the returned batch has less than 25 items, we've reached the end
#   #   if (nrow(entries) < 25) break
#   #   
#   #   # Increment the start index for the next page
#   #   start_idx <- start_idx + 25
#   # }
#   
#   # Combine all the paginated batches into one large data frame
#   combined_entries <- bind_rows(all_entries)
#   
#   # 1. Capture the total number of rows before the pipeline starts
#   total_records <- nrow(combined_entries)
#   
#   if(total_records == 0) {
#     return(tibble::tibble())
#   }
#   
#   # Wrap the pipeline to format data
#   final_tibble <- combined_entries %>%
#     as_tibble() %>%
#     mutate(
#       Title = if("dc:title" %in% names(.)) `dc:title` else NA_character_,
#       
#       Authors = purrr::imap_chr(author, ~ {
#         
#         current_idx <- .y 
#         
#         # 2. UPDATE PROGRESS BAR (Throttled)
#         if (!is.null(session) && (current_idx %% 10 == 0 || current_idx == total_records)) {
#           pct <- round((current_idx / total_records) * 100)
#           
#           shinyWidgets::updateProgressBar(
#             session,
#             id = "prog_scopus",
#             value = current_idx,
#             total = total_records,
#             title = sprintf("Formatting Records: %d%% (%d / %d)", pct, current_idx, total_records),
#             status = "info"
#           )
#         }
#         
#         if (is.null(.x) || length(.x) == 0) return(NA_character_)
#         
#         if ("given-name" %in% names(.x) && "surname" %in% names(.x)) {
#           name_strings <- paste(.x$`given-name`, .x$surname)
#         } else if ("authname" %in% names(.x)) {
#           name_strings <- sapply(.x$authname, function(name) {
#             name_parts <- unlist(strsplit(name, " "))
#             if (length(name_parts) > 1) {
#               paste(paste(name_parts[-1], collapse = " "), name_parts[1])
#             } else {
#               name
#             }
#           })
#         } else {
#           return(NA_character_)
#         }
#         paste(name_strings, collapse = ", ")
#       }),
#       
#       Author_Count = map_int(author, ~ ifelse(is.null(.x), 0L, as.integer(nrow(.x)))),
#       Citations    = if("citedby-count" %in% names(.)) as.numeric(`citedby-count`) else NA_real_,
#       Journal      = if("prism:publicationName" %in% names(.)) `prism:publicationName` else NA_character_,
#       Publisher    = if ("dc:publisher" %in% names(.)) `dc:publisher` else NA_character_,
#       Year         = if("prism:coverDate" %in% names(.)) substr(`prism:coverDate`, 1, 4) else NA_character_
#     ) %>%
#     select(Title, Authors, Author_Count, Citations, Journal, Publisher, Year)
#   
#   return(final_tibble)
# }
