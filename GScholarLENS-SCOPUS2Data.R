suppressPackageStartupMessages(require(httr))
suppressPackageStartupMessages(require(jsonlite))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(purrr))
suppressPackageStartupMessages(require(stringr))

suppressPackageStartupMessages(require(httr2))
suppressPackageStartupMessages(require(jsonlite))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(purrr))
suppressPackageStartupMessages(require(stringr))

get_complete_scopus_data <- function(api_key = NULL, orcid) {
  if(is.null(api_key) || trimws(api_key) == ""){
    stop("SCOPUS API Key is missing.")
  }
  
  api_key <- gsub("[^[:alnum:]]", "", api_key)
  print("HERE12")
  # Calculate is_WASM locally inside the worker!
  local_is_WASM <- grepl(pattern="wasm", x=Sys.info()["machine"])
  
  if (!local_is_WASM) {
    base_req <- httr2::request("https://api.elsevier.com/content/search/scopus") %>%
      httr2::req_url_query(apiKey = api_key, httpAccept = "application/json") %>%
      httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 400)
  }
  
  all_entries <- list()
  start_idx <- 0
  total_results <- NA
  print("HERE13")
  repeat {
    status <- 200
    pubs_raw <- NULL
    err_body <- ""
    print("HERE13.1")
    if (local_is_WASM) {
      encoded_query <- URLencode(paste0("ORCID(", orcid, ")"), reserved = TRUE)
      req_url <- sprintf(
        "https://api.elsevier.com/content/search/scopus?apiKey=%s&httpAccept=application/json&query=%s&count=25&start=%d&view=COMPLETE",
        api_key, encoded_query, start_idx
      )
      
      con <- NULL
      res_err <- tryCatch({
        con <- url(req_url, headers = c(Accept = "application/json"))
        lines <- readLines(con, warn = FALSE)
        txt <- paste(lines, collapse = "\n")
        pubs_raw <- jsonlite::fromJSON(txt, flatten = TRUE)
        NULL 
      }, error = function(e) {
        conditionMessage(e) 
      }, finally = {
        if (!is.null(con) && inherits(con, "connection")) try(close(con), silent = TRUE)
      })
      
      if (!is.null(res_err)) {
        if (grepl("401", res_err)) status <- 401
        else if (grepl("429", res_err)) status <- 429
        else status <- 500
        err_body <- res_err
      }
      
    } else {
      current_req <- base_req %>%
        httr2::req_url_query(
          query = paste0("ORCID(", orcid, ")"), count = 25, start = start_idx, view  = "COMPLETE"
        ) %>% httr2::req_error(is_error = ~ FALSE)
      
      resp_pubs <- httr2::req_perform(current_req)
      status <- httr2::resp_status(resp_pubs)
      
      if (status == 200) {
        pubs_raw <- httr2::resp_body_string(resp_pubs, encoding = "UTF-8") %>% 
          jsonlite::fromJSON(flatten = TRUE)
      } else {
        err_body <- httr2::resp_body_string(resp_pubs)
      }
    }
    
    print("HERE13.2")
    
    if (status == 401) stop("Authorization Error (401): API key lacks permissions. Check network.")
    else if (status != 200) stop(sprintf("API Error (%d): %s", status, err_body))
    
    if (is.na(total_results)) {
      total_results <- as.numeric(pubs_raw$`search-results`$`opensearch:totalResults`)
      if (is.na(total_results) || total_results == 0) total_results <- 1 
    }
    
    entries  <- pubs_raw$`search-results`$entry
    if (is.null(entries) || length(entries) == 0) break
    
    all_entries <- append(all_entries, list(entries))
    if (nrow(entries) < 1) break
    start_idx <- start_idx + nrow(entries)
  } 
  
  combined_entries <- dplyr::bind_rows(all_entries)
  if(nrow(combined_entries) == 0) return(tibble::tibble())
  
  final_tibble <- combined_entries %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      Title = if("dc:title" %in% names(.)) `dc:title` else NA_character_,
      Authors = purrr::map_chr(author, ~ {
        if (is.null(.x) || length(.x) == 0) return(NA_character_)
        if ("given-name" %in% names(.x) && "surname" %in% names(.x)) {
          paste(.x$`given-name`, .x$surname, collapse = ", ")
        } 
        else if ("authname" %in% names(.x)) {
          sapply(.x$authname, function(name) {
            name_parts <- unlist(strsplit(name, " "))
            if (length(name_parts) > 1) {
              paste(paste(name_parts[-1], collapse = " "), name_parts[1])
            } else {
              name
            }
          }) %>% paste(collapse = ", ")
        } 
        else {
          NA_character_
        }
      }),
      Author_Count = purrr::map_int(author, ~ ifelse(is.null(.x), 0L, as.integer(nrow(.x)))),
      Citations    = if("citedby-count" %in% names(.)) as.numeric(`citedby-count`) else NA_real_,
      Journal      = if("prism:publicationName" %in% names(.)) `prism:publicationName` else NA_character_,
      Publisher    = if ("dc:publisher" %in% names(.)) `dc:publisher` else NA_character_,
      Year         = if("prism:coverDate" %in% names(.)) substr(`prism:coverDate`, 1, 4) else NA_character_
    ) %>%
    dplyr::select(Title, Authors, Author_Count, Citations, Journal, Publisher, Year)
  
  return(final_tibble)
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
