require(httr)
require(jsonlite)
require(dplyr)
require(purrr)
require(stringr)

get_complete_scopus_data <- function(api_key = NULL, orcid, rv, session = NULL) {
  if(is.null(api_key)){
    message("SCOPUS API Key is missing...Skipping")
    return()
  }
  headers <- add_headers(
    "X-ELS-APIKey" = api_key,
    "Accept" = "application/json"
  )

  if (!is.null(session)) {
    shinyWidgets::updateProgressBar(
      session, id = "doi_progress", value = 0, total = 100,
      title = "Locating Author in Scopus...", status = "info"
    )
  }
  # print(api_key) #DEBUG
  # print(orcid) #DEBUG
  # --- STEP 1: Get AU-ID ---
  author_url <- paste0("https://api.elsevier.com/content/author/orcid/", orcid)
  resp_auth <- GET(author_url, headers)
  # print(str(resp_auth))
  stop_for_status(resp_auth)

  auth_data <- content(resp_auth, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
  raw_id <- auth_data$`author-retrieval-response`$`coredata.dc:identifier`[1]
  au_id  <- str_extract(raw_id, "\\d+")

  message(paste("Extracted AU-ID:", au_id))

  # --- STEP 2: Paginate through Publications ---
  search_url <- "https://api.elsevier.com/content/search/scopus"

  all_entries <- list()
  start_idx <- 0
  total_results <- NA

  repeat {
    if (!is.null(rv) && rv$is_cancelled) {
      stop("Process caneclled by user.")
    }

    message(paste("Fetching results starting at:", start_idx))

    pub_params <- list(
      query = paste0("AU-ID(", au_id, ")"),
      count = 25,              # Must be 25 or less for COMPLETE view!
      start = start_idx,       # Offset for pagination
      view  = "COMPLETE"
    )

    resp_pubs <- GET(search_url, headers, query = pub_params)

    if (status_code(resp_pubs) != 200) {
      stop(paste("API Error:", status_code(resp_pubs), content(resp_pubs, as="text")))
    }

    pubs_raw <- content(resp_pubs, as = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)

    if (is.na(total_results)) {
      total_results <- as.numeric(pubs_raw$`search-results`$`opensearch:totalResults`)
      if (is.na(total_results) || total_results == 0) total_results <- 1 # Failsafe
    }

    entries  <- pubs_raw$`search-results`$entry

    # Break loop if no more entries are found
    if (is.null(entries) || length(entries) == 0) {
      # Max out progress bar before breaking
      if (!is.null(session)) {
        shinyWidgets::updateProgressBar(session, id = "doi_progress", value = total_results, total = total_results,
                                        title = "Scopus fetching complete!", status = "success")
      }
      break
    }

    # Save the current batch of entries
    all_entries <- append(all_entries, list(entries))

    if (!is.null(session)) {
      # Calculate how many we have fetched so far (either start_idx + 25, or the total, whichever is smaller)
      current_fetched <- min(start_idx + nrow(entries), total_results)
      pct <- round((current_fetched / total_results) * 100)

      shinyWidgets::updateProgressBar(
        session,
        id = "doi_progress",
        value = current_fetched,
        total = total_results,
        title = sprintf("Fetching Scopus Data: %d%% (%d / %d records)", pct, current_fetched, total_results),
        status = if(pct == 100) "success" else "primary" # Use a different color (blue) for Scopus vs DOIs
      )
    }

    # If the returned batch has less than 25 items, we've reached the end
    if (nrow(entries) < 25) break

    # Increment the start index for the next page
    start_idx <- start_idx + 25
  }

  # Combine all the paginated batches into one large data frame
  combined_entries <- bind_rows(all_entries)

  # --- STEP 3: Build the final Tibble ---
  # final_tibble <- combined_entries %>%
  #   as_tibble() %>%
  #   mutate(
  #     Title        = `dc:title`,
  #     # Authors      = map_chr(author, ~ {
  #     #   if (is.null(.x) || length(.x) == 0) return(NA_character_)
  #     #   paste(.x$authname, collapse = "; ")
  #     # }),
  #     Authors = map_chr(author, ~ {
  #       if (is.null(.x) || length(.x) == 0) return(NA_character_)
  #
  #       # 1. Attempt to fetch Full Names (First Last) if available in Scopus response
  #       if ("given-name" %in% names(.x) && "surname" %in% names(.x)) {
  #         name_strings <- paste(.x$`given-name`, .x$surname)
  #       } else {
  #         # 2. Fallback: Switch existing authname (e.g., "Sharma G." -> "G. Sharma")
  #         name_strings <- sapply(.x$authname, function(name) {
  #           # Splitting by space to separate Surname and Initials
  #           name_parts <- unlist(strsplit(name, " "))
  #           if (length(name_parts) > 1) {
  #             # Places the initials/first name first and surname last
  #             paste(paste(name_parts[-1], collapse = " "), name_parts[1])
  #           } else {
  #             name
  #           }
  #         })
  #       }
  #
  #       # 3. Join authors with ',' as requested
  #       paste(name_strings, collapse = ", ")
  #     }),
  #     Author_Count = map_int(author, ~ ifelse(is.null(.x), 0L, nrow(.x))),
  #     Citations    = as.numeric(`citedby-count`),
  #     Journal      = `prism:publicationName`,
  #     Publisher    = if ("dc:publisher" %in% names(.)) `dc:publisher` else NA_character_,
  #     Year         = substr(`prism:coverDate`, 1, 4)
  #   ) %>%
  #   select(Title, Authors, Author_Count, Citations, Journal, Publisher, Year)
  # return(future::future({
    # 1. Capture the total number of rows before the pipeline starts
    total_records <- nrow(combined_entries)

    # Wrap the pipeline in tryCatch to gracefully catch the intentional cancellation
    final_tibble <- combined_entries %>%
        as_tibble() %>%
        mutate(
          Title = `dc:title`,

          Authors = purrr::imap_chr(author, ~ {

            # # --- 1. INSTANT CANCEL CHECK ---
            # # If the user clicked cancel, throw a specific error to instantly break the loop
            # if (!is.null(rv) && rv$is_cancelled) {
            #   stop("USER_CANCELLED")
            # }

            current_idx <- .y # .y is the current row number (1, 2, 3...)

            # 2. UPDATE PROGRESS BAR (Throttled)
            if (!is.null(session) && (current_idx %% 10 == 0 || current_idx == total_records)) {
              pct <- round((current_idx / total_records) * 100)

              shinyWidgets::updateProgressBar(
                session,
                id = "doi_progress",
                value = current_idx,
                total = total_records,
                title = sprintf("Formatting Records: %d%% (%d / %d)", pct, current_idx, total_records),
                status = "info"
              )
            }

            if (is.null(.x) || length(.x) == 0) return(NA_character_)

            if ("given-name" %in% names(.x) && "surname" %in% names(.x)) {
              name_strings <- paste(.x$`given-name`, .x$surname)
            } else {
              name_strings <- sapply(.x$authname, function(name) {
                name_parts <- unlist(strsplit(name, " "))
                if (length(name_parts) > 1) {
                  paste(paste(name_parts[-1], collapse = " "), name_parts[1])
                } else {
                  name
                }
              })
            }
            paste(name_strings, collapse = ", ")
          }),

          Author_Count = map_int(author, ~ ifelse(is.null(.x), 0L, nrow(.x))),
          Citations    = as.numeric(`citedby-count`),
          Journal      = `prism:publicationName`,
          Publisher    = if ("dc:publisher" %in% names(.)) `dc:publisher` else NA_character_,
          Year         = substr(`prism:coverDate`, 1, 4)
        ) %>%
        select(Title, Authors, Author_Count, Citations, Journal, Publisher, Year)

    # }, error = function(e) {
    #   # --- 2. CATCH THE ERROR ---
    #   # If the error is our custom cancel message, return an empty tibble silently
    #   if (e$message == "USER_CANCELLED") {
    #     message("Data formatting aborted by user.")
    #     return(tibble::tibble())
    #   } else {
    #     # If it's a real code error, re-throw it so you can debug
    #     stop(e)
    #   }
    # })
    return(final_tibble)
  # }))
}