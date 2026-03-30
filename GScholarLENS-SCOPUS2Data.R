require(httr)
require(jsonlite)
require(dplyr)
require(purrr)
require(stringr)

get_complete_scopus_data <- function(api_key = NULL, orcid) {
  if(is.null(api_key)){
    message("SCOPUS API Key is missing...Skipping")
    return()
  }
  headers <- add_headers(
    "X-ELS-APIKey" = api_key,
    "Accept" = "application/json"
  )
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
  
  repeat {
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
    entries  <- pubs_raw$`search-results`$entry
    
    # Break loop if no more entries are found
    if (is.null(entries) || length(entries) == 0) break
    
    # Save the current batch of entries
    all_entries <- append(all_entries, list(entries))
    
    # If the returned batch has less than 25 items, we've reached the end
    if (nrow(entries) < 25) break
    
    # Increment the start index for the next page
    start_idx <- start_idx + 25
  }
  
  # Combine all the paginated batches into one large data frame
  combined_entries <- bind_rows(all_entries)
  
  # --- STEP 3: Build the final Tibble ---
  final_tibble <- combined_entries %>%
    as_tibble() %>%
    mutate(
      Title        = `dc:title`,
      # Authors      = map_chr(author, ~ {
      #   if (is.null(.x) || length(.x) == 0) return(NA_character_)
      #   paste(.x$authname, collapse = "; ")
      # }),
      Authors = map_chr(author, ~ {
        if (is.null(.x) || length(.x) == 0) return(NA_character_)
        
        # 1. Attempt to fetch Full Names (First Last) if available in Scopus response
        if ("given-name" %in% names(.x) && "surname" %in% names(.x)) {
          name_strings <- paste(.x$`given-name`, .x$surname)
        } else {
          # 2. Fallback: Switch existing authname (e.g., "Sharma G." -> "G. Sharma")
          name_strings <- sapply(.x$authname, function(name) {
            # Splitting by space to separate Surname and Initials
            name_parts <- unlist(strsplit(name, " "))
            if (length(name_parts) > 1) {
              # Places the initials/first name first and surname last
              paste(paste(name_parts[-1], collapse = " "), name_parts[1])
            } else {
              name
            }
          })
        }
        
        # 3. Join authors with ',' as requested
        paste(name_strings, collapse = ", ")
      }),
      Author_Count = map_int(author, ~ ifelse(is.null(.x), 0L, nrow(.x))),
      Citations    = as.numeric(`citedby-count`),
      Journal      = `prism:publicationName`,
      Publisher    = if ("dc:publisher" %in% names(.)) `dc:publisher` else NA_character_, 
      Year         = substr(`prism:coverDate`, 1, 4)
    ) %>%
    select(Title, Authors, Author_Count, Citations, Journal, Publisher, Year)
  
  return(final_tibble)
}
