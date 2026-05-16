suppressPackageStartupMessages(require(httr))
suppressPackageStartupMessages(require(httr2))
suppressPackageStartupMessages(require(jsonlite))
suppressPackageStartupMessages(require(stringi))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(openxlsx))

is_WASM <- grepl(pattern="wasm",x=Sys.info()["machine"])
# use a multisession plan so futures run in background R sessions
# if(!is_WASM){
  # future::plan(future::multisession)
# future::plan(future.callr::callr)
future::plan(future::multicore)
# }else{
#   future::plan(future::sequential)
# }

# Universal Extractor: Pulls pure DOIs out of full URLs, dirty text, and ignores non-DOIs (like Scopus EIDs)
extract_pure_doi <- function(raw_id) {
  if (is.na(raw_id) || trimws(raw_id) == "") return(NA_character_)
  # Regex to find exactly: "10." + 4-9 digits + "/" + valid DOI characters
  m <- regexpr("10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+", raw_id, ignore.case = TRUE)
  if (m != -1) return(regmatches(raw_id, m))
  return(NA_character_) # Return NA if it's a Scopus ID, PMID, etc.
}

detect_identifier_type <- function(raw_id) {
  # Base case for empty or NA inputs
  if (is.na(raw_id) || trimws(raw_id) == "") {
    return(list(original = raw_id, clean_id = NA_character_, type = "Unknown", is_doi = FALSE))
  }
  
  raw_id <- trimws(raw_id)
  
  # 1. DOI (Digital Object Identifier)
  # Looks for '10.' followed by 4-9 digits, a slash, and the suffix
  doi_match <- regexpr("10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+", raw_id, ignore.case = TRUE)
  if (doi_match != -1) {
    clean_doi <- regmatches(raw_id, doi_match)
    return(list(original = raw_id, clean_id = clean_doi, type = "DOI", is_doi = TRUE))
  }
  
  # 2. Scopus EID (Electronic Identifier)
  # Format: "2-s2.0-" followed by digits
  if (grepl("^2-s2\\.0-\\d+$", raw_id, ignore.case = TRUE)) {
    return(list(original = raw_id, clean_id = raw_id, type = "Scopus EID", is_doi = FALSE))
  }
  
  # 3. PMCID (PubMed Central ID)
  # Format: "PMC" followed by digits
  if (grepl("^PMC\\d+$", raw_id, ignore.case = TRUE)) {
    return(list(original = raw_id, clean_id = toupper(raw_id), type = "PMCID", is_doi = FALSE))
  }
  
  # 4. PMID (PubMed ID)
  # Format: 1 to 8 digits, optionally preceded by "PMID:"
  pmid_match <- regexpr("^(PMID:?\\s*)?(\\d{1,8})$", raw_id, ignore.case = TRUE)
  if (pmid_match != -1) {
    # Extract just the pure numeric digits for the clean ID
    digits_only <- gsub("[^0-9]", "", raw_id)
    return(list(original = raw_id, clean_id = digits_only, type = "PMID", is_doi = FALSE))
  }
  
  # 5. ArXiv ID
  # Format: "YYMM.NNNNN" or "arXiv:YYMM.NNNNN"
  if (grepl("^(arXiv:)?\\d{4}\\.\\d{4,5}(v\\d+)?$", raw_id, ignore.case = TRUE)) {
    # Strip the "arXiv:" prefix if it exists to normalize
    clean_arxiv <- gsub("(?i)^arxiv:\\s*", "", raw_id)
    return(list(original = raw_id, clean_id = clean_arxiv, type = "ArXiv", is_doi = FALSE))
  }
  
  # 6. ISBN (Books/Chapters)
  # Format: 10 or 13 digits (ignoring hyphens)
  clean_isbn <- gsub("-", "", raw_id)
  if (grepl("^(97(8|9))?\\d{9}(\\d|X)$", clean_isbn, ignore.case = TRUE)) {
    return(list(original = raw_id, clean_id = clean_isbn, type = "ISBN", is_doi = FALSE))
  }
  
  # 7. Generic URL (Webpage scraping)
  if (grepl("^https?://", raw_id, ignore.case = TRUE)) {
    return(list(original = raw_id, clean_id = raw_id, type = "URL", is_doi = FALSE))
  }
  
  # Fallback for anything else
  return(list(original = raw_id, clean_id = raw_id, type = "Other", is_doi = FALSE))
}


extract_ris <- function(doi_or_url,
                        write_file = NULL,
                        timeout_secs = 15,
                        user_agent_str = paste0("R (", R.version$version.string, ")")) {
  
  stopifnot(is.character(doi_or_url), length(doi_or_url) == 1)
  
  # normalize DOI: remove leading doi.org, whitespace
  doi <- sub("^\\s+|\\s+$", "", doi_or_url)
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  doi <- sub("^doi:", "", doi, ignore.case = TRUE)
  doi <- trimws(doi)
  
  if (doi == "") stop("Couldn't parse DOI from input.")
  
  # URL-encode the DOI for placing into a URL path
  doi_enc <- utils::URLencode(doi, reserved = TRUE)
  
  # helper to do GET with common headers
  get_with_ris_accept <- function(url) {
    tryCatch({
      if(is_WASM){
        # Open a connection, passing the Accept header
        con <- url(url, headers = c(Accept = "application/x-research-info-systems"))
        lines <- readLines(con, warn = FALSE)
        close(con)
        return(paste(lines, collapse = "\n"))
      }else{
        res <- GET(url,
                   add_headers(Accept = "application/x-research-info-systems"),
                   user_agent(user_agent_str),
                   timeout(timeout_secs))
        if (inherits(res, "response") && status_code(res) == 200) {
          # extract as text
          txt <- content(res, as = "text", encoding = "UTF-8")
          # quick sanity check: RIS often starts with "TY  - "
          if (nzchar(txt) && grepl("^TY  - |^TY - ", txt)) {
            return(txt)
          } else {
            # some servers may return RIS but without the typical header; still accept non-empty text
            if (nzchar(txt)) return(txt)
          }
        }
      }
    }, error = function(e) {
      if (exists("con")) try(close(con), silent = TRUE)
      return(NULL) # Return NULL on failure
    })
  }
  
  # 1) Try doi.org content negotiation
  doi_url <- paste0("https://doi.org/", doi_enc)
  ris_text <- NULL
  ris_text <- tryCatch(get_with_ris_accept(doi_url), error = function(e) e)
  # message(paste("RIS TEXT 1:", ris_text))
  
  # 2) Fallback: CrossRef transform endpoint: /works/{doi}/transform/application/x-research-info-systems
  if (is.null(ris_text)) {
    crossref_url <- paste0("https://api.crossref.org/works/", doi_enc,
                           "/transform/application/x-research-info-systems")
    ris_text <- tryCatch(get_with_ris_accept(crossref_url), error = function(e) e)
  }
  # message(paste("RIS TEXT 2:", ris_text))
  
  if (is.null(ris_text)) {
    # stop("Failed to retrieve RIS. The DOI may not support content negotiation and CrossRef doesn't have a transform for it.")
    return(NULL)
  }
  
  # Optionally write to file
  if (!is.null(write_file)) {
    writeLines(ris_text, con = write_file, useBytes = TRUE)
    message("Written RIS to: ", write_file)
  }
  
  return(ris_text)
  # ris_lines <- strsplit(ris_text, "\n")[[1]]
  # author_list <- gsub("^AU\\s+-\\s+", "", x = grep(pattern = "AU", ris_lines,value = T)) #Extracting Authors from AU lines
  # author_list_corrected <- unlist(lapply(strsplit(x = author_list,split = ", ", fixed = T), FUN=function(x){
  #   return(paste(x[2],x[1]))
  # }))
  # return(data.frame(Authors=paste(author_list_corrected, collapse = ", ")))
}

resolve_non_doi_ris <- function(clean_id, id_type, scopus_key = NULL) {
  if (is.na(clean_id) || is.na(id_type)) return(NULL)
  
  ris_text <- NULL
  
  tryCatch({
    # ==========================================
    # 1. PubMed (PMID) & PubMed Central (PMCID)
    # ==========================================
    if (id_type %in% c("PMID", "PMCID")) {
      # Hit the NCBI ID Converter API
      conv_url <- paste0("https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/?ids=", 
                         utils::URLencode(clean_id), "&format=json")
      
      # Use base R connection for WASM compatibility
      con <- url(conv_url)
      json_data <- paste(readLines(con, warn = FALSE), collapse = "\n")
      close(con)
      
      parsed <- jsonlite::fromJSON(json_data)
      
      # If NCBI found a DOI for this PubMed ID, extract it!
      if (!is.null(parsed$records) && "doi" %in% names(parsed$records)) {
        mapped_doi <- parsed$records$doi[1]
        if (!is.na(mapped_doi) && mapped_doi != "") {
          # Pass the discovered DOI to your existing function!
          ris_text <- extract_ris(mapped_doi) 
        }
      }
    }
    
    # ==========================================
    # 2. ArXiv IDs
    # ==========================================
    else if (id_type == "ArXiv") {
      # ArXiv assigned official DOIs to all papers using this prefix
      arxiv_doi <- paste0("10.48550/arXiv.", clean_id)
      ris_text <- extract_ris(arxiv_doi)
    }
    
    # ==========================================
    # 3. Scopus EIDs
    # ==========================================
    else if (id_type == "Scopus EID") {
      # Strip the '2-s2.0-' prefix to get the pure Scopus Number
      scopus_num <- gsub("^2-s2\\.0-", "", clean_id)
      
      if (!is.null(scopus_key) && scopus_key != "") {
        scopus_url <- paste0("https://api.elsevier.com/content/abstract/scopus_id/", scopus_num)
        
        if (exists("is_WASM") && is_WASM) {
          # WASM compatible fetch with Elsevier headers
          con <- url(scopus_url, headers = c(
            Accept = "application/x-research-info-systems",
            `X-ELS-APIKey` = scopus_key
          ))
          lines <- readLines(con, warn = FALSE)
          close(con)
          ris_text <- paste(lines, collapse = "\n")
        } else {
          # Local R httr fetch
          res <- httr::GET(
            scopus_url,
            httr::add_headers(
              Accept = "application/x-research-info-systems",
              `X-ELS-APIKey` = scopus_key
            ),
            httr::timeout(15)
          )
          if (httr::status_code(res) == 200) {
            ris_text <- httr::content(res, as = "text", encoding = "UTF-8")
          }
        }
      }
    }
    
  }, error = function(e) {
    if (exists("con")) try(close(con), silent = TRUE)
  })
  
  return(ris_text)
}

normalize_doi <- function(doi_or_url) {
  doi <- trimws(doi_or_url)
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  doi <- sub("^doi:", "", doi, ignore.case = TRUE)
  doi <- trimws(doi)
  if (doi == "") stop("Couldn't parse DOI.")
  doi
}

safe_fetch_with_backoff <- function(url, req_headers = NULL, max_retries = 3) {
  wait_time <- 2 
  
  # OPTIMIZATION 1: Hoist header creation outside the loop (Local R)
  if (!is_WASM) {
    # 1. Initialize the httr2 request
    req <- httr2::request(url) |> 
      httr2::req_user_agent("R (httr2)") |> 
      # PREVENT httr2 from throwing an R error on 4xx/5xx so we can read the status manually
      httr2::req_error(is_error = ~ FALSE) 
    
    # 2. Add headers if they exist
    if (!is.null(req_headers) && length(req_headers) > 0) {
      # httr2::req_headers uses tidy evaluation, so we splice the list with !!!
      req <- httr2::req_headers(req, !!!req_headers)
    }
  }
  
  for (i in 1:max_retries) {
    txt <- NULL
    success <- FALSE
    fatal_error <- FALSE # Flag to instantly kill the loop
    
    if (is_WASM) {
      con <- NULL 
      tryCatch({
        con <- url(url, headers = req_headers)
        txt <- paste0(readLines(con, warn = FALSE), collapse = "\n")
        success <- TRUE
      }, error = function(e) {
        # Fetch the error string
        err_msg <- conditionMessage(e)
        message(paste("ERROR:",str(e),e,err_msg))
        # OPTIMIZATION 2: WebR throws the HTTP status in the error string.
        # If it's a rate limit, just let the loop continue and backoff
        if (grepl("429", err_msg)) {
          fatal_error <<- FALSE
        } 
        # If it's a true client error (Not Found, Bad Request), kill it
        else if (grepl("400|401|403|404", err_msg)) {
          fatal_error <<- TRUE
        }
      }, finally = {
        if (!is.null(con) && inherits(con, "connection")) {
          try(close(con), silent = TRUE)
        }
      })
      
    } else {
      # Standard local R (httr2)
      res <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
      
      if (!is.null(res)) {
        status <- httr2::resp_status(res)
        
        if (status == 200) {
          # httr2 safely extracts text body
          txt <- httr2::resp_body_string(res) 
          success <- TRUE
        } else if (status == 429) {
          # Extract retry header in httr2
          retry_header <- httr2::resp_header(res, "retry-after")
          if (!is.null(retry_header)) wait_time <- as.numeric(retry_header) + 1
        } else if (status >= 400 && status < 500) {
          # OPTIMIZATION 3: Local R fatal client error check
          fatal_error <- TRUE
        }
      }
    }
    
    # If successful, return the text immediately
    if (success && !is.null(txt) && txt != "") {
      return(txt)
    }
    
    # If the API explicitly told us the record doesn't exist, stop wasting time
    if (fatal_error) {
      break
    }
    
    # If failed (Timeout, 429, 500, 502), wait and increase the backoff time
    if (i < max_retries) {
      Sys.sleep(wait_time)
      wait_time <- wait_time * 2 
    }
  }
  
  return(NULL) 
}

get_crossref_count <- function(doi, crossref_key = NULL) {
  doi_e <- URLencode(doi, reserved = TRUE)
  url <- paste0("https://api.crossref.org/works/", doi_e)
  
  # 1. Base headers
  headers <- c(Accept = "application/json")
  
  # 2. Add API key if available (Crossref uses 'Crossref-Plus-API-Token' for auth)
  if (!is.null(crossref_key) && trimws(crossref_key) != "") {
    headers["Crossref-Plus-API-Token"] <- crossref_key
  }
  
  txt <- safe_fetch_with_backoff(url, req_headers = headers)
  if (is.null(txt)) return(NA_integer_)
  
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(j)) return(NA_integer_)
  
  cnt <- j$message$`is-referenced-by-count`
  if (is.null(cnt)) return(NA_integer_) else return(as.integer(cnt))
}

get_opencitations_count <- function(doi, opencitations_key = NULL) {
  doi_e <- URLencode(doi, reserved = TRUE)
  url <- paste0("https://api.opencitations.net/index/v1/citation-count/", doi_e)
  
  # 1. Base headers
  headers <- c(Accept = "application/json")
  
  # 2. Add API key if available (OpenCitations uses the 'authorization' header)
  if (!is.null(opencitations_key) && trimws(opencitations_key) != "") {
    headers["authorization"] <- opencitations_key
  }
  
  txt <- safe_fetch_with_backoff(url, req_headers = headers)
  if (is.null(txt)) return(NA_integer_)
  
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(j)) return(NA_integer_)
  
  if (!is.null(j$count)) return(as.integer(j$count))
  if (is.data.frame(j) && "count" %in% names(j)) return(as.integer(j$count[1]))
  if (is.list(j) && length(j) >= 1 && !is.null(j[[1]]$count)) return(as.integer(j[[1]]$count))
  
  return(NA_integer_)
}

get_semanticscholar_count <- function(doi, api_key = NULL) {
  doi_e <- URLencode(doi, reserved = TRUE)
  url <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", doi_e, "?fields=citationCount")
  
  # message(paste("HERE0",glens_env$semantic_key))
  if(is.null(api_key) && !is.null(glens_env$semantic_key)){
    # message("HERE1")
    if(!stringi::stri_isempty(glens_env$semantic_key)){
      api_key <- glens_env$semantic_key
      # message("HERE2")
    }
  }
  
  if(is.null(api_key)){
    # message("HERE3")
    return(NA_integer_)  
  }
  
  req_headers <- c(Accept = "application/json")
  if (!is.null(api_key)) {
    req_headers["Authorization"] <- paste("Bearer", api_key)
  }
  
  txt <- safe_fetch_with_backoff(url, req_headers = req_headers)
  if (is.null(txt)) return(NA_integer_)
  
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(j)) return(NA_integer_)
  
  if (!is.null(j$citationCount)) return(as.integer(j$citationCount))
  
  return(NA_integer_)
}

get_crossref_count_async <- function(doi, crossref_key = NULL, input_id = "api_count_result") {
  # Clean up the key for JS injection
  key_arg <- if(is.null(crossref_key) || trimws(crossref_key) == "") "null" else paste0("'", crossref_key, "'")
  
  js_code <- sprintf("window.fetchCitationCount('crossref', '%s', %s, '%s');", doi, key_arg, input_id)
  webr::eval_js(js_code)
}

get_opencitations_count_async <- function(doi, opencitations_key = NULL, input_id = "api_count_result") {
  key_arg <- if(is.null(opencitations_key) || trimws(opencitations_key) == "") "null" else paste0("'", opencitations_key, "'")
  
  js_code <- sprintf("window.fetchCitationCount('opencitations', '%s', %s, '%s');", doi, key_arg, input_id)
  webr::eval_js(js_code)
}

get_semanticscholar_count_async <- function(doi, api_key = NULL, input_id = "api_count_result") {
  # Handle your global environment key logic in R before passing to JS
  if (is.null(api_key) && !is.null(glens_env$semantic_key) && !stringi::stri_isempty(glens_env$semantic_key)) {
    api_key <- glens_env$semantic_key
  }
  
  key_arg <- if(is.null(api_key) || trimws(api_key) == "") "null" else paste0("'", api_key, "'")
  
  js_code <- sprintf("window.fetchCitationCount('semanticscholar', '%s', %s, '%s');", doi, key_arg, input_id)
  webr::eval_js(js_code)
}

# Master function: tries multiple sources and returns a named list (counts may be NA)
get_citation_counts <- function(doi_or_url, semanticscholar_key = NULL, try_sources = c("crossref","opencitations","semanticscholar")) {
  
  doi <- normalize_doi(doi_or_url)
  
  # 1. Define a helper function to route the source to the correct API call
  fetch_source <- function(source) {
    Sys.sleep(1)
    if (source == "crossref") {
      return(tryCatch(get_crossref_count(doi), error = function(e) NA_integer_))
    } else if (source == "opencitations") {
      return(tryCatch(get_opencitations_count(doi), error = function(e) NA_integer_))
    } else if (source == "semanticscholar") {
      return(tryCatch(get_semanticscholar_count(doi, semanticscholar_key), error = function(e) NA_integer_))
    }
    return(NA_integer_)
  }
  
  # 2. Fire all requested API calls simultaneously using future_lapply
  # future.seed = TRUE suppresses warnings about random number generation in parallel environments
  res_list <- future.apply::future_lapply(try_sources, fetch_source, future.seed = TRUE)
  
  # 3. Name the resulting list elements to match the requested sources
  names(res_list) <- try_sources
  
  return(res_list)
}

get_citation_counts_async <- function(doi_or_url, 
                                      semanticscholar_key = NULL, 
                                      try_sources = c("crossref","opencitations","semanticscholar"), 
                                      input_id = "api_counts_ready",
                                      request_id = NULL) {
  
  doi <- normalize_doi(doi_or_url)
  
  # Format keys for JavaScript injection
  ss_key <- if (is.null(semanticscholar_key)) "null" else paste0("'", semanticscholar_key, "'")
  
  # Format array and request ID for JS
  sources_js <- paste0("['", paste(try_sources, collapse = "','"), "']")
  req_id_js <- if(is.null(request_id)) "null" else paste0("'", request_id, "'")
  
  # Construct the JS call
  js_code <- sprintf("
    window.fetchAllCitationCounts('%s', %s, {semanticscholar: %s}, '%s', %s);
  ", doi, sources_js, ss_key, input_id, req_id_js)
  
  # CRITICAL FIX: Use shinyjs instead of webr
  # This works perfectly in BOTH local RStudio testing and deployed WASM
  shinyjs::runjs(js_code)
}

# get_citation_counts_async <- function(doi_or_url, 
#                                       semanticscholar_key = NULL, 
#                                       try_sources = c("crossref","opencitations","semanticscholar"), 
#                                       input_id = "api_counts_ready",
#                                       request_id = NULL) {
#   
#   doi <- normalize_doi(doi_or_url)
#   
#   # Format keys for JavaScript injection
#   ss_key <- if (is.null(semanticscholar_key)) "null" else paste0("'", semanticscholar_key, "'")
#   
#   # Format array and request ID for JS
#   sources_js <- paste0("['", paste(try_sources, collapse = "','"), "']")
#   req_id_js <- if(is.null(request_id)) "null" else paste0("'", request_id, "'")
#   
#   # Construct the JS call
#   js_code <- sprintf("
#     window.fetchAllCitationCounts('%s', %s, {semanticscholar: %s}, '%s', %s);
#   ", doi, sources_js, ss_key, input_id, req_id_js)
#   
#   # Dispatch to the browser
#   webr::eval_js(js_code)
# }

get_title_from_ris <- function(ris){
  # cat(ris)
  ris_lines <- strsplit(ris, "\n")[[1]]
  title_keys <- c()
  type_of_reference <- gsub(paste0("^TY\\s+-\\s+"), "", x = grep(pattern = "TY", ris_lines,value = T))
  if(grepl(pattern= "CHAP|BOOK|GENERIC", x = type_of_reference)){
    title_keys <- c("BT", "T1", "T2", "TI")
  }else{
    title_keys <- c("BT", "TI", "T1", "T2") 
  }
  title_strings <- unique(na.omit(sapply(title_keys, function(title_key){
      title_str <- gsub(paste0("^",title_key,"\\s+-\\s+"), "", x = grep(pattern = paste0("^",title_key), ris_lines,value = T, , ignore.case = F)) #Extracting TI Title
      if(length(title_str) != 0){
        return(title_str)
      }else{
        return(NA)
      }
    }, simplify = T)))
  # print(title_strings)
  if(length(title_strings[1]) != 0){
    return(title_strings[1])
  }else{
    warning(paste("Title not available"))
    return("NA")  
  }
}

construct_journal_from_ris <- function(ris){
  #Generic 
  #JP = T2 OR JF
  #"JP VL (IS), SP-EP""
  ris_lines <- strsplit(ris, "\n")[[1]]
  # print(head(ris_lines, n=10))
  journal_keys <- c("JF", "T2", "JO", "PB") # "TI"
  journal_strings <- na.omit(sapply(journal_keys, function(journal_key){
    journal_str <- gsub(paste0("^",journal_key,"\\s+-\\s+"), "", x = grep(pattern = paste0("^",journal_key), ris_lines,value = T, ignore.case = F)) #Extracting T2 Journal Publisher
    if(length(journal_str) != 0){
      return(journal_str)
    }else{
      return(NA)
    }  
  }, simplify = T))
  # print(journal_strings)
  if(length(journal_strings) <= 0){
    return(NA)
  }
  vl <- gsub("^VL\\s+-\\s+", "", x = grep(pattern = "^VL", ris_lines,value = T, ignore.case = F)) #Extracting VL
  is <- gsub("^IS\\s+-\\s+", "", x = grep(pattern = "^IS", ris_lines,value = T, ignore.case = F)) #Extracting IS
  sp <- gsub("^SP\\s+-\\s+", "", x = grep(pattern = "^SP", ris_lines,value = T, ignore.case = F)) #Extracting SP Start Page
  ep <- gsub("^EP\\s+-\\s+", "", x = grep(pattern = "^EP", ris_lines,value = T, ignore.case = F)) #Extracting EP End Page
  pb <- gsub("^PB\\s+-\\s+", "", x = grep(pattern = "^PB", ris_lines,value = T, ignore.case = F)) #Extracting PB Publisher
  if(length(ep) > 0){
    journal_string <- paste(journal_strings[1], vl,paste("(",is,"),",sep=""),paste(sp,"-",ep,sep=""))
  }else if(length(sp) > 0 && length(is) > 0 && length(vl) > 0){
    journal_string <- paste(journal_strings[1], vl,paste("(",is,"),",sep=""), sp)
  }else if(length(is) > 0 && length(vl) > 0){
    journal_string <- paste(journal_strings[1], vl,paste("(",is,"),",sep=""))
  }else if(length(vl) > 0){
    journal_string <- paste(journal_strings[1], vl)
  }else if(length(journal_strings[1]) > 0){
    journal_string <- journal_strings[1]
  }else{
    journal_string <- "NA"
  }
  # else if(length(pb) > 0){
  #   print(str(pb))
  #   journal_string <- pb
  # }
  
  return(journal_string)
}

construct_author_list_from_ris <- function(ris){
  # print(ris)
  ris_lines <- strsplit(ris, "\n")[[1]]
  author_keys <- c("AU", "A1", "A2")
  author_strings <- dplyr::bind_rows(lapply(author_keys, function(author_key){
    author_list <- gsub(paste0("^",author_key,"\\s+-\\s+"), "", x = grep(pattern = paste0("^",author_key), ris_lines,value = T, ignore.case = F)) #Extracting Authors from AU lines
    # print(length(author_list))
    author_list_corrected <- unlist(lapply(strsplit(x = author_list,split = ", ", fixed = T), FUN=function(x){
      return(paste(x[2],x[1]))
    }))
    # print(author_list_corrected)
    if(length(author_list_corrected) > 0){
      return(data.frame(Authors=paste(author_list_corrected, collapse=", "), Author_Count=length(author_list_corrected)))
    }
    # else{
    #   return(NA)
    # }
  }))
  # print(author_strings)
  # print(na.omit(author_strings))
  if(nrow(author_strings) > 0){
    # return(paste(author_strings, collapse=","))
    return(author_strings)
  }else{
    return(NA)
  }
}

prepare_doi_metadata <- function(doi_input, orcid, rv, id_type = "DOI", write_file = NULL) {
  if(is.null(doi_input) || length(doi_input) == 0 || is.na(doi_input[1]) || stringi::stri_length(doi_input[1]) <= 0){
    warning("Empty DOI")
    return(NULL)
  }
  
  if(!fs::file_exists(file.path("run.lock"))) return(NULL)
  if(stringi::stri_isempty(doi_input)) return(NULL)
  
  ris <- NULL
  
  # If it is a true DOI, use your standard function
  if (id_type == "DOI") {
    ris <- extract_ris(doi_input, write_file = write_file)
  } else {
    # If it is NOT a DOI, pass it to our new translator!
    ris <- resolve_non_doi_ris(doi_input, id_type, scopus_key = glens_env$scopus_key)
  }
  
  # If BOTH extraction methods fail, abort row processing
  if(is.null(ris) || trimws(ris) == ""){
    rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'> RIS Extraction Failed for:", doi_input, "(", id_type, ")</span>"), sep="<br>")
    return(NULL)
  }
  
  ris_lines <- strsplit(ris, "\n")[[1]]
  journal_text <- construct_journal_from_ris(ris)
  
  publisher_text <- gsub(paste0("^PB\\s+-\\s+"), "", x = grep(pattern = "PB", ris_lines, value = T))
  publisher_text <- ifelse(length(publisher_text) > 0, publisher_text, NA)
  
  publisher_year_text <- gsub(paste0("^(PY|Y1|Y2)\\s+-\\s+"), "", x = grep(pattern = "PY|Y1|Y2", ris_lines, value = T))
  publisher_year_text <- ifelse(length(publisher_year_text) > 0, publisher_year_text, NA)
  
  author_list <- tidyr::tibble(na.omit(construct_author_list_from_ris(ris)))
  if(nrow(author_list) <= 0) return(NULL)
  
  author_text <- author_list$Authors 
  author_text <- ifelse(length(author_text) > 0, author_text, NA)
  title_text <- get_title_from_ris(ris)
  
  # Return the dataframe WITHOUT citations (we will add them asynchronously)
  return(data.frame(
    Title = title_text, 
    Authors = author_text,
    Author_Count = author_list$Author_Count, 
    Citations = NA_real_, # Placeholder
    Journal = journal_text, 
    Publisher = publisher_text, 
    Year = publisher_year_text,
    doi = trimws(doi_input),
    orcid = trimws(orcid)
  ))
}

# doi2gscholarlens <- function(doi_input, orcid, rv, write_file = NULL){
#   if(is.null(doi_input) || length(doi_input) == 0 || is.na(doi_input[1]) || stringi::stri_length(doi_input[1]) <= 0){
#     warning("Empty DOI")
#     return(NULL)
#   }
#   
#   # message(doi_input)
#   # message(orcid)
#   # message(str(doi_input))
#   # message(str(orcid))
#   # message(class(doi_input))
#   # message(class(orcid))
#   # # message(paste("glens_env:"))
#   # # message(glens_env$scopus_key)
#   
#   # if (rv$is_cancelled) return(NULL)
#   if(!fs::file_exists(file.path("run.lock"))) return(NULL)
#   if(stringi::stri_isempty(doi_input)){
#     return(NULL)
#   }
#   
#   ris <- extract_ris(doi_input, write_file = write_file)
#   if(is.null(ris)){
#     rv$log_text <- paste(rv$log_text, paste("<span  style='color: red;'> RIS Extraction Failed for:",doi_input,"</span>"),sep="<br>")
#     return(NULL)
#   }
#   ris_lines <- strsplit(ris, "\n")[[1]]
#   
#   journal_text <- construct_journal_from_ris(ris)
#   
#   publisher_text <- gsub(paste0("^PB\\s+-\\s+"), "", x = grep(pattern = "PB", ris_lines,value = T))
#   publisher_text <- ifelse(length(publisher_text) > 0,publisher_text, NA)
#   
#   publisher_year_text <- gsub(paste0("^(PY|Y1|Y2)\\s+-\\s+"), "", x = grep(pattern = "PY|Y1|Y2", ris_lines,value = T))
#   publisher_year_text <- ifelse(length(publisher_year_text) > 0,publisher_year_text, NA)
#   
#   author_list <- tidyr::tibble(na.omit(construct_author_list_from_ris(ris)))
#   
#   if(nrow(author_list) <=0){
#     return(NULL)
#   }
#   
#   author_text <- author_list$Authors 
#   author_text <- ifelse(length(author_text) > 0, author_text, NA)
#   title_text <- get_title_from_ris(ris)
#   # message(doi_input)
#   doi_citations <- get_citation_counts(doi_input)
#   
#   # Safely extract valid citations using standard base logic
#   valid_citations <- na.omit(unlist(doi_citations))
#   max_cit <- if (length(valid_citations) == 0) 0 else as.numeric(max(valid_citations))
#   
#   return(data.frame(
#     Title = title_text, 
#     Authors = author_text,
#     Author_Count = author_list$Author_Count, 
#     Citations = max_cit, 
#     Journal = journal_text, 
#     Publisher = publisher_text, 
#     Year = publisher_year_text,
#     doi = trimws(doi_input),
#     orcid = trimws(orcid)
#   ))
#   
#   # doi_lines_input <- strsplit(doi_input, "\n")[[1]]
#   #
#   # return(dplyr::bind_rows(lapply(doi_lines_input, function(doi_line){
#   #   # if (rv$is_cancelled) return(NULL)
#   #   if(!fs::file_exists(file.path("run.lock"))) return(NULL)
#   #   if(stringi::stri_isempty(doi_line)){
#   #     return(NULL)
#   #   }
#   #   
#   #   ris <- extract_ris(doi_line, write_file = write_file)
#   #   ris_lines <- strsplit(ris, "\n")[[1]]
#   #   
#   #   journal_text <- construct_journal_from_ris(ris)
#   #   
#   #   publisher_text <- gsub(paste0("^PB\\s+-\\s+"), "", x = grep(pattern = "PB", ris_lines,value = T))
#   #   publisher_text <- ifelse(length(publisher_text) > 0,publisher_text, NA)
#   #   
#   #   publisher_year_text <- gsub(paste0("^(PY|Y1|Y2)\\s+-\\s+"), "", x = grep(pattern = "PY|Y1|Y2", ris_lines,value = T))
#   #   publisher_year_text <- ifelse(length(publisher_year_text) > 0,publisher_year_text, NA)
#   #   
#   #   author_list <- tidyr::tibble(na.omit(construct_author_list_from_ris(ris)))
#   #   
#   #   if(nrow(author_list) <=0){
#   #     return(NULL)
#   #   }
#   #   
#   #   author_text <- author_list$Authors 
#   #   author_text <- ifelse(length(author_text) > 0, author_text, NA)
#   #   title_text <- get_title_from_ris(ris)
#   #   # message(doi_line)
#   #   doi_citations <- get_citation_counts(doi_line)
#   #   
#   #   # Safely extract valid citations using standard base logic
#   #   valid_citations <- na.omit(unlist(doi_citations))
#   #   max_cit <- if (length(valid_citations) == 0) 0 else as.numeric(max(valid_citations))
#   #   
#   #   return(data.frame(
#   #     Title = title_text, 
#   #     Authors = author_text,
#   #     Author_Count = author_list$Author_Count, 
#   #     Citations = max_cit, 
#   #     Journal = journal_text, 
#   #     Publisher = publisher_text, 
#   #     Year = publisher_year_text,
#   #     doi = trimws(doi_line),
#   #     orcid = trimws(orcid)
#   #   ))
#   # })))
# }

# # If run as script with args, use them
# args <- commandArgs(trailingOnly = TRUE)
# if (length(args) >= 1) {
#   doi_list_file <- args[1]
#   output_xlsx <- if (length(args) >= 2) args[2] else NULL
#   if(file.exists(doi_list_file)){
#     doi_list <- readLines(doi_list_file)
#     if(length(doi_list) <= 0){
#         stop(paste(doi_list_file,"empty or unknown format"))
#     }
#     lens_format_table <- dplyr::bind_rows(sapply(doi_list, function(x){
#       doi2gscholarlens(x)  
#     }, simplify = F))
#     if(!is.null(output_xlsx)){
#       #write.csv(lens_format_table, output_csv, row.names = FALSE)
#       openxlsx::write.xlsx(lens_format_table, output_xlsx)
#     }else{
#       print(lens_format_table)
#     }
#   }else{
#     stop(paste(doi_list_file,"does not exist"))
#   }
#   
# } else {
#   # interactive example
#   message("Usage: Rscript GScholarLENS-DOI2Data.R \"<doi-list-file>\" [output_file.xlsx]")
#   # example (uncomment to test interactively):
#   # example_doi <- "https://doi.org/10.1038/s41586-020-2649-2"
#   # cat(extract_ris(example_doi))
# }
