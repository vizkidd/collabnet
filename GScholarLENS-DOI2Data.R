require(httr)
require(jsonlite)
require(stringi)
require(dplyr)
require(openxlsx)

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
    GET(url,
        add_headers(Accept = "application/x-research-info-systems"),
        user_agent(user_agent_str),
        timeout(timeout_secs))
  }
  
  # 1) Try doi.org content negotiation
  doi_url <- paste0("https://doi.org/", doi_enc)
  res <- tryCatch(get_with_ris_accept(doi_url), error = function(e) e)
  
  ris_text <- NULL
  if (inherits(res, "response") && status_code(res) == 200) {
    # extract as text
    txt <- content(res, as = "text", encoding = "UTF-8")
    # quick sanity check: RIS often starts with "TY  - "
    if (nzchar(txt) && grepl("^TY  - |^TY - ", txt)) {
      ris_text <- txt
    } else {
      # some servers may return RIS but without the typical header; still accept non-empty text
      if (nzchar(txt)) ris_text <- txt
    }
  }
  
  # 2) Fallback: CrossRef transform endpoint: /works/{doi}/transform/application/x-research-info-systems
  if (is.null(ris_text)) {
    crossref_url <- paste0("https://api.crossref.org/works/", doi_enc,
                           "/transform/application/x-research-info-systems")
    res2 <- tryCatch(get_with_ris_accept(crossref_url), error = function(e) e)
    if (inherits(res2, "response") && status_code(res2) == 200) {
      txt2 <- content(res2, as = "text", encoding = "UTF-8")
      if (nzchar(txt2)) ris_text <- txt2
    }
  }
  
  if (is.null(ris_text)) {
    stop("Failed to retrieve RIS. The DOI may not support content negotiation and CrossRef doesn't have a transform for it.")
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

normalize_doi <- function(doi_or_url) {
  doi <- trimws(doi_or_url)
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", doi, ignore.case = TRUE)
  doi <- sub("^doi:", "", doi, ignore.case = TRUE)
  doi <- trimws(doi)
  if (doi == "") stop("Couldn't parse DOI.")
  doi
}

get_crossref_count <- function(doi) {
  doi_e <- URLencode(doi, reserved = TRUE)
  url <- paste0("https://api.crossref.org/works/", doi_e)
  res <- tryCatch(GET(url, user_agent("R (httr)")), error = function(e) NULL)
  if (is.null(res) || status_code(res) != 200) return(NA_integer_)
  j <- fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  # # Crossref uses "is-referenced-by-count"
  # print(paste("str(j): ", str(j)))
  # print(paste("j: ", j))
  # print(paste("is-referenced-by-count: ", j$message$`is-referenced-by-count`))
  # print(paste("reference-count: ", j$message$`reference-count`))
  # print(paste("references-count: ", j$message$`references-count`))
  cnt <- j$message$`is-referenced-by-count`
  if (is.null(cnt)) return(NA_integer_) else return(as.integer(cnt))
}

get_opencitations_count <- function(doi) {
  doi_e <- URLencode(doi, reserved = TRUE)
  # OpenCitations unified endpoint (index/v1/citation-count/{doi})
  url <- paste0("https://api.opencitations.net/index/v1/citation-count/", doi_e)
  res <- tryCatch(GET(url, user_agent("R (httr)")), error = function(e) NULL)
  if (is.null(res) || status_code(res) != 200) return(NA_integer_)
  txt <- content(res, as = "text", encoding = "UTF-8")
  # The API may return JSON array/object; try parsing robustly
  j <- tryCatch(fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(j)) return(NA_integer_)
  # shape may be list(count = N) or array [{"count":N}] etc.
  if (!is.null(j$count)) return(as.integer(j$count))
  if (is.data.frame(j) && "count" %in% names(j)) return(as.integer(j$count[1]))
  if (is.list(j) && length(j) >= 1 && !is.null(j[[1]]$count)) return(as.integer(j[[1]]$count))
  return(NA_integer_)
}

get_semanticscholar_count <- function(doi, api_key = NULL) {
  doi_e <- URLencode(doi, reserved = TRUE)
  url <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", doi_e, "?fields=citationCount")
  hdrs <- add_headers()
  if (!is.null(api_key)) hdrs <- add_headers(Authorization = paste("Bearer", api_key))
  res <- tryCatch(GET(url, hdrs, user_agent("R (httr)")), error = function(e) NULL)
  if (is.null(res)) return(NA_integer_)
  if (status_code(res) == 200) {
    j <- fromJSON(content(res, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
    if (!is.null(j$citationCount)) return(as.integer(j$citationCount))
  }
  return(NA_integer_)
}

# Master function: tries multiple sources and returns a named list (counts may be NA)
get_citation_counts <- function(doi_or_url, semanticscholar_key = NULL, try_sources = c("crossref","opencitations","semanticscholar")) {
  doi <- normalize_doi(doi_or_url)
  # res <- list(doi = doi)
  res <- list()
  if ("crossref" %in% try_sources) res$crossref <- tryCatch(get_crossref_count(doi), error = function(e) NA_integer_)
  if ("opencitations" %in% try_sources) res$opencitations <- tryCatch(get_opencitations_count(doi), error = function(e) NA_integer_)
  if ("semanticscholar" %in% try_sources) res$semanticscholar <- tryCatch(get_semanticscholar_count(doi, semanticscholar_key), error = function(e) NA_integer_)
  return(res)
}

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

doi2gscholarlens <- function(doi_input, write_file = NULL){
  if(is.null(doi_input) || stringi::stri_length(doi_input) <= 0){
    warning("Empty DOI")
    return(NULL)
  }
  doi_lines_input <- strsplit(doi_input, "\n")[[1]]
  return(dplyr::bind_rows(sapply(doi_lines_input, function(doi_line){
    if(stringi::stri_isempty(doi_line)){
      return()
    }
    ris <- extract_ris(doi_line, write_file = write_file)
    ris_lines <- strsplit(ris, "\n")[[1]]
    # cat(ris)
    # print(head(strsplit(ris, "\n")[[1]], n=10))
    journal_text <- construct_journal_from_ris(ris)
    publisher_text <- gsub(paste0("^PB\\s+-\\s+"), "", x = grep(pattern = "PB", ris_lines,value = T))
    publisher_text <- ifelse(length(publisher_text) > 0,publisher_text, NA)
    publisher_year_text <- gsub(paste0("^(PY|Y1|Y2)\\s+-\\s+"), "", x = grep(pattern = "PY|Y1|Y2", ris_lines,value = T))
    publisher_year_text <- ifelse(length(publisher_year_text) > 0,publisher_year_text, NA)
    #Extracting T2 Journal Publisher
    author_list <- tidyr::tibble(na.omit(construct_author_list_from_ris(ris)))
    # print(str(author_list))
    # print(author_list)
    if(nrow(author_list) <=0){
      # warning("Author list is empty.")
      return()
    }
    author_text <- author_list$Authors #paste(author_list, collapse = ", ")
    author_text <- ifelse(length(author_text) > 0, author_text, NA)
    # print(paste(author_list, collapse=", "))
    title_text <- get_title_from_ris(ris)
    doi_citations <- get_citation_counts(doi_line)
    # print(title_text)
    # print(journal_text)
    # print(doi_citations)
    # print("---------")
    # print(unlist(doi_citations))
    # print("======")
    # print(na.omit(unlist(doi_citations)))
    # print(max(as.numeric(unlist(doi_citations))))
    return(data.frame(Title=title_text, Authors=author_text,Author_Count=author_list$Author_Count, Citations=as.numeric(max(na.omit(unlist(doi_citations)))),Journal=journal_text, Publisher=publisher_text, Year=publisher_year_text))
    # return(data.frame(Title=title_text, Authors=author_list, Citations=max(as.numeric(na.omit(unlist(doi_citations)))),Journal=journal_text, Publisher=publisher_text))
  }, simplify = F)))
}

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
