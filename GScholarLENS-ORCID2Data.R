suppressPackageStartupMessages(require(xml2))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(purrr))
suppressPackageStartupMessages(require(tibble))


xtext <- function(node, xpath, ns) {
  x <- xml2::xml_find_first(node, xpath, ns)
  if (length(x) == 0) NA_character_ else xml2::xml_text(x)
}


# ---- P10-A: ORCID works fetch/parse, moved out of server.R (was inline in the fetch observer's future) -----------
# Pure functions (no Shiny): server.R only wires them into the future/promise and the progress bar. The previous
# inline behaviour is kept exactly, incl. its quirks (a transport error leaves the body empty -> "Empty ORCID response").
cn_orcid_is_wellformed <- function(orcid) length(strsplit(orcid, "-")[[1]]) == 4
cn_orcid_works_url <- function(orcid) paste0("https://pub.orcid.org/v3.0/", orcid, "/works")

# Fetch the /works XML. Returns the body text ("" on a transport error) or a list(df, error) on a non-200 response.
cn_orcid_fetch_works <- function(clean_orcid, is_wasm = grepl(pattern = "wasm", x = Sys.info()["machine"])) {
  target_url <- cn_orcid_works_url(clean_orcid)
  res_text <- ""
  tryCatch({
    if (is_wasm) {
      con <- url(target_url, headers = c(Accept = "application/xml"))
      res_text <- paste(readLines(con, warn = FALSE), collapse = "\n")
      close(con)
    } else {
      # httr because ORCID blocks base R user-agents
      resp <- httr::GET(target_url, httr::add_headers(Accept = "application/xml"))
      if (httr::status_code(resp) == 200) {
        res_text <- httr::content(resp, as = "text", encoding = "UTF-8")
      } else {
        return(list(df = data.frame(), error = paste("ORCID API returned HTTP", httr::status_code(resp))))
      }
    }
  }, error = function(e) {
    if (exists("con")) try(close(con), silent = TRUE)
    return(list(df = data.frame(), error = e$message))
  })
  res_text
}

# Parse an ORCID v3.0 /works XML body into one row per work-summary (namespace-agnostic via local-name()).
cn_orcid_parse_works <- function(res_text, clean_orcid) {
  xml_vec <- xml2::read_xml(res_text)
  xml_summaries <- xml2::xml_find_all(xml_vec, "//*[local-name()='work-summary']")
  if (length(xml_summaries) == 0) return(data.frame())
  xtext_safe <- function(node, xpath) {
    val <- xml2::xml_text(xml2::xml_find_first(node, xpath))
    if (is.na(val) || trimws(val) == "") return(NA_character_) else return(trimws(val))
  }
  purrr::map_dfr(xml_summaries, function(w) {
    ext_id_nodes <- xml2::xml_find_all(w, ".//*[local-name()='external-id']")
    ext_id_val <- NA_character_
    ext_id_url <- NA_character_
    if (length(ext_id_nodes) > 0) {
      id_types <- sapply(ext_id_nodes, function(x) xtext_safe(x, ".//*[local-name()='external-id-type']"))
      id_vals <- sapply(ext_id_nodes, function(x) xtext_safe(x, ".//*[local-name()='external-id-value']"))
      id_urls <- sapply(ext_id_nodes, function(x) xtext_safe(x, ".//*[local-name()='external-id-url']"))
      doi_idx <- which(tolower(id_types) == "doi")   # prefer the DOI; else the first external id
      if (length(doi_idx) > 0) {
        ext_id_val <- id_vals[doi_idx[1]]
        ext_id_url <- id_urls[doi_idx[1]]
      } else {
        ext_id_val <- id_vals[1]
        ext_id_url <- id_urls[1]
      }
    }
    title_val <- xtext_safe(w, ".//*[local-name()='title']/*[local-name()='title']")
    if (is.na(title_val)) title_val <- xtext_safe(w, ".//*[local-name()='title']")
    tibble::tibble(
      source_name = xtext_safe(w, ".//*[local-name()='source-name']"),
      title = title_val,
      external_id_value = ext_id_val,
      external_id_url = ext_id_url,
      journal_title = xtext_safe(w, ".//*[local-name()='journal-title']"),
      work_type = xtext_safe(w, ".//*[local-name()='type']"),
      pub_year = xtext_safe(w, ".//*[local-name()='publication-date']/*[local-name()='year']"),
      orcid = paste0("https://orcid.org/", clean_orcid)
    )
  })
}

# Whole per-ORCID step as run inside the future: list(df, error).
cn_orcid_works <- function(clean_orcid) {
  if (!cn_orcid_is_wellformed(clean_orcid)) return(list(error = "Malformed ORCID"))
  res_text <- cn_orcid_fetch_works(clean_orcid)
  if (is.list(res_text)) return(res_text)
  if (trimws(res_text) == "") return(list(df = data.frame(), error = "Empty ORCID response"))
  list(df = cn_orcid_parse_works(res_text, clean_orcid), error = NULL)
}

# ---- P10-A6: ORCID holder's name -> lookup keywords -------------------------------------------------------------
# With only an ORCID and no lookup keywords, extend_input_table() takes its "no keywords" bypass and every
# First/Second/Co/Corresponding count is 0: the app cannot tell which author on each paper is the ORCID holder.
# These pure functions fetch the holder's names and turn them into keywords that the app's EXISTING matchers hit:
#  - publication filter apply_author_logic(): literal, case-insensitive, word-bounded regex on the raw author
#    string ("Gaurav Sharma, A. Kumar") -> needs literal forms such as "G. Sharma", "G Sharma", "Sharma G";
#  - position scorer score_match() on normalize_name() tokens (lowercase, dots dropped, "First Last" order after
#    split_authors()): exact full name, first+last, or first initial+last -> "Given Family" already covers
#    "G. Family" / "Given X. Family"; surname-first tokens left unflipped ("sharma g") need the exact "Family G".
# Never generated: bare surname, bare initials, "Family, G." (would hit "X Family, G Other" in comma-joined lists).

cn_orcid_person_url <- function(orcid) paste0("https://pub.orcid.org/v3.0/", orcid, "/person")

# GET a JSON body with the same transport as cn_orcid_fetch_works(): base url() under webR (pub.orcid.org sends
# Access-Control-Allow-Origin: * on /person as on /works), httr natively (ORCID blocks base R user-agents).
# Returns the body text, or NULL on any failure.
cn_http_get_json <- function(target_url, is_wasm = grepl(pattern = "wasm", x = Sys.info()["machine"])) {
  tryCatch({
    if (is_wasm) {
      con <- url(target_url, headers = c(Accept = "application/json"))
      on.exit(try(close(con), silent = TRUE), add = TRUE)
      paste(readLines(con, warn = FALSE), collapse = "\n")
    } else {
      resp <- httr::GET(target_url, httr::add_headers(Accept = "application/json"), httr::timeout(20))
      if (httr::status_code(resp) == 200) httr::content(resp, as = "text", encoding = "UTF-8") else NULL
    }
  }, error = function(e) NULL)
}

# ORCID /person JSON -> list(given, family, credit, other). Private/absent parts are "" / character(0).
cn_orcid_parse_person <- function(txt) {
  empty <- list(given = "", family = "", credit = "", other = character(0))
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(j)) return(empty)
  val <- function(x) { v <- x[["value"]]; if (is.null(v)) "" else trimws(as.character(v)) }
  n <- j[["name"]]
  on <- j[["other-names"]][["other-name"]]; if (!is.list(on)) on <- list()
  other <- vapply(on, function(o) if (is.null(o[["content"]])) "" else trimws(as.character(o[["content"]])), "")
  list(given = if (is.list(n)) val(n[["given-names"]]) else "", family = if (is.list(n)) val(n[["family-name"]]) else "",
       credit = if (is.list(n)) val(n[["credit-name"]]) else "", other = other[nzchar(other)])
}

# OpenAlex fallback: /authors/orcid:{id} -> c(display_name, display_name_alternatives). Same key/mailto handling
# as fetch_journal_metrics_openalex(): api_key if set, else OPENALEX_MAILTO.
cn_openalex_author_names <- function(orcid, api_key = "", fetch = cn_http_get_json) {
  u <- paste0("https://api.openalex.org/authors/orcid:", orcid, "?select=display_name,display_name_alternatives")
  mailto <- Sys.getenv("OPENALEX_MAILTO", unset = "")
  if (!is.null(api_key) && nzchar(api_key)) u <- paste0(u, "&api_key=", api_key)
  else if (nzchar(mailto)) u <- paste0(u, "&mailto=", utils::URLencode(mailto, reserved = TRUE))
  txt <- fetch(u)
  if (is.null(txt)) return(character(0))
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(j)) return(character(0))
  x <- trimws(c(j$display_name, unlist(j$display_name_alternatives)))
  unique(x[!is.na(x) & nzchar(x)])
}

# Final fallback: names of the authors whose ORCID equals the iD in already-fetched works. Needs per-author ORCIDs
# aligned with Authors (column Author_ORCIDs, same separator); the current DOI->RIS path does not provide them.
cn_works_names_for_orcid <- function(works, orcid, sep = ",") {
  if (!is.data.frame(works) || !all(c("Authors", "Author_ORCIDs") %in% names(works))) return(character(0))
  out <- character(0)
  for (i in seq_len(nrow(works))) {
    a <- trimws(strsplit(as.character(works$Authors[i]), sep, fixed = TRUE)[[1]])
    o <- trimws(strsplit(as.character(works$Author_ORCIDs[i]), sep, fixed = TRUE)[[1]])
    if (length(a) != length(o)) next
    out <- c(out, a[cn_orcid_bare(o) == cn_orcid_bare(orcid)])
  }
  unique(out[!is.na(out) & nzchar(out)])
}
cn_orcid_bare <- function(x) toupper(sub("^https?://(www\\.)?orcid\\.org/", "", trimws(as.character(x))))

cn_ascii <- function(x) {
  y <- if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_trans_general(x, "Latin-ASCII") else iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "")
  ifelse(is.na(y), x, y)
}

# Name parts -> keyword variants (see header). given/family may be "" (private); credit/other are free text,
# split as "everything but the last word" + last word unless written "Family, Given".
cn_orcid_name_variants <- function(given = "", family = "", credit = "", other = character(0), max_n = 24L) {
  split_free <- function(s) {
    s <- trimws(s); if (!nzchar(s)) return(NULL)
    if (grepl(",", s, fixed = TRUE)) { p <- trimws(strsplit(s, ",", fixed = TRUE)[[1]]); return(list(given = paste(p[-1], collapse = " "), family = p[1])) }
    w <- strsplit(s, "\\s+")[[1]]; if (length(w) < 2) return(NULL)
    list(given = paste(w[-length(w)], collapse = " "), family = w[length(w)])
  }
  forms <- function(g, f) {
    g <- trimws(gsub("\\s+", " ", g)); f <- trimws(gsub("\\s+", " ", f))
    if (!nzchar(g) || !nzchar(f)) return(character(0))
    gw <- strsplit(g, "[\\s-]+", perl = TRUE)[[1]]; gw <- gw[nzchar(gw)]
    ini <- toupper(substr(gsub("[^[:alpha:]]", "", gw), 1, 1)); ini <- ini[nzchar(ini)]
    if (!length(ini)) return(character(0))
    i_dot <- paste0(ini, ".", collapse = " "); i_dot2 <- paste0(ini, ".", collapse = ""); i_sp <- paste(ini, collapse = " "); i_cat <- paste(ini, collapse = "")
    full_given <- any(nchar(gsub("[^[:alpha:]]", "", gw)) >= 2)                 # "Family, G." never (see header)
    v <- c(paste(g, f), if (full_given) paste0(f, ", ", g),
           paste(i_dot, f), paste(i_dot2, f), paste(i_sp, f), paste(i_cat, f),
           paste(f, i_cat), paste(f, i_sp), paste(f, i_dot2))
    if (length(gw) > 1 && nchar(gsub("[^[:alpha:]]", "", gw[1])) >= 2) v <- c(v, paste(gw[1], paste0(ini[-1], ".", collapse = " "), f), paste(gw[1], f))   # "Om P. Nanda", "Om Nanda"
    if (grepl("-", g, fixed = TRUE)) v <- c(v, paste(gsub("-", " ", g, fixed = TRUE), f),
                                              paste(paste0(ini, ".", collapse = "-"), f))                         # "Jean Pierre X", "J.-P. X"
    if (grepl("[- ]", f)) { f2 <- if (grepl("-", f, fixed = TRUE)) gsub("-", " ", f, fixed = TRUE) else gsub(" ", "-", f, fixed = TRUE)
      v <- c(v, paste(g, f2), paste(i_dot, f2), paste(f2, i_cat)) }
    v
  }
  parts <- list(list(given = given, family = family), split_free(credit))
  for (o in other) parts <- c(parts, list(split_free(o)))
  # priority order: each name's forms followed by their diacritic-stripped forms; capped at max_n
  v <- unlist(lapply(Filter(Negate(is.null), parts), function(p) { f <- forms(p$given, p$family); c(f, cn_ascii(f)) }))
  v <- trimws(gsub("\\s+", " ", v))
  # never a single word or initials only
  v <- v[vapply(strsplit(gsub("[.,]", " ", v), "\\s+"), function(w) { w <- w[nzchar(w)]; length(w) >= 2 && any(nchar(w) >= 3) }, logical(1))]
  v <- v[!duplicated(tolower(v))]
  utils::head(v, max_n)
}

# Merge new keywords into the user's newline-separated list: user lines kept first and untouched, new ones appended,
# case-insensitive dedupe. Returns list(text, added).
cn_merge_keywords <- function(existing_text, new) {
  ex <- trimws(strsplit(if (is.null(existing_text)) "" else existing_text, "\n", fixed = TRUE)[[1]]); ex <- ex[nzchar(ex)]
  new <- trimws(new); new <- new[nzchar(new)]
  add <- new[!duplicated(tolower(new)) & !(tolower(new) %in% tolower(ex))]
  list(text = paste(c(ex, add), collapse = "\n"), added = add)
}

# One iD -> list(name, source, variants). cache: an environment (one entry per iD per session = one request per iD).
cn_orcid_holder_names <- function(orcid, cache = NULL, openalex_key = "", works = NULL,
                                  fetch = cn_http_get_json) {
  id <- cn_orcid_bare(orcid)
  if (!is.null(cache) && exists(id, envir = cache, inherits = FALSE)) return(get(id, envir = cache))
  p <- list(given = "", family = "", credit = "", other = character(0))
  txt <- fetch(cn_orcid_person_url(id))
  if (!is.null(txt)) p <- cn_orcid_parse_person(txt)
  name <- trimws(paste(p$given, p$family)); src <- "ORCID"
  v <- cn_orcid_name_variants(p$given, p$family, p$credit, p$other)
  if (!length(v) || !nzchar(p$family)) {
    oa <- cn_openalex_author_names(id, openalex_key, fetch = fetch)
    if (length(oa)) { name <- oa[1]; src <- "OpenAlex"; v <- unique(c(v, unlist(lapply(oa, function(s) cn_orcid_name_variants(credit = s))))) }
  }
  if (!length(v)) {
    w <- cn_works_names_for_orcid(works, id)
    if (length(w)) { name <- w[1]; src <- "works"; v <- unlist(lapply(w, function(s) cn_orcid_name_variants(credit = s))) }
  }
  v <- v[!duplicated(tolower(v))]
  res <- list(name = if (length(v)) name else "", source = if (length(v)) src else "none", variants = v)
  if (!is.null(cache)) assign(id, res, envir = cache)
  res
}

# ORCID-first author position: 1-based index of the author whose ORCID equals the iD, NA if none. Used when works
# carry per-author ORCIDs (Author_ORCIDs aligned with Authors); otherwise name keywords decide (method "name").
cn_position_by_orcid <- function(authors, author_orcids, orcid, sep = ",") {
  vapply(seq_along(authors), function(i) {
    if (is.na(author_orcids[i]) || !nzchar(author_orcids[i])) return(NA_integer_)
    o <- cn_orcid_bare(strsplit(as.character(author_orcids[i]), sep, fixed = TRUE)[[1]])
    k <- which(o == cn_orcid_bare(orcid)); if (length(k)) as.integer(k[1]) else NA_integer_
  }, integer(1))
}

# Per-work method that decided the author position: "orcid" (Position_Method set by an ORCID match), "name" (a
# lookup keyword matched a token) or "none". Returns c(orcid, name, none) counts.
cn_position_method_totals <- function(df) {
  lab <- as.character(df$label)
  meth <- if ("Position_Method" %in% names(df)) as.character(df$Position_Method) else rep(NA_character_, nrow(df))
  unmatched <- is.na(lab) | lab %in% c("Not_found", "Unmatched", "No_Filter", "")
  m <- ifelse(!is.na(meth) & meth == "orcid", "orcid", ifelse(unmatched, "none", "name"))
  c(orcid = sum(m == "orcid"), name = sum(m == "name"), none = sum(m == "none"))
}
