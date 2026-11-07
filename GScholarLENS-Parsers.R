# GScholarLENS-Parsers.R: P2-1 import parsers (base R only; webR-safe). Each returns a data.frame with the columns
# CollabNET maps by default: Authors ("; "-separated, "Surname, Initials" as in the source), Title, Year, DOI,
# Source, Citations (NA when absent), plus Document_Type where available.

.cn_rows_to_df <- function(recs) {
  cols <- c("Authors", "Title", "Year", "DOI", "Source", "Citations", "Document_Type")
  if (!length(recs)) return(as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols), stringsAsFactors = FALSE))
  out <- as.data.frame(do.call(rbind, lapply(recs, function(r) vapply(cols, function(k) { v <- r[[k]]; if (is.null(v) || !length(v)) NA_character_ else as.character(v[1]) }, ""))),
                       stringsAsFactors = FALSE)
  out$Year <- suppressWarnings(as.integer(out$Year)); out$Citations <- suppressWarnings(as.numeric(out$Citations))
  out
}
.cn_read_text <- function(path) {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(x)) x[1] <- sub("^﻿", "", x[1])
  x
}

# --- RIS (EndNote / Zotero / Scopus RIS) -----------------------------------------------------------------------
cn_parse_ris <- function(path) {
  x <- .cn_read_text(path)
  recs <- list(); cur <- list(au = character(0))
  for (ln in x) {
    m <- regmatches(ln, regexec("^([A-Z][A-Z0-9])  - ?(.*)$", ln))[[1]]
    if (!length(m)) next
    tag <- m[2]; val <- trimws(m[3])
    if (tag == "TY") cur <- list(au = character(0), Document_Type = val)
    else if (tag %in% c("AU", "A1")) cur$au <- c(cur$au, val)
    else if (tag %in% c("TI", "T1") && is.null(cur$Title)) cur$Title <- val
    else if (tag %in% c("PY", "Y1") && is.null(cur$Year)) cur$Year <- sub("^([0-9]{4}).*$", "\\1", val)
    else if (tag == "DA" && is.null(cur$Year)) cur$Year <- sub("^([0-9]{4}).*$", "\\1", val)
    else if (tag == "DO") cur$DOI <- val
    else if (tag %in% c("T2", "JO", "JF", "JA") && is.null(cur$Source)) cur$Source <- val
    else if (tag == "ER") { cur$Authors <- paste(cur$au, collapse = "; "); recs[[length(recs) + 1]] <- cur; cur <- list(au = character(0)) }
  }
  .cn_rows_to_df(recs)
}

# TeX accent commands -> Unicode (common Latin cases), so BibTeX names match their Unicode spellings (P1-1).
.cn_tex_accents <- function(v) {
  combining <- c('"' = "\u0308", "'" = "\u0301", "`" = "\u0300", "^" = "\u0302", "~" = "\u0303")
  for (k in names(combining)) {
    rx <- paste0("\\\\", if (k == "^") "\\^" else k, "\\{?([A-Za-z])\\}?")
    v <- gsub(rx, paste0("\\1", combining[[k]]), v, perl = TRUE)
  }
  v <- gsub("\\\\c\\s*\\{?([A-Za-z])\\}?", "\\1\u0327", v, perl = TRUE)
  v <- gsub("\\\\ss\\b\\s*", "\u00df", v, perl = TRUE)
  # compose the common precomposed forms (webR has no stringi::stri_trans_nfc; cover Latin-1 vowels and c/n)
  base <- c("a", "e", "i", "o", "u", "A", "E", "I", "O", "U")
  for (b in base) {
    v <- gsub(paste0(b, "\u0308"), switch(b, a = "\u00e4", e = "\u00eb", i = "\u00ef", o = "\u00f6", u = "\u00fc", A = "\u00c4", E = "\u00cb", I = "\u00cf", O = "\u00d6", U = "\u00dc"), v, fixed = TRUE)
    v <- gsub(paste0(b, "\u0301"), switch(b, a = "\u00e1", e = "\u00e9", i = "\u00ed", o = "\u00f3", u = "\u00fa", A = "\u00c1", E = "\u00c9", I = "\u00cd", O = "\u00d3", U = "\u00da"), v, fixed = TRUE)
    v <- gsub(paste0(b, "\u0300"), switch(b, a = "\u00e0", e = "\u00e8", i = "\u00ec", o = "\u00f2", u = "\u00f9", A = "\u00c0", E = "\u00c8", I = "\u00cc", O = "\u00d2", U = "\u00d9"), v, fixed = TRUE)
    v <- gsub(paste0(b, "\u0302"), switch(b, a = "\u00e2", e = "\u00ea", i = "\u00ee", o = "\u00f4", u = "\u00fb", A = "\u00c2", E = "\u00ca", I = "\u00ce", O = "\u00d4", U = "\u00db"), v, fixed = TRUE)
  }
  v <- gsub("n\u0303", "\u00f1", v, fixed = TRUE); v <- gsub("c\u0327", "\u00e7", v, fixed = TRUE)
  gsub("[\u0300-\u036f]", "", v, perl = TRUE)          # drop any remaining combining marks
}

# --- BibTeX -----------------------------------------------------------------------------------------------------
cn_parse_bibtex <- function(path) {
  txt <- paste(.cn_read_text(path), collapse = "\n")
  starts <- gregexpr("@[A-Za-z]+\\s*\\{", txt)[[1]]
  if (starts[1] < 0) return(.cn_rows_to_df(list()))
  ends <- c(starts[-1] - 1L, nchar(txt))
  field <- function(body, name) {
    m <- regexpr(paste0("(?i)\\b", name, "\\s*=\\s*"), body, perl = TRUE)
    if (m < 0) return(NULL)
    rest <- substring(body, m + attr(m, "match.length"))
    ch <- substr(rest, 1, 1)
    if (ch %in% c("{", "\"")) {                      # brace- or quote-delimited, nested braces allowed
      depth <- 0L; close <- if (ch == "{") "}" else "\""; i <- 1L; n <- nchar(rest); chars <- strsplit(rest, "")[[1]]
      for (i in seq_len(n)) {
        c <- chars[i]
        if (ch == "{" && c == "{") depth <- depth + 1L
        else if (ch == "{" && c == "}") { depth <- depth - 1L; if (depth == 0L) break }
        else if (ch == "\"" && c == "\"" && i > 1L) break
      }
      v <- substr(rest, 2L, i - 1L)
    } else v <- sub("^([^,}\n]*).*$", "\\1", rest)
    v <- gsub("[{}]", "", .cn_tex_accents(v)); trimws(gsub("[[:space:]]+", " ", v))
  }
  recs <- lapply(seq_along(starts), function(k) {
    ent <- substr(txt, starts[k], ends[k])
    type <- tolower(sub("^@([A-Za-z]+).*$", "\\1", ent)); if (type %in% c("comment", "string", "preamble")) return(NULL)
    au <- field(ent, "author"); au <- if (is.null(au)) character(0) else trimws(strsplit(au, "\\s+and\\s+", perl = TRUE)[[1]])
    list(Authors = paste(au, collapse = "; "), Title = field(ent, "title"), Year = field(ent, "year"), DOI = field(ent, "doi"),
         Source = field(ent, "journal") %||% field(ent, "booktitle"), Document_Type = type)
  })
  .cn_rows_to_df(Filter(Negate(is.null), recs))
}

# --- Web of Science: plain-text ("FN Clarivate ...", two-letter tags) and tab-delimited exports -------------------
cn_parse_wos <- function(path) {
  x <- .cn_read_text(path)
  if (length(x) && grepl("^PT\t", x[1])) {                                  # tab-delimited export
    d <- utils::read.delim(path, quote = "", stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM", row.names = NULL)
    g <- function(k) if (k %in% names(d)) as.character(d[[k]]) else rep(NA_character_, nrow(d))
    return(.cn_rows_to_df(lapply(seq_len(nrow(d)), function(i) list(Authors = g("AU")[i], Title = g("TI")[i], Year = g("PY")[i],
           DOI = g("DI")[i], Source = g("SO")[i], Citations = g("TC")[i], Document_Type = g("DT")[i]))))
  }
  recs <- list(); cur <- list(); last <- NULL
  for (ln in x) {
    tag <- substr(ln, 1, 2); val <- trimws(substring(ln, 4))
    if (tag == "ER") { if (length(cur)) { cur$Authors <- paste(cur$AU, collapse = "; "); recs[[length(recs) + 1]] <- cur }; cur <- list(); last <- NULL; next }
    if (tag == "  " && !is.null(last)) { if (last %in% c("AU", "AF")) cur[[last]] <- c(cur[[last]], val) else cur[[last]] <- paste(cur[[last]], val); next }
    if (!grepl("^[A-Z][A-Z0-9]$", tag)) next
    last <- tag; cur[[tag]] <- val
    if (tag == "TI") cur$Title <- val; if (tag == "PY") cur$Year <- val; if (tag == "DI") cur$DOI <- val
    if (tag == "SO") cur$Source <- val; if (tag == "TC") cur$Citations <- val; if (tag == "DT") cur$Document_Type <- val
  }
  recs <- lapply(recs, function(r) { if (!is.null(r$TI)) r$Title <- r$TI; r })
  .cn_rows_to_df(recs)
}

# Dispatch by extension / content sniffing. Returns NULL for unknown formats.
cn_parse_bibliographic <- function(path, ext = tolower(tools::file_ext(path))) {
  first <- tryCatch(readLines(path, n = 3L, warn = FALSE), error = function(e) character(0))
  if (ext == "ris" || any(grepl("^TY  - ", first))) return(cn_parse_ris(path))
  if (ext %in% c("bib", "bibtex") || any(grepl("^\\s*@[A-Za-z]+\\s*\\{", first))) return(cn_parse_bibtex(path))
  if (any(grepl("^(﻿)?FN (Clarivate|Thomson)", first)) || any(grepl("^(﻿)?PT\t", first))) return(cn_parse_wos(path))
  NULL
}
