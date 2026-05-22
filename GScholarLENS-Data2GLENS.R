suppressPackageStartupMessages(require(readxl))
suppressPackageStartupMessages(require(tools))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))

# --------------------------------------------------------------
# Helper functions - Script 2
# --------------------------------------------------------------
normalize_name <- function(x) {
  x2 <- tolower(as.character(x))
  x2 <- gsub("\\.", "", x2)
  x2 <- gsub("[^\\w\\s-]", " ", x2, perl = TRUE)
  x2 <- gsub("\\s+", " ", x2)
  str_trim(x2)
}

name_tokens <- function(x) {
  t <- unlist(str_split(x, "\\s+"))
  t[t != ""]
}

extract_parts <- function(name) {
  nn <- name_tokens(name)
  if (length(nn) == 0) return(list(first_tokens = character(0), last = "", initials = character(0)))
  last <- nn[length(nn)]
  if (length(nn) == 1) {
    first_tokens <- nn[1]
  } else {
    first_tokens <- nn[1:(length(nn) - 1)]
  }
  initials <- sapply(first_tokens, function(s) substr(s, 1, 1))
  list(first_tokens = first_tokens, last = last, initials = initials)
}

score_match <- function(token_clean, target_variants_norm) {
  best <- list(score = -Inf, variant = NA_character_, reason = NA_character_)
  
  # Safeguard: If the cleaned token is somehow NA, instantly return no match
  if (is.na(token_clean) || trimws(token_clean) == "") {
    return(list(score = -Inf, variant = NA_character_, reason = "no_match"))
  }
  
  for (vname in names(target_variants_norm)) {
    vnorm <- target_variants_norm[[vname]]$norm
    vparts <- target_variants_norm[[vname]]$parts
    tparts <- extract_parts(token_clean)
    
    # Score levels
    if (token_clean == vnorm) {
      sc <- 100     # exact normalized full-name
      reason <- "exact_full"
    } else if (tparts$last == vparts$last &&
               length(tparts$first_tokens) >= 1 &&
               length(vparts$first_tokens) >= 1 &&
               tparts$first_tokens[1] == vparts$first_tokens[1]) {
      sc <- 90      # same first name + same last name
      reason <- "first_and_last_match"
    } else if (length(tparts$initials) >= 1 &&
               length(vparts$initials) >= 1 &&
               tparts$initials[1] == vparts$initials[1] &&
               tparts$last == vparts$last) {
      sc <- 80      # initial + last match
      reason <- "initial_and_last"
    } else if (tparts$last == vparts$last) {
      # sc <- 10      # last name only
      sc <- -Inf
      reason <- "last_only"
    } else {
      sc <- -Inf
      reason <- "no_match"
    }
    
    if (sc > best$score) best <- list(score = sc, variant = vname, reason = reason)
  }
  
  return(best)
}

escape_regex <- function(x) gsub("([][{}()+*^$\\\\.|?])", "\\\\\\1", x)
build_name_regex_for_variants <- function(vars, ignore_case = TRUE) {
  parts <- sapply(vars, function(v) paste0("\\b", escape_regex(v), "\\b"))
  if (ignore_case) {
    paste0("(?i)(", paste(parts, collapse = "|"), ")")
  } else {
    paste0("(", paste(parts, collapse = "|"), ")")
  }
}
# build_name_regex_for_variants <- function(vars) {
#   parts <- sapply(vars, function(v) paste0("\\b", escape_regex(v), "\\b"))
#   paste0("(?i)(", paste(parts, collapse = "|"), ")")
# }

# --------------------------------------------------------------
# Tokenize authors & position rules
# --------------------------------------------------------------
split_authors <- function(author_field) {
  # Safeguard: if the field is NA or completely blank, return empty
  if (is.na(author_field) || trimws(as.character(author_field)) == "") {
    return(character(0))
  }
  
  s <- as.character(author_field)
  s <- gsub("\\s+and\\s+|\\s+&\\s+|\\s+/\\s+|;", ",", s, ignore.case = TRUE)
  parts <- unlist(str_split(s, "\\s*,\\s*"))
  parts[parts != ""]
}

tokenize_and_position <- function(author_field) {
  # Safeguard: check for NA early
  if (is.na(author_field) || trimws(as.character(author_field)) == "") {
    return(tibble())
  }
  
  tokens <- split_authors(author_field)
  if (length(tokens) == 0) return(tibble())
  
  df_tokens <- tibble(raw = tokens) %>%
    # Filter out any accidental NA tokens that sneak through
    filter(!is.na(raw) & trimws(raw) != "") %>% 
    mutate(
      has_star = str_detect(raw, "\\*"),
      has_caret = str_detect(raw, "\\^"),
      clean = str_trim(str_replace_all(raw, "[\\*\\^]", "")),
      clean_norm = sapply(clean, normalize_name) # Ensure normalize_name maps cleanly
    )
  
  if(nrow(df_tokens) == 0) return(tibble())
  
  # '^' means shared position
  positions <- integer(nrow(df_tokens))
  pos <- 1
  for (i in seq_len(nrow(df_tokens))) {
    if (i == 1) {
      positions[i] <- pos
    } else if (df_tokens$has_caret[i]) {
      positions[i] <- pos
    } else {
      pos <- pos + 1
      positions[i] <- pos
    }
  }
  
  df_tokens$pos <- positions
  df_tokens
}

# --------------------------------------------------------------
# Decide label for target author
# --------------------------------------------------------------
decide_label_for_target <- function(author_field, target_variants_norm, author_regex) {
  toks <- tokenize_and_position(author_field)
  if (nrow(toks) == 0) return(list(label="Not_found", matched_token=NA))
  
  # print(toks)
  # print(nrow(toks))
  # print(target_variants_norm)
  
  last_pos <- max(toks$pos)
  any_star_in_row <- any(toks$has_star)
  
  # Score each token
  scores <- lapply(seq_len(nrow(toks)), function(i) {
    score_match(toks$clean_norm[i], target_variants_norm)
  })
  
  score_df <- tibble(
    idx = seq_len(nrow(toks)),
    score = sapply(scores, `[[`, "score"),
    reason = sapply(scores, `[[`, "reason"),
    has_star = toks$has_star,
    pos = toks$pos,
    clean = toks$clean
  )
  
  # If no strong matches, fallback to regex
  if (all(is.infinite(score_df$score))) {
    fallback <- which(str_detect(toks$clean, author_regex))
    if (length(fallback) == 0) return(list(label="Not_found", matched_token=NA))
    score_df$score[fallback] <- 5
    # return(list(label="Not_found", matched_token=NA))
  }
  
  # Pick best
  best_score <- max(score_df$score)
  candidates <- score_df %>% filter(score == best_score)
  
  # Prefer '*' (corresponding)
  if (any(candidates$has_star)) {
    candidates <- candidates %>% filter(has_star)
  }
  
  # Prefer last author *only if no '*' in entire row*
  if (nrow(candidates) > 1 && !any_star_in_row) {
    if (any(candidates$pos == last_pos)) {
      candidates <- candidates %>% filter(pos == last_pos)
    }
  }
  
  chosen <- candidates %>% arrange(idx) %>% slice(1)
  idx <- chosen$idx
  this_pos <- chosen$pos
  
  is_corresponding <- toks$has_star[idx] || ((!any_star_in_row) && this_pos == last_pos)
  
  label <- if (is_corresponding) "Corresponding_Author"
  else if (this_pos == 1) "First_Author"
  else if (this_pos == 2) "Second_Author"
  else "Co_Author"
  
  list(label=label, matched_token=toks$clean[idx], token_count=length(toks$clean))
}

# apply_author_logic <- function(pubs_df, primary_regex, selected_authors, gate) {
#   
#   # Failsafe: If no data, return as-is
#   if (nrow(pubs_df) == 0) return(pubs_df)
#   
#   # Clean list and remove empty strings
#   if (is.null(selected_authors)) selected_authors <- character(0)
#   selected_authors <- selected_authors[trimws(selected_authors) != ""]
#   
#   # 1. DYNAMIC TARGET POOL
#   # If NOR or NAND, take ALL authors into account (Primary + Selected)
#   # Otherwise, just use the selected secondary authors
#   if (!is.null(gate) && gate %in% c("NOR", "NAND")) {
#     target_authors <- unique(c(primary_regex, selected_authors))
#   } else {
#     target_authors <- unique(selected_authors)
#   }
#   
#   # If no targets exist for the current gate, skip filtering
#   if (length(target_authors) == 0) return(pubs_df)
#   
#   # 2. Escape special characters in names
#   escaped_authors <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", target_authors)
#   
#   # 3. Create a boolean matrix based on our dynamic target_authors
#   match_matrix <- sapply(escaped_authors, function(rgx) {
#     grepl(rgx, pubs_df$Authors, ignore.case = TRUE)
#   })
#   
#   # Safely handle single-row or single-column matrix collapses
#   if (!is.matrix(match_matrix)) {
#     match_matrix <- matrix(match_matrix, nrow = nrow(pubs_df), ncol = length(escaped_authors))
#   }
#   
#   N <- length(escaped_authors)
#   
#   # 4. Filter using the logic gates
#   pubs_df %>%
#     mutate(
#       match_counts = rowSums(match_matrix),
#       # Optional: Count the actual number of authors printed on the paper
#       total_paper_authors = stringr::str_count(Authors, ",") + 1
#     ) %>%
#     filter(
#       case_when(
#         is.null(gate)  ~ TRUE,
#         gate == "OR"   ~ match_counts > 0,  # Has AT LEAST 1 of the secondary authors
#         gate == "AND"  ~ match_counts == N, # Has ALL of the secondary authors
#         gate == "XOR"  ~ match_counts == 1, # Has EXACTLY 1 of the secondary authors
#         
#         # NOR & NAND now evaluate against ALL authors (Primary + Secondary)
#         gate == "NOR"  ~ match_counts == 0, # Has NONE of the primary or secondary authors
#         gate == "NAND" ~ match_counts < N,  # NEVER has ALL of them together
#         TRUE           ~ TRUE
#       )
#     ) %>%
#     select(-match_counts, -total_paper_authors) # Clean up the temporary columns
# }

apply_author_logic <- function(pubs_df, selected_authors, gate, ext_match = TRUE, ignore_case = TRUE, search_cols = "Authors") {
  
  selected_authors <- selected_authors[trimws(selected_authors) != ""]
  
  # If no authors are selected, OR if the gate tells us not to filter ("ANY"), 
  # return the data instantly without running heavy Regex math!
  if (length(selected_authors) == 0 || is.null(gate) || gate == "FULL") {
    return(pubs_df)
  }
  
  # Create a boolean matrix: rows = publications, cols = selected authors
  match_matrix <- sapply(selected_authors, function(author) {
    
    # Apply Extended Match logic to the regex boundary
    if (ext_match) {
      rgx <- paste0("\\b", escape_regex(author), "\\b")
    } else {
      rgx <- escape_regex(author) # Allow partial string matches
    }
    
    # Search across all selected columns. (Logical OR across the columns)
    # If the author is found in ANY of the mapped columns, they count as a match.
    col_matches <- lapply(search_cols, function(col) {
      if (col %in% colnames(pubs_df)) {
        grepl(rgx, pubs_df[[col]], ignore.case = ignore_case)
      } else {
        rep(FALSE, nrow(pubs_df)) # Safety fallback if column is missing
      }
    })
    
    # Combine the T/F results from all columns into a single vector
    Reduce("|", col_matches)
  })
  
  # Safely handle single-row or single-column matrix collapses
  if (!is.matrix(match_matrix)) {
    match_matrix <- matrix(match_matrix, nrow = nrow(pubs_df), ncol = length(selected_authors))
  }
  
  N <- length(selected_authors)
  
  # Apply the logical gate
  ret_df <- pubs_df %>%
    mutate(match_counts = rowSums(match_matrix)) %>%
    filter(
      case_when(
        is.null(gate)  ~ TRUE,
        gate == "OR"   ~ match_counts > 0,  # Has AT LEAST 1 of the selected authors
        gate == "AND"  ~ match_counts == N, # Has ALL of the selected authors
        gate == "XOR"  ~ match_counts == 1, # Has EXACTLY 1 of the selected authors
        gate == "NOR"  ~ match_counts == 0, # Has NONE of the selected authors
        gate == "NAND" ~ match_counts < N,  # Divide: NEVER has all of them together (can have some, or none)
        TRUE           ~ TRUE
      )
    ) %>%
    select(-match_counts)
  return(ret_df)
}

# -----------------------------
# H-index function (sorted descending inside)
# -----------------------------
compute_h_index <- function(citations_vec) {
  v <- citations_vec[!is.na(citations_vec)]
  if (length(v) == 0) return(0L)
  v <- sort(v, decreasing = TRUE)
  h <- 0L
  for (i in seq_along(v)) {
    if (v[i] > i) h <- i else break
  }
  as.integer(h)
}

########JOURNAL MATCHING AND SCORING HELPERS
# ---------------------------
# Read JCR file and pick relevant columns
# ---------------------------
read_jcr <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx","xls")) {
    j <- readxl::read_excel(path)
  } else if (ext %in% c("csv","txt")) {
    j <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Unsupported JCR file type")
  }
  #Rename 8th column to Qscore
  colnames(j)[8] <- "Qscore"
  # Normalize column names and select the ones requested (if present)
  names(j) <- str_trim(names(j))
  want <- c("Name", "Abbr Name","ISSN","EISSN","JIF","JIF5Years","Category","Qscore","Rank","Rank out of Total Journals")
  present <- want[want %in% names(j)]
  # print(colnames(j))
  # print(present)
  j2 <- j[, present, drop = FALSE]
  # rename to consistent names
  colnames(j2) <- make.names(colnames(j2))
  # ensure Rank columns are numeric if present
  if ("Rank" %in% names(j2)) j2$Rank <- suppressWarnings(as.numeric(j2$Rank))
  if ("Rank.out.of.Total.Journals" %in% names(j2)) j2$Rank.out.of.Total.Journals <- suppressWarnings(as.numeric(j2$Rank.out.of.Total.Journals))
  j2
}

normalize_journal <- function(x) {
  x |>
    str_to_lower() |>
    ## str_split(",\\s*(.+)", simplify = TRUE) |>
    # str_split("\\d", simplify = TRUE) |>
    str_split("(?<=[A-Za-z])\\s*\\d+", simplify = T) |>
    (\(y) y[1])() |>
    str_remove_all("\\d+") |>
    str_remove_all("[^\\w\\s]|_") |>
    str_squish()
}

getExcelColumns <- function(journalTitleIdx, unique_journals, jsonData) {
  journalTitle <- unique_journals[journalTitleIdx]
  if(length(journalTitle) <= 0){
    warning(paste("idx:", journalTitleIdx, "is empty!:", unique_journals[journalTitleIdx]))
    stop()
    return(data.frame(Name_norm=NA, Journal=NA, JIF5Years=NA, Qscore=NA))
  }
  title_norm <- normalize_journal(journalTitle)
  if(length(title_norm) <= 0){
    warning(paste("idx:", journalTitleIdx, "normalize failed!:", unique_journals[journalTitleIdx]))
    stop()
    # print(c(idx,title_norm, journalTitle))
  }
  idx <- c()
  idx <- which(
    str_detect(title_norm, fixed(jsonData$Name_norm)) &
      str_detect(jsonData$Name_norm, fixed(title_norm))
  )
  if (length(idx) == 0) {
    idx <- which(
      str_detect(title_norm, paste0("\\b", jsonData$Name_norm, "\\b")) &
        str_detect(jsonData$Name_norm, paste0("\\b", title_norm, "\\b"))
    )
    # idx <- which(
    #   str_detect(
    #     title_norm,
    #     paste0("(^|\\s)", jsonData$Name_norm, "(\\s|$)")
    #   )
    # )
  }
  
  if (length(idx) == 1) {
    i <- idx
    return(data.frame(
      Name_norm=title_norm,
      Journal=jsonData$Name[i],
      JIF5Years=jsonData$JIF5Years[i],
      Qscore=jsonData$Qscore[i]
    ))
  }
  else if(length(idx) > 1) {
    # warning(c(paste("idx:", journalTitleIdx, "multiple matches!:", title_norm," + ", journalTitle,"\nMatching with these JCR rows:\n")), paste(jsonData$Name[idx], collapse = ","), paste(idx, collapse = ","))
    # #stop()
    i <- idx[1]
    return(data.frame(
      Name_norm=title_norm,
      Journal=jsonData$Name[i],
      JIF5Years=jsonData$JIF5Years[i],
      Qscore=jsonData$Qscore[i]
    ))
  }
  
  if (length(idx) == 0) {
    idx <- which(
      str_detect(title_norm, paste0("\\b", jsonData$Name_norm, "\\b")) |
        str_detect(jsonData$Name_norm, paste0("\\b", title_norm, "\\b"))
    )
    # idx <- which(
    #   str_detect(
    #     title_norm,
    #     paste0("(^|\\s)", jsonData$Name_norm, "(\\s|$)")
    #   )
    # )
  }
  
  #DEBUG
  # if(length(idx) == 0) {
  #   warning(paste("idx:", journalTitleIdx, "cannot match!:", title_norm," + ", journalTitle))
  #   warning(paste("norm 1->2:",all(str_detect(title_norm, fixed(jsonData$Name_norm)))), "norm 2->1:", all(str_detect(jsonData$Name_norm, fixed(title_norm))))
  #   # stop()
  # }
  
  return(data.frame(Name_norm=title_norm, Journal=journalTitle, JIF5Years=NA, Qscore=NA))
}
