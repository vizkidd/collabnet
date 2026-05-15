

# # Helper to escape special regex characters from user inputs
# escape_regex_inline <- function(x) {
#   gsub("([][{}()+*^$\\\\|?])", "\\\\\\1", x)
# }

collabnet_required_cols <- c("Citations",	"User_Journal",	"orcid",	"Name",	"JIF5Years",	"Qscore",	"JCR_Journal", "Title",	"Authors",	"Year",	"Source")

# Helper to escape special regex characters from user inputs
escape_regex_inline <- function(x) {
  gsub("([\\.\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\\\|])", "\\\\\\1", x)
}

# Helper to launch the Step 2 Modal (Keeps your code DRY)
show_row_merge_modal <- function(rv, session) {
  showModal(modalDialog(
    title = tags$span(icon("layer-group", lib = "font-awesome"), " Step 2: Row Merging"),
    size = "m",
    radioButtons("row_import_type", label = "Choose Row import type:", inline = TRUE, choices = c("New", "Append", "Merge")),
    uiOutput("row_merge_ui"),
    footer = tagList(
      modalButton("Cancel"),
      # shinyjs::disabled(actionButton("confirm_import", "Confirm & Finish Import", class = "btn-success"))
      actionButton("confirm_import", "Confirm & Finish Import", 
                   class = "btn-success",
                   disabled = "disabled", 
                   style = "pointer-events: none; opacity: 0.5;")
    ),
    easyClose = FALSE
  ))
  
  if (is.null(rv$glens_etable_final) || ncol(rv$glens_etable_final) == 0) {
    updateRadioButtons(session, "row_import_type", selected = "New")
    shinyjs::delay(100, {
      shinyjs::runjs("$('input[name=\"row_import_type\"][value=\"Append\"]').prop('disabled', true);")
      shinyjs::runjs("$('input[name=\"row_import_type\"][value=\"Merge\"]').prop('disabled', true);")
    })
  }
}

detect_vpn <- function(rv, output) {
  req <- request("https://ipinfo.io/json") |>
    req_timeout(3) |>
    req_error(is_error = ~ FALSE) # Prevent it from throwing an R error if the API fails
  # Perform the request, catch any hard network failures (e.g., no internet)
  ip_resp <- tryCatch(req_perform(req), error = function(e) NULL)
  
  if (!is.null(ip_resp) && resp_status(ip_resp) == 200) {
    # Extract the body as a list
    ip_data <- resp_body_json(ip_resp)
    org_name <- tolower(ip_data$org)
    # print(ip_data)
    # if (grepl("cloudflare", org_name)) {
    #return("Cloudflare WARP detected.")
    # } else if (grepl("vpn|proxy", org_name)) {
    #return("A VPN or Proxy service was detected.")
    # }
    rv$log_text <- paste(
      rv$log_text,
      "Warning: Detected VPN. Skipping APIs.",
      sep = "<br>"
    )
    # output$log <- renderText({ rv$log_text })
  }
  return(NULL) # Network looks normal
}

#Flow functions
extend_input_table <- function(rv, df, author_match_regex, target_variants_norm) {
  print("extend_input_table(rv):")
  print("author_match_regex:")
  print(author_match_regex)
  print("target_variants_norm:")
  print(target_variants_norm)
  if (nrow(df) <= 0) {
    rv$log_text <- paste(rv$log_text, paste("No data to extend."), sep="<br>")
    return(df)
  }
  
  # --- 1. DEFENSIVE CHECKS: Prevent missing column crashes ---
  if (!"Citations" %in% colnames(df)) df$Citations <- 0
  if (!"Authors" %in% colnames(df)) df$Authors <- NA_character_
  if (!"Year" %in% colnames(df)) df$Year <- NA_integer_
  # -----------------------------------------------------------
  
  # --- 2. EXTRACT RAW KEYWORDS SAFELY ---
  raw_kws <- if (is.list(rv$target_variants_norm)) {
    if (!is.null(names(rv$target_variants_norm))) {
      names(rv$target_variants_norm)
    } else {
      sapply(rv$target_variants_norm, function(x) x$norm)
    }
  } else {
    as.character(rv$target_variants_norm)
  }
  
  # Inject your raw lookup text (e.g., "Electrical") into the array!
  generic_kws <- if (!is.null(rv$author_list) && length(rv$author_list) > 0) rv$author_list else ""
  
  kw_list <- unique(c(raw_kws, rv$author_match_regex, generic_kws))
  kw_list <- kw_list[!is.na(kw_list) & trimws(kw_list) != ""]
  
  print("kw_list:")
  print(kw_list)
  
  # --- 3. BYPASS LOGIC: No Keywords Provided ---
  if (length(kw_list) == 0) {
    df <- df %>%
      dplyr::mutate(
        label = "No_Filter",
        matched_token = NA_character_,
        First_Author = 0L,
        Second_Author = 0L,
        Co_Author = 0L,
        Corresponding_Author = 0L,
        Author_Count = stringr::str_count(Authors, ",") + 1,
        Adjustment_Weights = 0.0,
        Adjusted_Citations = suppressWarnings(as.numeric(Citations)),
        Year = as.integer(Year),
        position_rank = 0L
      )
    return(df) # Exit the function early
  }
  
  # --- Retrieve toggles from rv (Fallback to TRUE if missing) ---
  ext_match <- if (!is.null(rv$ext_match)) rv$ext_match else TRUE
  ignore_case <- if (!is.null(rv$ignore_case)) rv$ignore_case else TRUE
  
  print(paste("ext_match:", ext_match))
  print(paste("rv$ext_match:", rv$ext_match))
  print(paste("ignore_case:", ignore_case))
  print(paste("rv$ignore_case:", rv$ignore_case))
  
  # Fetch selected columns and delimiters from the extended controls panel
  search_cols <- names(rv$detected_mv_cols)
  delims <- rv$detected_mv_cols
  
  if (is.null(search_cols) || length(search_cols) == 0) {
    search_cols <- "Authors"
    delims <- list(Authors = ",")
  }
  
  valid_search_cols <- intersect(search_cols, colnames(df))
  
  if(length(valid_search_cols) == 0) {
    valid_search_cols <- "Authors"
  }
  
  # --- 4. EXTENDED MATCHING ---
  glens_extended_table <- df %>%
    rowwise() %>%
    mutate(
      dec = list({
        best_match <- list(label = "Not_found", matched_token = NA)
        
        for (col in valid_search_cols) {
          val <- get(col)
          
          if (!is.na(val) && val != "") {
            
            # Standardize delimiter
            col_delim <- if (!is.null(delims[[col]])) delims[[col]] else ","
            if (col_delim != ",") {
              val <- gsub(escape_regex(col_delim), ",", val)
            }
            
            # --- BRANCH: Extended Matching vs Basic Regex ---
            if (ext_match) {
              match_res <- decide_label_for_target(val, rv$target_variants_norm, rv$author_match_regex)
              if (match_res$label != "Not_found") {
                best_match <- match_res
                break 
              }
            } else {
              match_found <- FALSE
              
              for (kw in kw_list) {
                print(paste("kw:",kw))
                is_match <- tryCatch(
                  grepl(kw, val, ignore.case = ignore_case),
                  error = function(e) FALSE
                )
                
                if (is_match) {
                  best_match <- list(label = "Generic_Match", matched_token = kw)
                  match_found <- TRUE
                  break
                }
              }
              if (match_found) break
            }
          }
        }
        best_match
      }),
      label = dec$label,
      matched_token = dec$matched_token
    ) %>%
    ungroup() %>%
    filter(label != "Not_found") %>%
    mutate(
      First_Author = as.integer(label == "First_Author"),
      Second_Author = as.integer(label == "Second_Author"),
      Co_Author = as.integer(label == "Co_Author"),
      Corresponding_Author = as.integer(label == "Corresponding_Author"),
      Generic_Match = as.integer(label == "Generic_Match") 
    ) %>%
    select(-dec)
  
  if(nrow(glens_extended_table) <= 0) {
    rv$log_text <- paste(rv$log_text, "Warning: No matches found during table extension.", sep="<br>")
    print("Warning: No matches found during table extension.")
    return(glens_extended_table) 
  }
  
  count_target_col <- if ("Authors" %in% valid_search_cols) "Authors" else valid_search_cols[1]
  count_target_delim <- if (!is.null(delims[[count_target_col]])) delims[[count_target_col]] else ","
  
  if (!"Author_Count" %in% colnames(glens_extended_table)) {
    glens_extended_table <- glens_extended_table %>%
      mutate(Author_Count = stringr::str_count(!!sym(count_target_col), escape_regex(count_target_delim)) + 1)
  }
  
  # Correct weight logic
  glens_extended_table <- glens_extended_table %>%
    mutate(
      Adjustment_Weights = case_when(
        label == "Generic_Match" ~ 1.00,
        label == "Corresponding_Author" ~ 1.00,
        label == "First_Author" ~ 0.90,
        label == "Second_Author" ~ 0.50,
        label == "Co_Author" & Author_Count <= 6 ~ 0.25,
        label == "Co_Author" & Author_Count > 6 ~ 0.10,
        TRUE ~ 0
      ),
      Adjusted_Citations = as.numeric(Citations) * Adjustment_Weights
    )
  
  glens_extended_table$Adjusted_Citations <- suppressWarnings(as.numeric(glens_extended_table$Adjusted_Citations))
  
  glens_extended_table <- glens_extended_table %>%
    mutate(Co_Author = ifelse(Generic_Match == 1, 1L, Co_Author))
  
  for (col in c("First_Author","Second_Author","Co_Author","Corresponding_Author")) {
    glens_extended_table[[col]][is.na(glens_extended_table[[col]])] <- 0
    glens_extended_table[[col]] <- ifelse(glens_extended_table[[col]] >= 1, 1L, 0L)
  }
  
  ret_df <- glens_extended_table %>%
    mutate(
      position_rank = case_when(
        First_Author == 1 ~ 1L,
        Second_Author == 1 ~ 2L,
        Co_Author == 1 ~ 3L,
        Corresponding_Author == 1 ~ 4L,
        TRUE ~ 99L
      ),
      adj_cit_for_sort = ifelse(is.na(Adjusted_Citations), -Inf, Adjusted_Citations)
    ) %>%
    arrange(position_rank, desc(adj_cit_for_sort)) %>%
    select(-adj_cit_for_sort, -Generic_Match) %>%
    mutate(Year = as.integer(Year))
  
  return(ret_df)
}

# extend_input_table <- function(rv) {
#   # print("target_variants_norm")
#   # print(rv$target_variants_norm)
#   # glens_extended_table <- dplyr::bind_rows(lapply(rv$target_variants_norm, function(curr_variant){
#   #   return(rv$glens_input_table %>%
#   #     rowwise() %>%
#   #     mutate(
#   #       dec = list(decide_label_for_target(Authors, curr_variant, rv$author_match_regex)),
#   #       label = dec$label,
#   #       matched_token = dec$matched_token
#   #     ) %>%
#   #     ungroup() %>%
#   #     filter(label != "Not_found") %>%
#   #     mutate(
#   #       First_Author = as.integer(label == "First_Author"),
#   #       Second_Author = as.integer(label == "Second_Author"),
#   #       Co_Author = as.integer(label == "Co_Author"),
#   #       Corresponding_Author = as.integer(label == "Corresponding_Author")
#   #     ) %>%
#   #     select(-dec) ) #END - return
#   # })) #END - lapply
#   
#   
#   
#   if(nrow(rv$glens_input_table) <= 0){
#     rv$log_text <- paste(rv$log_text, paste("No data to extend."), sep="<br>")
#     return()
#   }
#   
#   # filtered_glens_input_table <- apply_author_logic(
#   #   pubs_df          = rv$glens_input_table,
#   #   primary_regex    = rv$author_match_regex,
#   #   selected_authors = rv$selected_filter_authors, 
#   #   gate             = rv$author_logic_gate        
#   # )
#   # 
#   # if(nrow(filtered_glens_input_table) <= 0){
#   #  stop("Check author logical filter: filtered_glens_input_table") 
#   # }
#   
#   glens_extended_table <- rv$glens_input_table %>%
#     rowwise() %>%
#     mutate(
#       dec = list(decide_label_for_target(Authors, rv$target_variants_norm, rv$author_match_regex)),
#       label = dec$label,
#       matched_token = dec$matched_token
#     ) %>%
#     ungroup() %>%
#     filter(label != "Not_found") %>%
#     mutate(
#       First_Author = as.integer(label == "First_Author"),
#       Second_Author = as.integer(label == "Second_Author"),
#       Co_Author = as.integer(label == "Co_Author"),
#       Corresponding_Author = as.integer(label == "Corresponding_Author")
#     ) %>%
#     select(-dec)
#   
#   # Ensure Author_Count exists
#   if (!"Author_Count" %in% colnames(glens_extended_table)) {
#     glens_extended_table <- glens_extended_table %>%
#       mutate(Author_Count = str_count(Authors, ",") + 1)
#   }
#   
#   # Correct weight logic
#   glens_extended_table <- glens_extended_table %>%
#     mutate(
#       Adjustment_Weights = case_when(
#         label == "Corresponding_Author" ~ 1.00,
#         label == "First_Author" ~ 0.90,
#         label == "Second_Author" ~ 0.50,
#         label == "Co_Author" & Author_Count <= 6 ~ 0.25,
#         label == "Co_Author" & Author_Count > 6 ~ 0.10,
#         TRUE ~ 0
#       ),
#       Adjusted_Citations = as.numeric(Citations) * Adjustment_Weights
#     )
#   
#   # Clean numeric columns
#   glens_extended_table$Adjusted_Citations <- suppressWarnings(as.numeric(glens_extended_table$Adjusted_Citations))
#   
#   for (col in c("First_Author","Second_Author","Co_Author","Corresponding_Author")) {
#     glens_extended_table[[col]][is.na(glens_extended_table[[col]])] <- 0
#     glens_extended_table[[col]] <- ifelse(glens_extended_table[[col]] >= 1, 1L, 0L)
#   }
#   
#   # Global ordering (consistent with file2.R logic)
#   rv$glens_etable_final <- glens_extended_table %>%
#     mutate(
#       position_rank = case_when(
#         First_Author == 1 ~ 1L,
#         Second_Author == 1 ~ 2L,
#         Co_Author == 1 ~ 3L,
#         Corresponding_Author == 1 ~ 4L,
#         TRUE ~ 99L
#       ),
#       adj_cit_for_sort = ifelse(is.na(Adjusted_Citations), -Inf, Adjusted_Citations)
#     ) %>%
#     arrange(position_rank, desc(adj_cit_for_sort)) %>%
#     select(-adj_cit_for_sort) %>%
#     mutate(Year = as.integer(Year))
# }

compute_indices <- function(rv, df) {
  
  # Strict H-index (Requires a NUMERIC VECTOR)
  compute_h_index <- function(citations_vec) {
    # Ensure the input is safely numeric, not characters!
    citations_vec <- suppressWarnings(as.numeric(citations_vec))
    v <- citations_vec[!is.na(citations_vec)]
    
    if (length(v) == 0) return(0L)
    v <- sort(v, decreasing = TRUE)
    h <- 0L
    for (i in seq_along(v)) {
      if (v[i] >= i) h <- i else break 
    }
    as.integer(h)
  }
  
  positions <- c("First_Author", "Second_Author", "Co_Author", "Corresponding_Author")
  
  # Return NULL safely if data isn't ready
  if(!all(positions %in% colnames(df))) {
    return(NULL) 
  }
  
  results <- list()
  for (pos in positions) {
    sub <- df %>% filter(.data[[pos]] == 1)
    
    # FIX: Use pull() to extract a vector, not select() which returns a tibble
    h <- compute_h_index(sub %>% pull(Citations))
    results[[pos]] <- list(
      h_index = h,
      n_papers = nrow(sub)
    )
  }
  
  # Classical H-indices
  h_cites <- compute_h_index(df %>% pull(Citations))
  h_adjcites <- compute_h_index(df %>% pull(Adjusted_Citations))
  
  # Build table safely into a local variable
  summary_tab <- tibble(
    Position = positions,
    H_index = sapply(results, function(x) x$h_index),
    Num_papers = sapply(results, function(x) x$n_papers)
  )
  
  summary_tab <- summary_tab %>%
    add_row(Position = "h-index(Citations)",
            H_index = h_cites,
            Num_papers = nrow(df))
  
  # Return pure data! No rv$ assignments, no shinyjs calls.
  return(list(
    sh_index = h_adjcites, 
    summary_table = summary_tab
  ))
}

match_journals <- function(rv, df){
  # print("match_journals(rv):")
  # need_cols <- c("Title","Authors","Adjusted_Citations","First_Author","Second_Author","Co_Author","Corresponding_Author")
  # missing_cols <- setdiff(need_cols, names(df))
  # if (length(missing_cols) > 0) {
  #   # warning(paste("Author-level file missing columns:", paste(missing_cols, collapse = ", ")))
  #   rv$log_text <- paste("<span style='color: red;'>Author-level file missing columns:", paste(missing_cols, collapse = ", "),"</span>",sep="<br>")
  #   return(df)
  # }
  # 
  # # --- Safely extract a target journal column to normalize ---
  # # Look for User_Journal first, fallback to Journal
  # if ("User_Journal" %in% names(df)) {
  #   target_journal_col <- df$User_Journal
  # } else if ("Journal" %in% names(df)) {
  #   target_journal_col <- df$Journal
  # } else {
  #   warning("No Journal or User_Journal column found to match against!")
  #   return(df)
  # }
  # 
  # unique_journals <- unique(target_journal_col)
  # cat("Unique journals to match:", length(unique_journals), "\n")
  # 
  # df$Name_norm <- sapply(target_journal_col, function(x) normalize_journal(x))
  # 
  # match_idx <- unique(
  #   bind_rows(
  #     future_sapply(
  #       seq_len(length(unique_journals)),
  #       getExcelColumns,
  #       unique_journals = unique_journals,
  #       jsonData = jcr_names_norm,
  #       simplify = FALSE,
  #       future.packages = c("stringr", "dplyr"),
  #       future.globals = c("jcr_names_norm", "getExcelColumns"),
  #       future.seed = TRUE
  #     )
  #   )
  # )
  # 
  # print(paste("CHECK FLOW1:",colnames(jcr_names_norm),collapse=","))
  # print(paste("CHECK FLOW2:",colnames(match_idx),collapse=","))
  # # print(paste("match_idx:", paste(match_idx,collapse = ",")))
  # if (nrow(match_idx) > 0) {
  #   jcr_matched <- inner_join(jcr_names_norm, match_idx, by = c("Name_norm", "Qscore", "JIF5Years"))
  # } else {
  #   jcr_matched <- jcr_names_norm[0, ] 
  # }
  # 
  # # --- CRITICAL FIX: Isolate JCR columns before joining ---
  # jcr_subset <- jcr_matched %>%
  #   dplyr::select(Name_norm, 
  #                 JCR_Journal = Name, 
  #                 JCR_Qscore = Qscore, 
  #                 JCR_JIF5Years = JIF5Years) %>%
  #   dplyr::distinct(Name_norm, .keep_all = TRUE)
  # 
  # # --- NEW: PRE-JOIN CLEANUP ---
  # # If this runs multiple times, JCR_Journal will already exist in the app.
  # # We must drop it before the join to prevent .x and .y collisions!
  # if ("JCR_Journal" %in% names(df)) {
  #   df <- df %>% dplyr::select(-JCR_Journal)
  # }
  # 
  # print("CHECK FLOW3:")
  # # Perform the join. Because we dropped the old JCR_Journal, 
  # # NO OTHER COLUMNS will overlap, and no .x or .y suffixes can be created!
  # df_auth_joined <- dplyr::left_join(df, jcr_subset, by = "Name_norm")
  # 
  # # --- 1. Safely resolve Qscore ---
  # if ("Qscore" %in% names(df_auth_joined)) {
  #   # If app already has Qscore, fill missing ones with JCR, but prioritize JCR
  #   df_auth_joined <- df_auth_joined %>%
  #     dplyr::mutate(Qscore = dplyr::coalesce(as.character(JCR_Qscore), as.character(Qscore))) %>%
  #     dplyr::select(-JCR_Qscore)
  # } else {
  #   # Otherwise just rename the newly brought over JCR column
  #   df_auth_joined <- df_auth_joined %>% dplyr::rename(Qscore = JCR_Qscore)
  # }
  # 
  # # --- 2. Safely resolve JIF5Years ---
  # if ("JIF5Years" %in% names(df_auth_joined)) {
  #   df_auth_joined <- df_auth_joined %>%
  #     dplyr::mutate(JIF5Years = dplyr::coalesce(as.character(JCR_JIF5Years), as.character(JIF5Years))) %>%
  #     dplyr::select(-JCR_JIF5Years)
  # } else {
  #   df_auth_joined <- df_auth_joined %>% dplyr::rename(JIF5Years = JCR_JIF5Years)
  # }
  # 
  # # Perform the join. Because jcr_subset ONLY has Name_norm + 3 unique columns, 
  # # NO OTHER COLUMNS in df will be touched, renamed, or suffixed!
  # df_auth_joined <- dplyr::left_join(df, jcr_subset, by = "Name_norm")
  # 
  # # --- 1. Safely resolve Qscore ---
  # if ("Qscore" %in% names(df_auth_joined)) {
  #   # If app already has Qscore, fill missing ones with JCR, but prioritize JCR
  #   df_auth_joined <- df_auth_joined %>%
  #     dplyr::mutate(Qscore = dplyr::coalesce(as.character(JCR_Qscore), as.character(Qscore))) %>%
  #     dplyr::select(-JCR_Qscore)
  # } else {
  #   # Otherwise just rename the newly brought over JCR column
  #   df_auth_joined <- df_auth_joined %>% dplyr::rename(Qscore = JCR_Qscore)
  # }
  # 
  # # --- 2. Safely resolve JIF5Years ---
  # if ("JIF5Years" %in% names(df_auth_joined)) {
  #   df_auth_joined <- df_auth_joined %>%
  #     dplyr::mutate(JIF5Years = dplyr::coalesce(as.character(JCR_JIF5Years), as.character(JIF5Years))) %>%
  #     dplyr::select(-JCR_JIF5Years)
  # } else {
  #   df_auth_joined <- df_auth_joined %>% dplyr::rename(JIF5Years = JCR_JIF5Years)
  # }
  # 
  # print(paste("colnames(df_auth_joined):",paste(colnames(df_auth_joined), collapse=",")))
  # # Note: "User_Journal" and "Journal" are left completely intact exactly as they were!
  
  print("match_journals(rv): Starting matching...")
  
  # 1. Validation
  need_cols <- c("Title","Authors","Adjusted_Citations","First_Author","Second_Author","Co_Author","Corresponding_Author")
  missing_cols <- setdiff(need_cols, names(df))
  if (length(missing_cols) > 0) {
    rv$log_text <- paste(rv$log_text, paste0("<span style='color: red;'>Missing columns: ", paste(missing_cols, collapse = ", "), "</span>"), sep="<br>")
    return(df) # Return early if data is broken
  }
  
  # 2. Determine Column
  target_journal_col <- if ("User_Journal" %in% names(df)) df$User_Journal else df$Journal
  if (is.null(target_journal_col)) {
    return(df)
  }
  
  # 3. Normalization & Matching
  df$Name_norm <- sapply(target_journal_col, normalize_journal)
  unique_journals <- unique(target_journal_col)
  
  match_idx <- unique(bind_rows(
    future_sapply(seq_along(unique_journals), getExcelColumns, 
                  unique_journals = unique_journals, jsonData = jcr_names_norm, 
                  simplify = FALSE, future.packages = c("stringr", "dplyr"))
  ))
  
  # 4. Join with JCR Data
  jcr_subset <- if (nrow(match_idx) > 0) {
    inner_join(jcr_names_norm, match_idx, by = c("Name_norm", "Qscore", "JIF5Years")) %>%
      dplyr::select(Name_norm, JCR_Journal = Name, JCR_Qscore = Qscore, JCR_JIF5Years = JIF5Years) %>%
      dplyr::distinct(Name_norm, .keep_all = TRUE)
  } else {
    NULL
  }
  
  # 5. Clean up existing columns to prevent .x / .y conflicts
  df <- df %>% dplyr::select(-any_of(c("JCR_Journal", "JCR_Qscore", "JCR_JIF5Years")))
  
  # 6. Merge and Coalesce
  if (!is.null(jcr_subset)) {
    # Ensure columns exist before coalescing to avoid errors
    if (!"Qscore" %in% names(df)) df$Qscore <- "NA"
    if (!"JIF5Years" %in% names(df)) df$JIF5Years <- "0"
    
    df <- left_join(df, jcr_subset, by = "Name_norm") %>%
      mutate(
        # Use JCR data if available, otherwise keep existing/default
        Qscore = coalesce(as.character(JCR_Qscore), as.character(Qscore)),
        JIF5Years = coalesce(as.character(JCR_JIF5Years), as.character(JIF5Years))
      ) %>%
      dplyr::select(-any_of(c("JCR_Qscore", "JCR_JIF5Years")))
  }
  
  # 7. Fallback Regex Matching (Simplified loop)
  unmatched <- which(is.na(df$JCR_Journal))
  if (length(unmatched) > 0) {
    cat("Trying fallback substring match for", length(unmatched), "journals...\n")
    for (i in unmatched) {
      jn <- df$Name_norm[i]
      if (is.na(jn) || nchar(jn) < 3) next
      hits <- grep(jn, jcr_names_norm$Name_norm, value = TRUE)
      
      if (length(hits) == 1) {
        idx <- which(jcr_names_norm$Name_norm == hits)[1]
        df$JCR_Journal[i] <- jcr_names_norm$Name[idx]
        df$Qscore[i] <- as.character(jcr_names_norm$Qscore[idx])
      }
    }
  }
  
  # Final formatting
  df <- df %>%
    dplyr::mutate(Qscore = dplyr::if_else(is.na(Qscore), "Unranked", as.character(Qscore)))
  
  n_unmatched <- length(which(is.na(df$JCR_Journal)))
  cat("Number of unmatched journal rows:", n_unmatched, "\n")
  
  # Save the protected dataframe back to reactive values
  # rv$glens_etable_final <- df_auth_joined %>% dplyr::distinct()
  df <- df %>% dplyr::distinct()
  return(df)
}

refresh_data <- function(rv, session){
  
}

# match_journals <- function(rv){
#   
#   need_cols <- c("Title","Authors","Adjusted_Citations","Journal",
#                  "First_Author","Second_Author","Co_Author","Corresponding_Author")
#   missing_cols <- setdiff(need_cols, names(rv$glens_etable_final))
#   if (length(missing_cols) > 0) {
#     warning(paste("Author-level file missing columns:", paste(missing_cols, collapse = ", ")))
#     return()
#   }
#   
#   unique_journals <- unique(rv$glens_etable_final$Journal)
#   cat("Unique journals to match:", length(unique_journals), "\n")
#   
#   rv$glens_etable_final$Name_norm <- sapply(rv$glens_etable_final$Journal, function(x) normalize_journal(x))
#   
#   # Note: This assumes 'jcr' and 'jcr_names_norm' are loaded in your global environment 
#   # since the reading code was commented out!
#   
#   match_idx <- unique(
#     bind_rows(
#       future_sapply(
#         seq_len(length(unique_journals)),
#         getExcelColumns,
#         unique_journals = unique_journals,
#         jsonData = jcr_names_norm,
#         simplify = FALSE,
#         future.packages = c("stringr", "dplyr"),
#         # EXPLICITLY pass the large object and the function
#         future.globals = c("jcr_names_norm", "getExcelColumns"),
#         future.seed = TRUE
#       )
#     )
#   )
#   print("HERE0")
#   # Safely handle the case where absolutely NO journals were matched in pass 1
#   if (nrow(match_idx) > 0) {
#     jcr_matched <- inner_join(jcr_names_norm, match_idx, by = c("Name_norm", "Qscore", "JIF5Years"))
#     print(str(match_idx))
#     print(str(jcr_matched))
#     print("HERE0.1")  
#   } else {
#     jcr_matched <- jcr[0, ] # Creates an empty df that still has the Qscore column
#     print("HERE0.2")
#   }
#   
#   # # Clean up old columns just in case
#   rv$glens_etable_final$Qscore <- NULL
#   rv$glens_etable_final$JIF5Years <- NULL
#   print("HERE1")
#   print(str(rv$glens_etable_final))
#   df_auth_joined <- left_join(rv$glens_etable_final, jcr_matched, by = c("Name_norm"))
#   print("HERE2")
#   
#   # --- 1. Handle Journal Naming ---
#   if ("Journal.x" %in% names(df_auth_joined)) {
#     df_auth_joined <- df_auth_joined %>% rename("User_Journal" = Journal.x)
#   } else if ("Journal" %in% names(df_auth_joined) && !"User_Journal" %in% names(df_auth_joined)) {
#     df_auth_joined <- df_auth_joined %>% rename("User_Journal" = Journal)
#   }
#   
#   if ("Journal.y" %in% names(df_auth_joined)) {
#     df_auth_joined <- df_auth_joined %>% rename("JCR_Journal" = Journal.y)
#   } else if (!"JCR_Journal" %in% names(df_auth_joined)) {
#     df_auth_joined$JCR_Journal <- NA_character_
#   }
#   
#   # --- 2. CRITICAL FIX: Resolve Qscore .x and .y collisions ---
#   if ("Qscore.x" %in% names(df_auth_joined) && "Qscore.y" %in% names(df_auth_joined)) {
#     df_auth_joined <- df_auth_joined %>%
#       mutate(Qscore = coalesce(Qscore.y, Qscore.x)) %>% # Prefer new match (.y), fallback to old (.x)
#       select(-Qscore.x, -Qscore.y)                      # Remove the messy collision columns
#   } else if (!"Qscore" %in% names(df_auth_joined)) {
#     df_auth_joined$Qscore <- NA_character_
#   }
#   
#   # --- 3. Resolve JIF5Years .x and .y collisions ---
#   if ("JIF5Years.x" %in% names(df_auth_joined) && "JIF5Years.y" %in% names(df_auth_joined)) {
#     df_auth_joined <- df_auth_joined %>%
#       mutate(JIF5Years = coalesce(JIF5Years.y, JIF5Years.x)) %>%
#       select(-JIF5Years.x, -JIF5Years.y)
#   }
#   print(str(df_auth_joined))
#   print("HERE3")
#   # For any unmatched journals, try a fallback: look for exact substring match in Name
#   unmatched <- which(is.na(df_auth_joined$JCR_Journal))
#   if (length(unmatched) > 0) {
#     cat("Trying fallback substring match for", length(unmatched), "journals...\n")
#     for (i in unmatched) {
#       jn <- df_auth_joined$Name_norm[i]
#       if (is.na(jn) || nchar(jn) < 3) next
#       hits <- grep(jn, jcr_names_norm$Name_norm, value = TRUE)
#       
#       if (length(hits) == 1) {
#         idx <- which(jcr_names_norm$Name_norm == hits)[1]
#         df_auth_joined$JCR_Journal[i] <- jcr_names_norm$Name[idx]
#         df_auth_joined$Qscore[i] <- as.character(jcr_names_norm$Qscore[idx])
#       }
#     }
#   }
#   print("HERE4")
#   # CRITICAL FIX 2: Convert all remaining NAs to "Unranked" so dplyr plotting doesn't crash
#   df_auth_joined <- df_auth_joined %>%
#     mutate(Qscore = if_else(is.na(Qscore), "Unranked", as.character(Qscore)))
#   print("HERE5")
#   n_unmatched <- length(which(is.na(df_auth_joined$JCR_Journal)))
#   cat("Number of unmatched journal rows:", n_unmatched, "\n")
#   print("HERE6")
#   if(nrow(rv$glens_etable_final) > 0){
#     rv$glens_etable_final <- dplyr::bind_rows(rv$glens_etable_final, df_auth_joined) %>% dplyr::distinct()
#   }else{
#     rv$glens_etable_final <- df_auth_joined %>% dplyr::distinct()
#   }
#   # rv$glens_etable_final <- df_auth_joined
#   print(str(rv$glens_etable_final))
#   print("HERE6.1")
# }
