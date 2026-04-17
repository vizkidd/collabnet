

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
      actionButton("confirm_import", "Confirm & Finish Import", class = "btn-success")
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
      sep = "\n"
    )
    # output$log <- renderText({ rv$log_text })
  }
  return(NULL) # Network looks normal
}

#Flow functions
extend_input_table <- function(rv) {
  # print("target_variants_norm")
  # print(rv$target_variants_norm)
  # glens_extended_table <- dplyr::bind_rows(lapply(rv$target_variants_norm, function(curr_variant){
  #   return(rv$glens_input_table %>%
  #     rowwise() %>%
  #     mutate(
  #       dec = list(decide_label_for_target(Authors, curr_variant, rv$author_match_regex)),
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
  #     select(-dec) ) #END - return
  # })) #END - lapply
  
  
  
  if(nrow(rv$glens_input_table) <= 0){
    stop("Check author logical filter: rv$glens_input_table")  
  }
  
  # filtered_glens_input_table <- apply_author_logic(
  #   pubs_df          = rv$glens_input_table,
  #   primary_regex    = rv$author_match_regex,
  #   selected_authors = rv$selected_filter_authors, 
  #   gate             = rv$author_logic_gate        
  # )
  # 
  # if(nrow(filtered_glens_input_table) <= 0){
  #  stop("Check author logical filter: filtered_glens_input_table") 
  # }
  
  glens_extended_table <- rv$glens_input_table %>%
    rowwise() %>%
    mutate(
      dec = list(decide_label_for_target(Authors, rv$target_variants_norm, rv$author_match_regex)),
      label = dec$label,
      matched_token = dec$matched_token
    ) %>%
    ungroup() %>%
    filter(label != "Not_found") %>%
    mutate(
      First_Author = as.integer(label == "First_Author"),
      Second_Author = as.integer(label == "Second_Author"),
      Co_Author = as.integer(label == "Co_Author"),
      Corresponding_Author = as.integer(label == "Corresponding_Author")
    ) %>%
    select(-dec)
  
  # Ensure Author_Count exists
  if (!"Author_Count" %in% colnames(glens_extended_table)) {
    glens_extended_table <- glens_extended_table %>%
      mutate(Author_Count = str_count(Authors, ",") + 1)
  }
  
  # Correct weight logic
  glens_extended_table <- glens_extended_table %>%
    mutate(
      Adjustment_Weights = case_when(
        label == "Corresponding_Author" ~ 1.00,
        label == "First_Author" ~ 0.90,
        label == "Second_Author" ~ 0.50,
        label == "Co_Author" & Author_Count <= 6 ~ 0.25,
        label == "Co_Author" & Author_Count > 6 ~ 0.10,
        TRUE ~ 0
      ),
      Adjusted_Citations = as.numeric(Citations) * Adjustment_Weights
    )
  
  # Clean numeric columns
  glens_extended_table$Adjusted_Citations <- suppressWarnings(as.numeric(glens_extended_table$Adjusted_Citations))
  
  for (col in c("First_Author","Second_Author","Co_Author","Corresponding_Author")) {
    glens_extended_table[[col]][is.na(glens_extended_table[[col]])] <- 0
    glens_extended_table[[col]] <- ifelse(glens_extended_table[[col]] >= 1, 1L, 0L)
  }
  
  # Global ordering (consistent with file2.R logic)
  rv$glens_etable_final <- glens_extended_table %>%
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
    select(-adj_cit_for_sort) %>%
    mutate(Year = as.integer(Year))
}

compute_indices <- function(rv) {
  
  # Use FINAL ordered table only
  # df <- rv$glens_etable_final
  df <- rv$glens_year_filtered
  
  # Strict H-index (as you changed)
  compute_h_index <- function(citations_vec) {
    v <- citations_vec[!is.na(citations_vec)]
    if (length(v) == 0) return(0L)
    v <- sort(v, decreasing = TRUE)
    h <- 0L
    for (i in seq_along(v)) {
      #STRICT
      #if (v[i] > i) h <- i else break 
      #Correct/Standard way
      if (v[i] >= i) h <- i else break 
    }
    as.integer(h)
  }
  
  positions <- c("First_Author",
                 "Second_Author",
                 "Co_Author",
                 "Corresponding_Author")
  
  results <- list()
  
  for (pos in positions) {
    sub <- df %>% filter(.data[[pos]] == 1)
    h <- compute_h_index(sub %>% arrange(Citations) %>% select(Citations))
    results[[pos]] <- list(
      h_index = h,
      n_papers = nrow(sub)
    )
  }
  
  # Classical H-indices
  h_cites <- compute_h_index(df %>% arrange(Citations) %>% select(Citations))
  h_adjcites <- compute_h_index(df %>% arrange(Adjusted_Citations) %>% select(Adjusted_Citations))
  
  rv$summary_table <- tibble(
    Position = positions,
    H_index = sapply(results, function(x) x$h_index),
    Num_papers = sapply(results, function(x) x$n_papers)
  )
  
  rv$summary_table <- rv$summary_table %>%
    add_row(Position = "h-index(Citations)",
            H_index = h_cites,
            Num_papers = nrow(df))
  # %>%
  #   add_row(Position = "h-index(Adj.Citations)",
  #           H_index = h_adjcites,
  #           Num_papers = nrow(df))
  
  # # Correct Sh-index: sum ONLY the 4 positional H indices
  # rv$sh_index <- sum(rv$summary_table$H_index[
  #   rv$summary_table$Position %in% positions
  # ], na.rm = TRUE)
  
  rv$sh_index <- h_adjcites
  
  shinyjs::show("sh_index")
  shinyjs::show("summary_table")
  shinyjs::show("extended_table")
}

match_journals <- function(rv){
  
  need_cols <- c("Title","Authors","Adjusted_Citations","First_Author","Second_Author","Co_Author","Corresponding_Author")
  missing_cols <- setdiff(need_cols, names(rv$glens_etable_final))
  if (length(missing_cols) > 0) {
    # warning(paste("Author-level file missing columns:", paste(missing_cols, collapse = ", ")))
    rv$log_text <- paste("<span style='color: red;'>Author-level file missing columns:", paste(missing_cols, collapse = ", "),"</span>",sep="<br>")
    return()
  }
  
  # --- Safely extract a target journal column to normalize ---
  # Look for User_Journal first, fallback to Journal
  if ("User_Journal" %in% names(rv$glens_etable_final)) {
    target_journal_col <- rv$glens_etable_final$User_Journal
  } else if ("Journal" %in% names(rv$glens_etable_final)) {
    target_journal_col <- rv$glens_etable_final$Journal
  } else {
    warning("No Journal or User_Journal column found to match against!")
    return()
  }
  
  unique_journals <- unique(target_journal_col)
  cat("Unique journals to match:", length(unique_journals), "\n")
  
  rv$glens_etable_final$Name_norm <- sapply(target_journal_col, function(x) normalize_journal(x))
  
  match_idx <- unique(
    bind_rows(
      future_sapply(
        seq_len(length(unique_journals)),
        getExcelColumns,
        unique_journals = unique_journals,
        jsonData = jcr_names_norm,
        simplify = FALSE,
        future.packages = c("stringr", "dplyr"),
        future.globals = c("jcr_names_norm", "getExcelColumns"),
        future.seed = TRUE
      )
    )
  )
  
  if (nrow(match_idx) > 0) {
    jcr_matched <- inner_join(jcr_names_norm, match_idx, by = c("Name_norm", "Qscore", "JIF5Years"))
  } else {
    jcr_matched <- jcr[0, ] 
  }
  
  # Clean up old columns to prevent duplication
  rv$glens_etable_final$Qscore <- NULL
  rv$glens_etable_final$JIF5Years <- NULL
  
  # df_auth_joined contains your ENTIRE dataset now
  df_auth_joined <- left_join(rv$glens_etable_final, jcr_matched, by = c("Name_norm"))
  
  # --- 1. CRITICAL FIX: Bulletproof Journal Naming ---
  # Grab all possible variations of the journal name columns
  journal_cols <- intersect(names(df_auth_joined), 
                            c("User_Journal", "Journal", "Journal.x", "Journal.y", "User_Journal.x", "User_Journal.y"))
  
  if (length(journal_cols) > 0) {
    final_journal <- rep(NA_character_, nrow(df_auth_joined))
    
    # Coalesce all available data into one unified vector
    for (j_col in journal_cols) {
      final_journal <- dplyr::coalesce(final_journal, as.character(df_auth_joined[[j_col]]))
    }
    
    # Assign the master column
    df_auth_joined$User_Journal <- final_journal
    
    # Erase the messy leftover columns
    cols_to_remove <- setdiff(journal_cols, "User_Journal")
    if(length(cols_to_remove) > 0) {
      df_auth_joined <- df_auth_joined %>% dplyr::select(-dplyr::all_of(cols_to_remove))
    }
  } else {
    df_auth_joined$User_Journal <- NA_character_
  }
  
  if ("Journal.y" %in% names(df_auth_joined)) {
    df_auth_joined <- df_auth_joined %>% rename("JCR_Journal" = Journal.y)
  } else if (!"JCR_Journal" %in% names(df_auth_joined)) {
    df_auth_joined$JCR_Journal <- NA_character_
  }
  
  # --- 2. Resolve Qscore .x and .y collisions ---
  if ("Qscore.x" %in% names(df_auth_joined) && "Qscore.y" %in% names(df_auth_joined)) {
    df_auth_joined <- df_auth_joined %>%
      mutate(Qscore = coalesce(as.character(Qscore.y), as.character(Qscore.x))) %>% 
      select(-Qscore.x, -Qscore.y)                      
  } else if (!"Qscore" %in% names(df_auth_joined)) {
    df_auth_joined$Qscore <- NA_character_
  }
  
  # --- 3. Resolve JIF5Years .x and .y collisions ---
  if ("JIF5Years.x" %in% names(df_auth_joined) && "JIF5Years.y" %in% names(df_auth_joined)) {
    df_auth_joined <- df_auth_joined %>%
      mutate(JIF5Years = coalesce(as.character(JIF5Years.y), as.character(JIF5Years.x))) %>%
      select(-JIF5Years.x, -JIF5Years.y)
  }
  
  unmatched <- which(is.na(df_auth_joined$JCR_Journal))
  if (length(unmatched) > 0) {
    cat("Trying fallback substring match for", length(unmatched), "journals...\n")
    for (i in unmatched) {
      jn <- df_auth_joined$Name_norm[i]
      if (is.na(jn) || nchar(jn) < 3) next
      hits <- grep(jn, jcr_names_norm$Name_norm, value = TRUE)
      
      if (length(hits) == 1) {
        idx <- which(jcr_names_norm$Name_norm == hits)[1]
        df_auth_joined$JCR_Journal[i] <- jcr_names_norm$Name[idx]
        df_auth_joined$Qscore[i] <- as.character(jcr_names_norm$Qscore[idx])
      }
    }
  }
  
  df_auth_joined <- df_auth_joined %>%
    mutate(Qscore = if_else(is.na(Qscore), "Unranked", as.character(Qscore)))
  
  n_unmatched <- length(which(is.na(df_auth_joined$JCR_Journal)))
  cat("Number of unmatched journal rows:", n_unmatched, "\n")
  
  # --- 4. CRITICAL FIX: Do NOT bind_rows here! ---
  # df_auth_joined already contains the full updated dataset. 
  rv$glens_etable_final <- df_auth_joined %>% dplyr::distinct()
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
