# server.R (or inside server function)
suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(shinyjs))
suppressPackageStartupMessages(require(promises))
suppressPackageStartupMessages(require(future))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(showtext))
suppressPackageStartupMessages(require(systemfonts))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(plotly))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(stringi))
suppressPackageStartupMessages(require(tibble))
suppressPackageStartupMessages(require(scales))
suppressPackageStartupMessages(require(stringdist))
suppressPackageStartupMessages(require(future.apply))
suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(DT))
suppressPackageStartupMessages(require(sodium))
suppressPackageStartupMessages(require(digest))
suppressPackageStartupMessages(require(uuid))
suppressPackageStartupMessages(require(openssl))
suppressPackageStartupMessages(require(xfun))
suppressPackageStartupMessages(require(httr))
suppressPackageStartupMessages(require(xml2))
suppressPackageStartupMessages(require(httr2))
suppressPackageStartupMessages(require(jsonlite))

is_WASM <- grepl(pattern="wasm",x=Sys.info()["machine"])

# use a multisession plan so futures run in background R sessions
# if(!is_WASM){
#   future::plan(future::multisession)
future::plan(future::multicore)
# }else{
#   future::plan(future::sequential)
# }

fs::dir_create("keys") 

BIOS_ID <- ""

tryCatch({
glens_env <- new.env(parent = emptyenv())
BIOS_ID <- Sys.info()[["user"]] #FALLBACK 
if(xfun::is_windows()){
  BIOS_ID <- trimws(system("wmic bios get serialnumber", intern = TRUE)[2])
}else if(xfun::is_linux() || xfun::is_unix()){
  BIOS_ID <- readLines("/var/lib/dbus/machine-id")
}else if(xfun::is_macos()){
  stop("macOS not tested")
  system("ioreg -l | grep IOPlatformSerialNumber", intern = TRUE)
}else{
  stop("Could not find operating system!")
}
}, error=function(e){
  #Probably running in webR with WASM so lets take the session cookie
  # We are likely in WebR/WASM. System commands are sandboxed.
  message("OS hardware ID failed (likely WebR). Generating session UUID instead.")
  # # Pure Base R function to generate random hex strings (WASM safe!)
  # random_hex <- function(n) {
  #   paste(sample(c(0:9, letters[1:6]), n, replace = TRUE), collapse = "")
  # }
  # BIOS_ID <<- paste0(
  #   random_hex(8), "-", 
  #   random_hex(4), "-4", 
  #   random_hex(3), "-",
  #   sample(c("8", "9", "a", "b"), 1), random_hex(3), "-", 
  #   random_hex(12)
  # )
  BIOS_ID <<- uuid::UUIDgenerate()
})

# Print to verify it worked in the browser console
message(paste("Active ID:", BIOS_ID))

#CREATING PRIVATE KEY (if it doesn't exist) TO encrypt API keys
if (is_WASM) {
  # =========================================================================
  # BRANCH 1: WebR / WASM (No Sodium, Mock Encryption)
  # =========================================================================
  if (!fs::file_exists(file.path("keys", "private_wasm.key")) || !fs::file_exists(file.path("keys", "private_wasm.key.signed"))) {
    
    # 1. WASM-safe SHA512 hashing using digest
    raw_uuid <- uuid::UUIDgenerate() 
    hashed_uuid <- digest::digest(raw_uuid, algo = "sha512", serialize = FALSE)
    glens_env$privkey <- charToRaw(hashed_uuid)
    
    # 2. Skip sodium encryption
    glens_env$privkey_final <- glens_env$privkey 
    
    saveRDS(glens_env$privkey, file = file.path("keys", "private_wasm.key"))
    saveRDS(glens_env$privkey_final, file = file.path("keys", "private_wasm.key.signed"))
    
    glens_env$privkey_dec <- glens_env$privkey_final
    
  } else {
    # Read existing WASM keys
    glens_env$privkey <- readRDS(file.path("keys", "private_wasm.key"))
    glens_env$privkey_final <- readRDS(file.path("keys", "private_wasm.key.signed"))
    glens_env$privkey_dec <- glens_env$privkey_final
  }
  
} else {
  # =========================================================================
  # BRANCH 2: Local Standard R (Requires Sodium)
  # =========================================================================
  if (!fs::file_exists(file.path("keys", "private.key")) || !fs::file_exists(file.path("keys", "private.key.signed"))) {
    glens_env$privkey <- charToRaw(openssl::sha512(openssl::base64_encode(uuid::UUIDgenerate()), key=BIOS_ID))
    # print(glens_env$privkey)
    # print(str(glens_env$privkey))
    glens_env$privkey_final <- sodium::data_encrypt(glens_env$privkey, key=sha256(charToRaw(BIOS_ID)))
    saveRDS(glens_env$privkey, file = file.path("keys","private.key"))
    saveRDS(glens_env$privkey_final, file = file.path("keys","private.key.signed"))
    glens_env$privkey_dec <- sodium::data_decrypt(glens_env$privkey_final, key=sha256(charToRaw(BIOS_ID)))
    
  } else {
    glens_env$privkey <- readRDS(file.path("keys", "private.key"))
    glens_env$privkey_final <- readRDS(file.path("keys", "private.key.signed"))
    message("Checking local key files...")
    
    # Note: If this still throws a "24 bytes" error after you delete your old keys, 
    # make sure you are using sodium::simple_decrypt() which takes a 32-byte key (like sha256). 
    # data_decrypt() sometimes expects a 24-byte nonce depending on how you call it!
    glens_env$privkey_dec <- sodium::data_decrypt(
      glens_env$privkey_final, 
      key = sodium::sha256(charToRaw(BIOS_ID))
    )
    stopifnot(identical(glens_env$privkey_dec, glens_env$privkey))
  }
}

# if(!fs::file_exists(file.path("keys","private.key")) || !fs::file_exists(file.path("keys","private.key.signed"))){
#   # message(paste("HERE1.1"))
#   # glens_env$privkey <- charToRaw(openssl::sha512(openssl::base64_encode(uuid::UUIDgenerate()), key=BIOS_ID))
#   # # print(glens_env$privkey)
#   # # print(str(glens_env$privkey))
#   # glens_env$privkey_final <- sodium::data_encrypt(glens_env$privkey, key=sha256(charToRaw(BIOS_ID)))
#   # saveRDS(glens_env$privkey, file = file.path("keys","private.key"))
#   # saveRDS(glens_env$privkey_final, file = file.path("keys","private.key.signed"))
#   # message(paste("HERE1.2"))
#   # glens_env$privkey_dec <- sodium::data_decrypt(glens_env$privkey_final, key=sha256(charToRaw(BIOS_ID)))
#   # message(paste("HERE1.3"))
#   
#   # 1. WASM-safe SHA512 hashing using digest
#   raw_uuid <- uuid::UUIDgenerate() 
#   hashed_uuid <- digest::digest(raw_uuid, algo = "sha512", serialize = FALSE)
#   glens_env$privkey <- charToRaw(hashed_uuid)
#   # 2. Skip sodium encryption (WASM incompatible). 
#   # If you must obfuscate, use base64 or a simple XOR, but for a local session, just copy it.
#   glens_env$privkey_final <- glens_env$privkey 
#   # Ensure the directory exists in the VFS before saving
#   dir.create("keys", showWarnings = FALSE)
#   saveRDS(glens_env$privkey, file = file.path("keys","private.key"))
#   saveRDS(glens_env$privkey_final, file = file.path("keys","private.key.signed"))
#   
#   # 3. Skip sodium decryption
#   glens_env$privkey_dec <- glens_env$privkey_final
#   
# }else if(fs::file_exists(file.path("keys","private.key")) && fs::file_exists(file.path("keys","private.key.signed"))){
#   glens_env$privkey <- readRDS(file.path("keys","private.key"))
#   glens_env$privkey_final <- readRDS(file.path("keys","private.key.signed"))
#   message("Checking key files, if it doesn't work delete private keys in the keys/ folder and re-run the app.")
#   glens_env$privkey_dec <- sodium::data_decrypt(glens_env$privkey_final, key=sha256(charToRaw(BIOS_ID)))
#   stopifnot(identical(glens_env$privkey_dec, glens_env$privkey))
# }

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
    output$log <- renderText({ rv$log_text })
 }
 return(NULL) # Network looks normal
}


source("GScholarLENS-ProcessJCR.R", local = TRUE)
source("GScholarLENS-SCOPUS2Data.R", local = TRUE)

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
  # jcr_base <- "2024-JCR_IMPACT_FACTOR"
  # jcr_file_xlsx <- paste0(jcr_base, ".xlsx")
  # jcr_file_xls  <- paste0(jcr_base, ".xls")
  # jcr_file_csv  <- paste0(jcr_base, ".csv")
  # 
  # jcr_path <- NULL
  # if (file.exists(jcr_file_xlsx)) jcr_path <- jcr_file_xlsx
  # if (is.null(jcr_path) && file.exists(jcr_file_xls)) jcr_path <- jcr_file_xls
  # if (is.null(jcr_path) && file.exists(jcr_file_csv)) jcr_path <- jcr_file_csv
  # 
  # if (is.null(jcr_path)) {
  #   warning("Cannot find '2024-JCR_IMPACT_FACTOR(.xlsx/.csv)' in working directory.\n")
  #   # jcr_path <- readline(prompt = "Enter full path to JCR file (xlsx or csv): ")
  #   # jcr_path <- str_trim(jcr_path)
  #   warning("JCR file not found. Exiting.")
  #   return()
  # } else {
  #   cat("Found JCR file:", jcr_path, "\n")
  # }
  # jcr <- read_jcr(jcr_path)
  # 
  # # If JIF columns exist, ensure numeric
  # if ("JIF" %in% names(jcr)) jcr$JIF <- suppressWarnings(as.numeric(jcr$JIF))
  # if ("JIF5Years" %in% names(jcr)) jcr$JIF5Years <- suppressWarnings(as.numeric(jcr$JIF5Years))
  # 
  # need_cols <- c("Title","Authors","Adjusted_Citations","Journal",
  #                "First_Author","Second_Author","Co_Author","Corresponding_Author")
  # missing_cols <- setdiff(need_cols, names(rv$glens_etable_final))
  # if (length(missing_cols) > 0) {
  #   warning(paste("Author-level file missing columns:", paste(missing_cols, collapse = ", ")))
  #   return()
  # }
  # 
  # unique_journals <- unique(rv$glens_etable_final$Journal)
  # print(cat("Unique journals to match:", length(unique_journals), "\n"))
  # 
  # jcr$Name_norm <- sapply(jcr$Name, function(x) normalize_journal(x))
  # rv$glens_etable_final$Name_norm <- sapply(rv$glens_etable_final$Journal, function(x) normalize_journal(x))
  # 
  # jcr_names_norm <- jcr |>
  #   select(Name, Name_norm, JIF5Years, Qscore) 
  # 
  # match_idx <- unique(
  #   bind_rows(
  #     future_sapply(
  #       seq_len(length(unique_journals)),
  #       getExcelColumns,
  #       unique_journals = unique_journals,
  #       jsonData = jcr_names_norm,
  #       simplify = FALSE,
  #       future.packages = c("stringr", "dplyr")
  #     )
  #   )
  # )
  # 
  # jcr_matched <- inner_join(jcr,match_idx)
  
  need_cols <- c("Title","Authors","Adjusted_Citations","Journal",
                 "First_Author","Second_Author","Co_Author","Corresponding_Author")
  missing_cols <- setdiff(need_cols, names(rv$glens_etable_final))
  if (length(missing_cols) > 0) {
    warning(paste("Author-level file missing columns:", paste(missing_cols, collapse = ", ")))
    return()
  }
  
  unique_journals <- unique(rv$glens_etable_final$Journal)
  print(cat("Unique journals to match:", length(unique_journals), "\n"))
  
  rv$glens_etable_final$Name_norm <- sapply(rv$glens_etable_final$Journal, function(x) normalize_journal(x))
  
  match_idx <- unique(
    bind_rows(
      future_sapply(
        seq_len(length(unique_journals)),
        getExcelColumns,
        unique_journals = unique_journals,
        jsonData = jcr_names_norm,
        simplify = FALSE,
        future.packages = c("stringr", "dplyr")
      )
    )
  )
  
  jcr_matched <- inner_join(jcr,match_idx)
  
  rv$glens_etable_final[c("Qscore", "JIF5Years")] <- NULL
  df_auth_joined <- left_join(rv$glens_etable_final, jcr_matched, by = c("Name_norm"))#, relationship = "many-to-many") 
  df_auth_joined <- df_auth_joined |> rename("User_Journal" = Journal.x) |> rename("JCR_Journal" = Journal.y)
  
  # For any unmatched journals, try a fallback: look for exact substring match in Name
  unmatched <- which(is.na(df_auth_joined$JCR_Journal))
  if (length(unmatched) > 0) {
    cat("Trying fallback substring match for", length(unmatched), "journals...\n")
    for (i in unmatched) {
      jn <- df_auth_joined$Name_norm[i]
      if (is.na(jn) || nchar(jn) < 3) next
      hits <- grep(jn, jcr_names_norm$Name_norm, value = TRUE)
      # print(hits)
      if (length(hits) == 1) {
        idx <- which(jcr_names_norm$Name_norm == hits)[1]
        df_auth_joined$JCR_Journal[i] <- jcr$Name[idx]
        df_auth_joined$Qscore[i] <- jcr$Qscore[idx]
        # df_auth_joined$ISSN[i] <- if ("ISSN" %in% names(jcr)) jcr$ISSN[idx] else NA_character_
        # df_auth_joined$EISSN[i] <- if ("EISSN" %in% names(jcr)) jcr$EISSN[idx] else NA_character_
      }
    }
  }
  
  # If still many unmatched, notify user (they can inspect sortedfile.csv)
  n_unmatched <- length(which(is.na(df_auth_joined$JCR_Journal)))
  print(paste("Number of unmatched journal rows:", n_unmatched, "\n"))
  rv$glens_etable_final <- df_auth_joined
  
}

plot_glens_table <- function(rv, output, session){
  if(nrow(rv$glens_year_filtered) <= 0){
    output$log <- renderText({paste("plot_glens_table() - Warning: No data found for this year range.")})
    warning("plot_glens_table() - Warning: No data found for this year range.")
    shinyjs::hide("sh_index")
    shinyjs::hide("summary_table")
    shinyjs::hide("acounts_plot")
    shinyjs::hide("ccounts_plot")
    shinyjs::hide("cdist_plot")
    shinyjs::hide("aprec_plot")
    shinyjs::hide("cprec_plot")
    shinyjs::hide("extended_table")
    return()
  }
  
  shinyjs::show("acounts_plot")
  shinyjs::show("ccounts_plot")
  shinyjs::show("cdist_plot")
  shinyjs::show("aperc_plot")
  shinyjs::show("cperc_plot")
  
  df_ordered_debug <- rv$glens_year_filtered %>%
    mutate(position_rank = case_when(
      as.numeric(First_Author) == 1 ~ 1L,
      as.numeric(Second_Author) == 1 ~ 2L,
      as.numeric(Co_Author) == 1 ~ 3L,
      as.numeric(Corresponding_Author) == 1 ~ 4L,
      TRUE ~ 99L
    )) %>%
    mutate(adj_cit_for_sort = ifelse(is.na(Adjusted_Citations), -Inf, Adjusted_Citations)) %>%
    arrange(position_rank, desc(adj_cit_for_sort)) %>%
    select(-adj_cit_for_sort)  
  
  df_ordered_debug$position_rank[which(df_ordered_debug$position_rank == 1)] <- "First Author"
  df_ordered_debug$position_rank[which(df_ordered_debug$position_rank == 2)] <- "Second Author"
  df_ordered_debug$position_rank[which(df_ordered_debug$position_rank == 3)] <- "Co-Author"
  df_ordered_debug$position_rank[which(df_ordered_debug$position_rank == 4)] <- "Corresponding Author"
  
  agg_first <- make_agg(df_ordered_debug, "First_Author", "First Author")
  agg_second <- make_agg(df_ordered_debug, "Second_Author", "Second Author")
  agg_co <- make_agg(df_ordered_debug, "Co_Author", "Co-Author")
  agg_cor <- make_agg(df_ordered_debug, "Corresponding_Author", "Corresponding Author")
  
  agg_all <- bind_rows(agg_first, agg_second, agg_co, agg_cor)
  
  # Ensure all quartiles present per position (fill zeros)
  all_positions <- c("First Author","Second Author","Co-Author","Corresponding Author")
  all_quartiles <- c("Q1","Q2","Q3","Q4", "NA")
  full_grid <- expand.grid(Position = all_positions, Qscore = all_quartiles, stringsAsFactors = FALSE)
  agg_all <- full_grid %>%
    left_join(agg_all, by = c("Position","Qscore")) %>%
    mutate(Count = tidyr::replace_na(Count, 0L),
           SumCitations = tidyr::replace_na(SumCitations, 0.0))
  
  # ---------------------------
  # Plotting parameters (colors + alpha per quartile)
  # ---------------------------
  # Base colors per quartile (Q1 -> rich color, Q4 -> light)
  # quartile_colors <- c("Q1" = "#2E8B57",  # greenish (co-author in your sample used green)
  #                      "Q2" = "#4B8BBE",  # blue-ish
  #                      "Q3" = "#B66BA4",  # purple-ish
  #                      "Q4" = "#D6A77A")  # tan (light)
  position_colors <- c("First Author" = "#D6A77A",  # greenish (co-author in your sample used green)
                       "Second Author" = "#B66BA4",  # blue-ish
                       "Co-Author" = "#2E8B57",  # purple-ish
                       "Corresponding Author" = "#4B8BBE")  # tan (light)
  position_border_colors <- c(
    "First Author" = "#ff9f40ff",
    "Second Author" = "#9966ffff",
    "Co-Author" = "#4bc093ff",
    "Corresponding Author" = "#36a2ebff"
  )
  
  # Alpha values so Q1 most opaque and Q4 faint
  quartile_alpha <- c("Q1" = 0.9, "Q2" = 0.70, "Q3" = 0.50, "Q4" = 0.30, "NA" = 0.1)
  
  # Order positions for plotting (Left to right as in your image: First, Second, Co, Corresponding)
  agg_all$Position <- factor(agg_all$Position, levels = all_positions)
  agg_all$Qscore <- factor(agg_all$Qscore, levels = all_quartiles)
  agg_all <- agg_all %>%
    group_by(Position) %>%
    mutate(Total_Position = sum(Count, na.rm = TRUE)) %>%
    ungroup()
  agg_all <- agg_all %>%
    group_by(Position) %>%
    mutate(Total_Citations = sum(SumCitations, na.rm = TRUE)) %>%
    ungroup()
  agg_all <- agg_all %>%
    group_by(Qscore) %>%
    mutate(Total_QCitations = sum(SumCitations, na.rm = TRUE)) %>%
    ungroup()
  
  agg_all <- agg_all %>%
    mutate(
      Position = factor(Position, levels = all_positions),
      Qscore   = factor(Qscore, levels = all_quartiles)
    ) %>%
    arrange(Qscore, Position)
  
  # print(agg_all)
  
  # ---------------------------
  # Create stacked bar chart for Counts
  # ---------------------------
  # p_counts <- ggplot(agg_all, aes(x = Position, y = Count, fill = Position, color = Position, alpha = Qscore, group = Qscore, text = paste(
  #   "Position:", Position,
  #   "<br>Quartile:", Qscore,
  #   "<br>Count:", Count,
  #   "<br>Total:", Total_Position
  #   # "<br>Author Name:", matched_token
  #   # "<br>Citations:", SumCitations
  # ))) +
  #   geom_bar(stat = "identity", size = 0.25) +
  #   scale_colour_manual(
  #     values = position_border_colors,
  #     guide = "none"          # hide border legend
  #   ) +
  #   # #scale_fill_manual(values = quartile_colors, name = "Qscore") +
  #   scale_fill_manual(values = position_colors, 
  #                     # name = "Position"
  #                     guide = "none"
  #   ) +
  #   scale_alpha_manual(values = quartile_alpha,
  #                      # name = "Journal Rank"
  #                      guide = "none"
  #   ) +
  #   theme_minimal(base_size = 12) +
  #   labs(title = "Publication Count based on Authorship with Journal Rank Categorization",
  #        y = NULL, x = NULL) +
  #   theme(
  #     plot.title = element_text(hjust = 0.5, face = "bold"),
  #     axis.text.x = element_text(angle = 15, hjust = 1)
  #   ) + 
  #   scale_color_manual(values = position_colors)
  # # +
  # # geom_text(aes(label = Count), position = position_stack(vjust = 0.5), size = 3, color = "black")
  # # geom_text(data = dplyr::filter(agg_all, Count > 0),aes(label = Count), position = position_stack(vjust = 0.5), size = 3, color = "black")
  
  # p_counts
  # output$acounts_plot <- renderPlotly({ plotly::ggplotly(p_counts, tooltip = "text") %>%
  # plotly::layout(showlegend = FALSE, transition = list(duration = 500))  }) #%>% toWebGL()
  # ---- animate update via plotlyProxy ----
  acounts_proxy <- plotlyProxy("acounts_plot", session)
  acounts_proxy_data <- lapply(all_quartiles, function(q) {
    agg_all %>%
      filter(Qscore == q) %>%
      arrange(Position) %>%
      pull(Count)
  })
  
  plotlyProxyInvoke(
    acounts_proxy,
    "restyle",
    list(
      # y = list(agg_all$Counts)
      y=acounts_proxy_data
    )
  )
  
  # ---------------------------
  # Create stacked bar chart for Sum of Adjusted Citations
  # ---------------------------
  # p_cites <- ggplot(agg_all, aes(x = Position, y = SumCitations, fill = Position, color = Position, alpha = Qscore, group=Qscore, text = paste(
  #   "Position:", Position,
  #   "<br>Position Citations:", Total_Citations,
  #   "<br>Quartile:", Qscore,
  #   "<br>Quartile Citations:", Total_QCitations
  #   # "<br>Citations:", SumCitations
  # ))) +
  #   geom_bar(stat = "identity", size = 0.25) +
  #   scale_colour_manual(
  #     values = position_border_colors,
  #     guide = "none"          # hide border legend
  #   ) +
  #   # scale_fill_manual(values = quartile_colors, name = "Quartile") +
  #   scale_fill_manual(values = position_colors, 
  #                     # name = "Position"
  #                     guide = "none"
  #   ) +
  #   scale_alpha_manual(values = quartile_alpha, 
  #                      name = "Journal Rank"
  #                      # guide = "none"
  #   ) +
  #   theme_minimal(base_size = 12) +
  #   labs(title = "Citation Count based on Authorship with Journal Rank Categorization",
  #        y = NULL, x = NULL) +
  #   theme(
  #     plot.title = element_text(hjust = 0.5, face = "bold"),
  #     axis.text.x = element_text(angle = 15, hjust = 1)
  #   ) #+
  # # geom_text(aes(label = ifelse(SumCitations==0, "", round(SumCitations, 0))), position = position_stack(vjust = 0.5), size = 3, color = "black")
  # # geom_text(data = dplyr::filter(agg_all, Count > 0), aes(label = ifelse(SumCitations==0, "", round(SumCitations, 0))), position = position_stack(vjust = 0.5), size = 3, color = "black")
  
  # p_cites
  # output$ccounts_plot <- renderPlotly({ plotly::ggplotly(p_cites, tooltip = "text") %>%
  # plotly::layout(showlegend = FALSE, transition = list(duration = 500))}) # %>% toWebGL() 
  ccounts_proxy <- plotlyProxy("ccounts_plot", session)
  ccounts_proxy_data <- lapply(all_quartiles, function(q) {
    agg_all %>%
      filter(Qscore == q) %>%
      arrange(Position) %>%
      pull(SumCitations)
  })
  
  plotlyProxyInvoke(
    ccounts_proxy,
    "restyle",
    list(
      # y = list(agg_all$Counts)
      y=ccounts_proxy_data
    )
  )
  
  stats_by_position <- df_ordered_debug %>%
    group_by(position_rank) %>%
    summarise(
      min  = min(Citations, na.rm = TRUE),
      q25  = quantile(Citations, 0.25, na.rm = TRUE),
      med  = median(Citations, na.rm = TRUE),
      mean = mean(Citations, na.rm = TRUE),
      q75  = quantile(Citations, 0.75, na.rm = TRUE),
      max  = max(Citations, na.rm = TRUE),
      .groups = "drop",
    )
  
  df_plot <- df_ordered_debug %>%
    left_join(stats_by_position, by = "position_rank")
  
  df_plot <- df_plot %>%
    mutate(
      position_rank = factor(position_rank),
      Qscore = factor(Qscore),
      Adjusted_Citations = as.numeric(Adjusted_Citations),
      Citations = as.numeric(Citations)
    )
  
  group_counts <- df_plot %>%
    count(position_rank)
  
  # print(df_plot)
  # print(df_plot[,c("Adjustment_Weights","Adjusted_Citations","position_rank", "JIF5Years", "Qscore")]) #"matched_token"
  # print(colnames(df_plot))
  # print(nrow(df_plot))
  # print(group_counts)
  
  if(nrow(df_plot) <= 1 || all(group_counts$n <= 1) || all(df_plot$Citations == 0)){
    rv$log_text <- paste(
      rv$log_text,
      "plot_glens_table() - Warning: Need more than one group and atleast 1 paper with 1 citation per-group for plotting distribution.",
      sep = "\n"
    )
    output$log <- renderText({ rv$log_text })
    warning("plot_glens_table() - Warning: Need more than one group and atleast 1 paper with 1 citation per-group for plotting distribution.")
    shinyjs::hide("cdist_plot")
    return()
  }
  
  # p_citesdist <- ggplot(df_plot, aes(x = position_rank, y = Citations, fill = position_rank, group=position_rank, colour = Qscore,size=Adjusted_Citations, text = paste0(
  #   "<b>Position:</b> ", position_rank,
  #   "<br><b>Citations:</b> ", Citations,
  #   "<br><b>Qscore:</b> ", Qscore,
  #   "<br><b>Author Count:</b> ", Author_Count,
  #   "<br><b>Adjustment Weight:</b> ", Adjustment_Weights,
  #   "<br><b>Adjusted Citations:</b> ", Adjusted_Citations,
  #   "<br><b>Min:</b> ", min,
  #   "<br><b>25%:</b> ", q25,
  #   "<br><b>Median:</b> ", med,
  #   "<br><b>Mean:</b> ", round(mean, 1),
  #   "<br><b>75%:</b> ", q75,
  #   "<br><b>Max:</b> ", max
  # ))) +     geom_violin(alpha = 0.5) +     geom_point(position = position_jitter(seed = 1, width = 0.2)) +     theme(legend.position = "none") + scale_colour_manual(
  #   values = position_border_colors,
  #   guide = "none"          # hide border legend
  # ) +
  #   scale_y_continuous(
  #     trans = "log1p"
  #   ) +
  #   scale_fill_manual(values = position_colors, 
  #                     # name = "Position"
  #                     guide = "none"
  #   ) +
  #   theme_minimal(base_size = 12) +
  #   labs(title = "Citation Distribution based on Authorship (Log Scale)",
  #        y = "log(1 + Citations)", x = NULL) +
  #   theme(
  #     plot.title = element_text(hjust = 0.5, face = "bold"),
  #     axis.text.x = element_text(angle = 15, hjust = 1)
  #   )
  # 
  # # p_citesdist
  # output$cdist_plot <- renderPlotly({ plotly::ggplotly(p_citesdist, tooltip = "text") %>%
  #   plotly::layout(showlegend = FALSE, transition = list(duration = 500)) }) # %>% toWebGL() 
  # --- function to build hover text identical to ggplot's text ---
  make_dist_hover_text <- function(df) {
    return(paste0(
      "<b>Position:</b> ", df$position_rank,
      "<br><b>Citations:</b> ", df$Citations,
      "<br><b>Qscore:</b> ", df$Qscore,
      "<br><b>Author Count:</b> ", df$Author_Count,
      "<br><b>Adjustment Weight:</b> ", df$Adjustment_Weights,
      "<br><b>Adjusted Citations:</b> ", df$Adjusted_Citations,
      "<br><b>Min:</b> ", df$min,
      "<br><b>25%:</b> ", df$q25,
      "<br><b>Median:</b> ", df$med,
      "<br><b>Mean:</b> ", round(df$mean, 1),
      "<br><b>75%:</b> ", df$q75,
      "<br><b>Max:</b> ", df$max
    ))
  }
  
  cdist_proxy <- plotlyProxy("cdist_plot", session)
  for (i in seq_along(all_positions)) {
    pos <- all_positions[i]
    rows <- which(df_plot$position_rank == pos)
    
    # Violin y-values (log1p transform to match ggplot scale)
    y_violin <- if (length(rows) > 0) log1p(df_plot$Citations[rows]) else numeric(0)
    x_violin <- rep(i, length(y_violin))
    
    # Scatter x/y/text/marker.size (jitter x around i)
    n <- length(rows)
    if (n > 0) {
      x_scatter <- i + runif(n, -0.18, 0.18)         # jitter around i
      y_scatter <- log1p(df_plot$Citations[rows]) 
      text_scatter <- make_dist_hover_text(df_plot[rows, , drop = FALSE])
      size_scatter <- (scale(df_plot$Adjusted_Citations[rows]) * 10) + 15   # maybe scale this if too large
    } else {
      x_scatter <- numeric(0); y_scatter <- numeric(0); text_scatter <- character(0); size_scatter <- numeric(0)
    }
    
    # Violin trace index = (i - 1) * 2   (0-based indices)
    violin_trace_idx <- (i - 1) * 2
    # Scatter trace index = (i - 1) * 2 + 1
    scatter_trace_idx <- (i - 1) * 2 + 1
    
    # print("x_violin")
    # print(x_violin)
    # print("y_violin")
    # print(y_violin)
    
    # Update violin 'y' (restyle)
    # Note: plotlyProxyInvoke expects values for the trace; we pass y as a list of values for that trace
    plotlyProxyInvoke(cdist_proxy, "restyle", list(x=list(x_violin),y = list(y_violin)), list(violin_trace_idx))
    
    # Update scatter x, y, text, marker.size
    # For nested properties like marker.size use named element `marker.size` in the props list
    cdist_proxy_data <- list(
      x = list(x_scatter),
      y = list(y_scatter),
      text = list(text_scatter),
      `marker.size` = list(size_scatter)
    )
    plotlyProxyInvoke(cdist_proxy, "restyle", cdist_proxy_data, list(scatter_trace_idx))
    
  } #End - for
  
  total_pubs <- nrow(df_plot)
  
  pub_pdata <- df_plot %>% group_by(position_rank) %>% count() %>% rowwise() %>% mutate(pcontrib=(n/total_pubs) * 100) %>% ungroup()
  print(pub_pdata)
  
  # output$aperc_plot <- renderPlotly(({
  #   # auth_pplot <- ggplot(
  #   #   pub_pdata,
  #   #   aes(
  #   #     x = "Publications",
  #   #     y = pcontrib,
  #   #     fill = position_rank,
  #   #     colour = position_rank,
  #   #     text = paste0(
  #   #       "<b>Position:</b> ", position_rank,
  #   #       "<br><b>Contribution %:</b> ", pcontrib
  #   #     )
  #   #   )
  #   # ) +
  #   #   geom_bar(
  #   #     stat = "identity",
  #   #     width = 0.3,
  #   #     size = 0.6,
  #   #     alpha = 0.7
  #   #   ) +
  #   #   coord_flip() + 
  #   #   scale_fill_manual(values = position_colors, guide = "none") +
  #   #   scale_colour_manual(values = position_border_colors, guide = "none") +
  #   #   theme_minimal(base_size = 12) +
  #   #   labs(
  #   #     title = "Author Contribution in % based on Authorship",
  #   #     x = NULL,
  #   #     y = NULL, #"Contribution (%)",
  #   #     fill = "Position"
  #   #   ) +
  #   #   theme(
  #   #     # axis.text.x = element_blank(),
  #   #     # axis.ticks.x = element_blank(),
  #   #     axis.text.y = element_blank(),
  #   #     plot.title = element_text(hjust = 0.5, face = "bold")
  #   #   )
  #   # 
  #   # # auth_pplot
  #   # plotly::ggplotly(auth_pplot, tooltip = "text") %>%
  #   #   plotly::layout(showlegend = FALSE)  
  #   
  # }))
  
  make_perc_hover_text <- function(df) {
    return(paste0(
      "<b>Position:</b> ", df$position_rank,
      "<br><b>Contribution %:</b> ", df$pcontrib
    ))
  }
  
  req(pub_pdata)
  aperc_proxy <- plotlyProxy("aperc_plot", session)
  
  n <- length(all_positions)
  
  # 1. Calculate values
  aperc_vals <- sapply(all_positions, function(pos) {
    i <- which(pub_pdata$position_rank == pos)
    if (length(i) == 1) pub_pdata$pcontrib[i] else 0
  })
  
  # Normalize to 100%
  if(sum(aperc_vals) > 0) {
    aperc_vals <- (aperc_vals / sum(aperc_vals)) * 100
  }
  
  # 2. Prepare the lists for restyle
  # Restyle expects a list where each element corresponds to a trace
  # Each element itself must be a list containing the data point(s)
  aperc_x_list <- lapply(aperc_vals, function(v) list(v)) 
  # print("aperc_x_list")
  # print(aperc_x_list)
  aperc_y_list <- lapply(seq_len(n), function(i) list("Publications"))
  # print("aperc_y_list")
  # print(aperc_y_list)
  aperc_text_list <- lapply(seq_len(n), function(i) {
    list(paste0(
      "<b>Position:</b> ", all_positions[i],
      "<br><b>Contribution %:</b> ", round(aperc_vals[i], 1), "%"
    ))
  })
  
  # 3. Execute Invoke
  plotlyProxyInvoke(
    aperc_proxy,
    "restyle",
    list(
      x = unname(aperc_x_list),
      y = unname(aperc_y_list),
      text = unname(aperc_text_list),
      textposition = rep(list("inside"), n)
    ),
    as.list(0:(n - 1)) # Trace indices
  )
  
  # if (!is.null(rv$aperc_plot)) {
  #   pb <- plotly_build(rv$aperc_plot)
  #   cat("---- TRACE DEBUG ----\n")
  #   for (i in seq_along(pb$x$data)) {
  #     cat(
  #       "Trace", i-1,
  #       "| name:", pb$x$data[[i]]$name,
  #       "| x:", paste(pb$x$data[[i]]$x, collapse=","),
  #       "| y:", paste(pb$x$data[[i]]$y, collapse=","),
  #       "\n"
  #     )
  #   }
  # }
  
  #   for (i in seq_along(all_positions)) {
  #     # trace index is 0-based
  #     trace_idx <- i - 1
  #     
  #     plotlyProxyInvoke(
  #       aperc_proxy, "restyle",
  #       # set a single x value and the shared y category for this trace
  #       list(
  #         x = list(pub_pdata$pcontrib[i]),                         # ONE value per trace
  #         # y = list("Publications"),                  # same category for all traces
  #         text = list(
  #           paste0(
  #             "<b>Position:</b> ", all_positions[i],
  #             "<br><b>Contribution %:</b> ", round(aperc_vals[i], 1)
  #           )
  #         ),
  #         textposition = list("inside")              # place text inside each segment
  #       ),
  #       trace_idx
  #     )
  #   }
  # # }, once = TRUE)
  
  total_cites <- sum(df_plot$Citations)
  
  cites_pdata <- df_plot %>% group_by(position_rank) %>% summarise(TotalCitations=sum(Citations)) %>% rowwise() %>% mutate(pcontrib=(TotalCitations/total_cites) * 100) %>% ungroup()
  # print(cites_pdata)
  
  req(cites_pdata)
  cperc_proxy <- plotlyProxy("cperc_plot", session)
  
  n <- length(all_positions)
  
  # 1. Map the citation data to match the order of all_positions
  cperc_vals <- sapply(all_positions, function(pos) {
    idx <- which(cites_pdata$position_rank == pos)
    if (length(idx) == 1) cites_pdata$pcontrib[idx] else 0
  })
  
  # Ensure total is 100% (Safety check)
  if(sum(cperc_vals) > 0) {
    cperc_vals <- (cperc_vals / sum(cperc_vals)) * 100
  }
  
  # 2. Build the List-of-Lists (Unnamed)
  cperc_x_list <- lapply(cperc_vals, function(v) list(v))
  cperc_y_list <- lapply(seq_len(n), function(i) list("Citations"))
  cperc_text_list <- lapply(seq_len(n), function(i) {
    list(paste0(
      "<b>Position:</b> ", all_positions[i],
      "<br><b>Contribution %:</b> ", round(cperc_vals[i], 1), "%"
    ))
  })
  
  # 3. Single Update Call
  plotlyProxyInvoke(
    cperc_proxy,
    "restyle",
    list(
      x = unname(cperc_x_list),
      y = unname(cperc_y_list),
      text = unname(cperc_text_list),
      textposition = rep(list("inside"), n)
    ),
    as.list(0:(n - 1))
  )# End - for
  
  print(cites_pdata)
  print(sum(cites_pdata$pcontrib))
  # print(str(cperc_proxy))
  
  # cites_pplot <- ggplot(
  #   cites_pdata,
  #   aes(
  #     x = "Publications",
  #     y = pcontrib,
  #     fill = position_rank,
  #     colour = position_rank,
  #     text = paste0(
  #       "<b>Position:</b> ", position_rank,
  #       "<br><b>Contribution %:</b> ", pcontrib
  #     )
  #   )
  # ) +
  #   geom_bar(
  #     stat = "identity",
  #     width = 0.3,
  #     size = 0.6,
  #     alpha = 0.7
  #   ) +
  #   coord_flip() + 
  #   scale_fill_manual(values = position_colors, guide = "none") +
  #   scale_colour_manual(values = position_border_colors, guide = "none") +
  #   theme_minimal(base_size = 12) +
  #   labs(
  #     title = "Citation Contribution in % based on Authorship",
  #     x = NULL,
  #     y = NULL, #"Contribution (%)",
  #     fill = "Position"
  #   ) +
  #   theme(
  #     # axis.text.x = element_blank(),
  #     # axis.ticks.x = element_blank(),
  #     axis.text.y = element_blank(),
  #     plot.title = element_text(hjust = 0.5, face = "bold")
  #   )
  # 
  # cites_pplot
  # plotly::ggplotly(cites_pplot, tooltip = "text") %>%
  #   plotly::layout(showlegend = FALSE)
} #End - Plotting

render_skeleton_plots <- function(rv, output){
  df_ordered_debug <- rv$glens_year_filtered %>%
    mutate(position_rank = case_when(
      as.numeric(First_Author) == 1 ~ 1L,
      as.numeric(Second_Author) == 1 ~ 2L,
      as.numeric(Co_Author) == 1 ~ 3L,
      as.numeric(Corresponding_Author) == 1 ~ 4L,
      TRUE ~ 99L
    )) %>%
    mutate(adj_cit_for_sort = ifelse(is.na(Adjusted_Citations), -Inf, Adjusted_Citations)) %>%
    arrange(position_rank, desc(adj_cit_for_sort)) %>%
    select(-adj_cit_for_sort)  
  
  df_ordered_debug$position_rank[which(df_ordered_debug$position_rank == 1)] <- "First Author"
  df_ordered_debug$position_rank[which(df_ordered_debug$position_rank == 2)] <- "Second Author"
  df_ordered_debug$position_rank[which(df_ordered_debug$position_rank == 3)] <- "Co-Author"
  df_ordered_debug$position_rank[which(df_ordered_debug$position_rank == 4)] <- "Corresponding Author"
  
  agg_first <- make_agg(df_ordered_debug, "First_Author", "First Author")
  agg_second <- make_agg(df_ordered_debug, "Second_Author", "Second Author")
  agg_co <- make_agg(df_ordered_debug, "Co_Author", "Co-Author")
  agg_cor <- make_agg(df_ordered_debug, "Corresponding_Author", "Corresponding Author")
  
  agg_all <- bind_rows(agg_first, agg_second, agg_co, agg_cor)
  
  # Ensure all quartiles present per position (fill zeros)
  all_positions <- c("First Author","Second Author","Co-Author","Corresponding Author")
  all_quartiles <- c("Q1","Q2","Q3","Q4", "NA")
  full_grid <- expand.grid(Position = all_positions, Qscore = all_quartiles, stringsAsFactors = FALSE)
  agg_all <- full_grid %>%
    left_join(agg_all, by = c("Position","Qscore")) %>%
    mutate(Count = tidyr::replace_na(Count, 0L),
           SumCitations = tidyr::replace_na(SumCitations, 0.0))
  
  
  # ---------------------------
  # Plotting parameters (colors + alpha per quartile)
  # ---------------------------
  # Base colors per quartile (Q1 -> rich color, Q4 -> light)
  # quartile_colors <- c("Q1" = "#2E8B57",  # greenish (co-author in your sample used green)
  #                      "Q2" = "#4B8BBE",  # blue-ish
  #                      "Q3" = "#B66BA4",  # purple-ish
  #                      "Q4" = "#D6A77A")  # tan (light)
  position_colors <- c("First Author" = "#D6A77A",  # greenish (co-author in your sample used green)
                       "Second Author" = "#B66BA4",  # blue-ish
                       "Co-Author" = "#2E8B57",  # purple-ish
                       "Corresponding Author" = "#4B8BBE")  # tan (light)
  position_border_colors <- c(
    "First Author" = "#ff9f40ff",
    "Second Author" = "#9966ffff",
    "Co-Author" = "#4bc093ff",
    "Corresponding Author" = "#36a2ebff"
  )
  
  position_45plots <- c("First Author" = "#dbf2f2",
                        "Second Author" = "#ebe0ff",
                        "Co-Author" = "#ffe9d4",
                        "Corresponding Author" = "#d7ecfb")
  
  position_45plots_border <- c("First Author" = "#8fcedb",
                               "Second Author" = "#cfaeec",
                               "Co-Author" = "#fbb774",
                               "Corresponding Author" = "#7dc2f1")
  
  # Alpha values so Q1 most opaque and Q4 faint
  quartile_alpha <- c("Q1" = 0.9, "Q2" = 0.70, "Q3" = 0.50, "Q4" = 0.30, "NA" = 0.1)
  
  # Order positions for plotting (Left to right as in your image: First, Second, Co, Corresponding)
  agg_all$Position <- factor(agg_all$Position, levels = all_positions)
  agg_all$Qscore <- factor(agg_all$Qscore, levels = all_quartiles)
  agg_all <- agg_all %>%
    group_by(Position) %>%
    mutate(Total_Position = sum(Count, na.rm = TRUE)) %>%
    ungroup()
  agg_all <- agg_all %>%
    group_by(Position) %>%
    mutate(Total_Citations = sum(SumCitations, na.rm = TRUE)) %>%
    ungroup()
  agg_all <- agg_all %>%
    group_by(Qscore) %>%
    mutate(Total_QCitations = sum(SumCitations, na.rm = TRUE)) %>%
    ungroup()
  
  agg_all <- agg_all %>%
    mutate(
      Position = factor(Position, levels = all_positions),
      Qscore   = factor(Qscore, levels = all_quartiles)
    ) %>%
    arrange(Qscore, Position)
  # Ensure factor order matches ggplot
  agg_all$Position <- factor(
    agg_all$Position,
    levels = all_positions
  )
  
  agg_all$Qscore <- factor(
    agg_all$Qscore,
    levels = all_quartiles
  )
  
  # print(agg_all)
  
  # stats_by_position <- df_ordered_debug %>%
  #   group_by(position_rank) %>%
  #   summarise(
  #     min  = min(Citations, na.rm = TRUE),
  #     q25  = quantile(Citations, 0.25, na.rm = TRUE),
  #     med  = median(Citations, na.rm = TRUE),
  #     mean = mean(Citations, na.rm = TRUE),
  #     q75  = quantile(Citations, 0.75, na.rm = TRUE),
  #     max  = max(Citations, na.rm = TRUE),
  #     .groups = "drop",
  #   )
  # 
  # df_plot <- df_ordered_debug %>%
  #   left_join(stats_by_position, by = "position_rank")
  # 
  # df_plot <- df_plot %>%
  #   mutate(
  #     position_rank = factor(position_rank),
  #     Qscore = factor(Qscore),
  #     Adjusted_Citations = as.numeric(Adjusted_Citations),
  #     Citations = as.numeric(Citations)
  #   )
  # 
  # group_counts <- df_plot %>%
  #   count(position_rank)
  
  # print(df_plot)
  # print(df_plot[,c("Adjustment_Weights","Adjusted_Citations","position_rank", "JIF5Years", "Qscore")]) #"matched_token"
  # print(colnames(df_plot))
  # print(nrow(df_plot))
  # print(group_counts)
  
  output$acounts_plot <- renderPlotly({
    
    rv$acounts_plotly <-  plot_ly(
      data = agg_all,
      x = ~Position,
      y = ~Count,
      split = ~Qscore,              # stacked bars by quartile
      type = "bar",
      hoverinfo = "text",
      hovertext = ~paste(
        "Position:", Position,
        "<br>Quartile:", Qscore,
        "<br>Count:", Count,
        "<br>Total:", Total_Position
      ),
      marker = list(
        color = ~position_colors[Position],
        opacity = ~quartile_alpha[Qscore],
        line = list(
          width = 1,
          color = ~position_border_colors[Position]
        )
      )
    ) %>%
      layout(
        title = list(
          text = "Publication Count based on Authorship with Journal Rank Categorization",
          x = 0.5
        ),
        barmode = "stack",
        xaxis = list(
          title = "",
          tickangle = 15
        ),
        yaxis = list(
          title = ""
        ),
        showlegend = FALSE,
        transition = list(
          duration = 1000,
          easing = "ease-in-out" #"cubic-in-out"
        )
      )
    
    # pb <- plotly_build(acounts_plotly)
    # print(str(pb$x$data))
    rv$acounts_plotly
  })
  
  output$ccounts_plot <- renderPlotly({
    # p_cites <- ggplot(agg_all, aes(x = Position, y = SumCitations, fill = Position, color = Position, alpha = Qscore, group=Qscore, text = paste(
    #   "Position:", Position,
    #   "<br>Position Citations:", Total_Citations,
    #   "<br>Quartile:", Qscore,
    #   "<br>Quartile Citations:", Total_QCitations
    #   # "<br>Citations:", SumCitations
    # ))) +
    #   geom_bar(stat = "identity", size = 0.25) +
    #   scale_colour_manual(
    #     values = position_border_colors,
    #     guide = "none"          # hide border legend
    #   ) +
    #   # scale_fill_manual(values = quartile_colors, name = "Quartile") +
    #   scale_fill_manual(values = position_colors, 
    #                     # name = "Position"
    #                     guide = "none"
    #   ) +
    #   scale_alpha_manual(values = quartile_alpha, 
    #                      name = "Journal Rank"
    #                      # guide = "none"
    #   ) +
    #   theme_minimal(base_size = 12) +
    #   labs(title = "Citation Count based on Authorship with Journal Rank Categorization",
    #        y = NULL, x = NULL) +
    #   theme(
    #     plot.title = element_text(hjust = 0.5, face = "bold"),
    #     axis.text.x = element_text(angle = 15, hjust = 1)
    #   ) 
    
    rv$ccounts_plotly <-  plot_ly(
      data = agg_all,
      x = ~Position,
      y = ~SumCitations,
      split = ~Qscore,              # stacked bars by quartile
      type = "bar",
      hoverinfo = "text",
      hovertext = ~paste(
        "Position:", Position,
        "<br>Position Citations:", Total_Citations,
        "<br>Quartile:", Qscore,
        "<br>Quartile Citations:", Total_QCitations
      ),
      marker = list(
        color = ~position_colors[Position],
        opacity = ~quartile_alpha[Qscore],
        line = list(
          width = 1,
          color = ~position_border_colors[Position]
        )
      )
    ) %>%
      layout(
        title = list(
          text = "Citation Count based on Authorship with Journal Rank Categorization",
          x = 0.5
        ),
        barmode = "stack",
        xaxis = list(
          title = "",
          tickangle = 15
        ),
        yaxis = list(
          title = ""
        ),
        showlegend = FALSE,
        transition = list(
          duration = 1000,
          easing = "ease-in-out" #"cubic-in-out"
        )
      )
    
    # pb <- plotly_build(ccounts_plotly)
    # print(str(pb$x$data))
    rv$ccounts_plotly
  })
  
  # p_citesdist <- ggplot(df_plot, aes(x = position_rank, y = Citations, fill = position_rank, group=position_rank, colour = Qscore,size=Adjusted_Citations, text = paste0(
  #   "<b>Position:</b> ", position_rank,
  #   "<br><b>Citations:</b> ", Citations,
  #   "<br><b>Qscore:</b> ", Qscore,
  #   "<br><b>Author Count:</b> ", Author_Count,
  #   "<br><b>Adjustment Weight:</b> ", Adjustment_Weights,
  #   "<br><b>Adjusted Citations:</b> ", Adjusted_Citations,
  #   "<br><b>Min:</b> ", min,
  #   "<br><b>25%:</b> ", q25,
  #   "<br><b>Median:</b> ", med,
  #   "<br><b>Mean:</b> ", round(mean, 1),
  #   "<br><b>75%:</b> ", q75,
  #   "<br><b>Max:</b> ", max
  # ))) +     geom_violin(alpha = 0.5) +     geom_point(position = position_jitter(seed = 1, width = 0.2)) +     theme(legend.position = "none") + scale_colour_manual(
  #   values = position_border_colors,
  #   guide = "none"          # hide border legend
  # ) +
  #   scale_y_continuous(
  #     trans = "log1p"
  #   ) +
  #   scale_fill_manual(values = position_colors, 
  #                     # name = "Position"
  #                     guide = "none"
  #   ) +
  #   theme_minimal(base_size = 12) +
  #   labs(title = "Citation Distribution based on Authorship (Log Scale)",
  #        y = "log(1 + Citations)", x = NULL) +
  #   theme(
  #     plot.title = element_text(hjust = 0.5, face = "bold"),
  #     axis.text.x = element_text(angle = 15, hjust = 1)
  #   )
  
  # if(nrow(df_plot) <= 1 || all(group_counts$n <= 1) || all(df_plot$Citations == 0)){
  #   rv$log_text <- paste(
  #     rv$log_text,
  #     "plot_glens_table() - Warning: Need more than one group and atleast 1 paper with 1 citation per-group for plotting distribution.",
  #     sep = "\n"
  #   )
  #   output$log <- renderText({ rv$log_text })
  #   warning("plot_glens_table() - Warning: Need more than one group and atleast 1 paper with 1 citation per-group for plotting distribution.")
  #   shinyjs::hide("cdist_plot")
  #   return()
  # }
  
  output$cdist_plot <- renderPlotly({
    p <- plot_ly()
    
    for (i in seq_along(all_positions)) {
      pos <- all_positions[i]
      fill_col <- position_colors[pos]
      border_col <- position_border_colors[pos]
      
      # Violin trace for this position (empty skeleton y)
      p <- add_trace(p,
                     # x = list(i),        # category position as numeric
                     # y = numeric(0),     # empty skeleton
                     # type = "violin",
                     # name = pos,
                     # side = "both",
                     # spanmode = "hard",
                     # box = list(visible = FALSE),
                     # meanline = list(visible = TRUE),
                     # fillcolor = fill_col,
                     # line = list(color = border_col),
                     # opacity = 0.5,
                     # points = FALSE,
                     type = "violin",
                     orientation = "v",
                     width = 0.7,
                     scalemode = "width",
                     spanmode = "hard",
                     box = list(visible = FALSE),
                     meanline = list(visible = TRUE),
                     points = FALSE,
                     showlegend = FALSE,
                     hoverinfo = "none"   # we rely on scatter points for hover
      )
      
      # Scatter (jittered points) for this position (empty skeleton)
      p <- add_trace(p,
                     x = numeric(0),
                     y = numeric(0),
                     type = "scatter",
                     mode = "markers",
                     name = pos,
                     marker = list(size = numeric(0), color = border_col, line = list(width = 0.5, color = border_col)),
                     text = character(0),
                     hoverinfo = "text",
                     showlegend = FALSE
      )
    }
    
    rv$cdist_plotly <- p %>%
      layout(
        title = list(text = "Citation Distribution based on Authorship (Log Scale)", x = 0.5),
        yaxis = list(title = "log(1 + Citations)"),
        xaxis = list(title = "", tickmode = "array", tickvals = seq_along(all_positions), ticktext = all_positions),
        showlegend = FALSE
      )
    rv$cdist_plotly
  })
  
  output$aperc_plot <- renderPlotly({
    p <- plot_ly()
    
    for (pos in all_positions) {
      p <- add_trace(
        p,
        type = "bar",
        orientation = "h",
        x = 0,               # Initialize with 0 instead of numeric(0)
        y = "Publications",  # Give it the actual category name immediately
        name = pos,
        marker = list(
          color = position_45plots[pos],
          line = list(color = position_45plots_border[pos], width = 1)
        ),
        hoverinfo = "text",
        
        showlegend = FALSE
      )
    }
    
    p %>% layout(
      barmode = "stack",
      xaxis = list(
        title = "", 
        range = c(0, 100), 
        dtick = 10, 
        showgrid = TRUE,
        ticksuffix = "%"
        
      ),
      yaxis = list(
        title = "", 
        showticklabels = FALSE, 
        fixedrange = TRUE
      ),
      margin = list(l = 10, r = 10, t = 50, b = 30),
      title = list(text = "Author Contribution in % based on Authorship", x = 0.5)
    )
  })
  
  output$cperc_plot <- renderPlotly({
    p <- plot_ly()
    for (pos in all_positions) {
      p <- add_trace(
        p,
        type = "bar",
        orientation = "h",
        x = 0,               # Start at 0
        y = "Citations",     # Pre-define the category
        name = pos,
        marker = list(
          color = position_45plots[pos],
          line = list(color = position_45plots_border[pos], width = 0.8)
        ),
        hoverinfo = "text",
        
        showlegend = FALSE
      )
    }
    
    p %>% layout(
      barmode = "stack",
      xaxis = list(title = "", range = c(0, 100), dtick = 10, ticksuffix = "%"),
      yaxis = list(title = "", showticklabels = FALSE, fixedrange = TRUE),
      margin = list(l = 20, r = 20, t = 50, b = 30),
      title = list(text = "Citation Contribution in % based on Authorship", x = 0.5)
    )
  })
  
} # End - Plot skeleton renders

server <- function(input, output, session) {
  rv <- reactiveValues(
    glens_input_table = data.frame(),
    glens_etable_final = data.frame(),
    glens_year_filtered = data.frame(),
    summary_table = data.frame(),
    scopus_df = data.frame(),
    # scopus_future = future({}),
    wos_df = data.frame(),
    semantic_df = data.frame(),
    sh_index = 0,
    is_glens_exec = F,
    is_cancelled = F,
    log_text = NULL,
    acounts_plotly = NULL,
    ccounts_plotly = NULL,
    cdist_plotly = NULL,
    aperc_plotly = NULL,
    cperc_plotly = NULL,
  )
  shinyjs::hide(id =  "year_slider")
  shinyjs::hide("sh_index")
  shinyjs::hide("summary_table")
  shinyjs::hide("acounts_plot")
  shinyjs::hide("ccounts_plot")
  shinyjs::hide("cdist_plot")
  shinyjs::hide("aperc_plot")
  shinyjs::hide("cperc_plot")
  shinyjs::hide("extended_table")
  shinyjs::hide("progress_overlay") # Reveal the bar
  # shinyWidgets::updateProgressBar(
  #   session, 
  #   id = "doi_progress", 
  #   value = 0, 
  #   total = doi_count,
  #   title = sprintf("Starting calculation for %d DOIs...", doi_count)
  # )
  
  output$log <- renderText({
    files <- list.files(getwd(), all.files = TRUE, recursive = TRUE)
    paste(
      "Current working dir:", getwd(),
      "\n\nFiles in VFS:\n", 
      paste(files, collapse = "\n")
    )
  })
  
  #light to dark mode and vice versa
  observeEvent(input$theme_toggle, {
    shinyjs::toggleClass(selector = "body", class = "dark-mode")
    
    # Update button label and icon
    if (input$theme_toggle %% 2 == 1) {
      updateActionButton(session, "theme_toggle", label = "☀️ Light Mode", icon = icon("sun", lib = "font-awesome"))
    } else {
      updateActionButton(session, "theme_toggle", label = "🌙 Dark Mode", icon = icon("moon", lib = "font-awesome"))
    }
  })
  
  observeEvent(input$cancel_button, {
    rv$is_cancelled <- TRUE
    print("HERE1")
    # Immediately hide the overlay and re-enable the UI
    shinyjs::hide("progress_overlay")
    shinyjs::enable(id = "submit_button")
    
    # Update logs
    rv$log_text <- paste0("Process cancelled by user.\n")
    output$log <- renderText({ rv$log_text })
  })
  
  observeEvent(input$settings_btn, {
    # Check file existence first to set badge states
    has_scopus <- fs::file_exists(file.path("keys","scopus.key"))
    has_wos <- fs::file_exists(file.path("keys","wos.key"))
    has_semantic <- fs::file_exists(file.path("keys","semantic.key"))
    
    showModal(modalDialog(
      title = tags$span(icon("gears", lib = "font-awesome"), " API Configuration Settings"),
      size = "m",
      
      # Scopus
      tags$div(class = "api-row",
               tags$div(class = "label-container",
                        tags$label("Scopus API Key:", style="margin-bottom:0;"),
                        if(has_scopus) tags$span(class="status-badge badge-found", icon("check", lib = "font-awesome"), " Key Found") 
                        else tags$span(class="status-badge badge-missing", "Missing")
               ),
               tags$div(class = "input-button-group",
                        passwordInput("scopus_key", label = NULL, placeholder = "Enter Scopus Key", width = "100%"),
                        tags$div(class = "api-save-wrap",
                                 actionButton("save_scopus", "Save Scopus Key", class = "btn-success save-btn-custom")
                        )
               )
      ),
      
      # Web of Science
      tags$div(class = "api-row",
               tags$div(class = "label-container",
                        tags$label("Web of Science API Key:", style="margin-bottom:0;"),
                        if(has_wos) tags$span(class="status-badge badge-found", icon("check", lib = "font-awesome"), " Key Found") 
                        else tags$span(class="status-badge badge-missing", "Missing")
               ),
               tags$div(class = "input-button-group",
                        passwordInput("wos_key", label = NULL, placeholder = "Enter Web of Science Key", width = "100%"),
                        tags$div(class = "api-save-wrap",
                                 actionButton("save_wos", "Save Web of Science Key", class = "btn-success save-btn-custom")
                        )
               )
      ),
      
      # Example Row: Semantic Scholar
      tags$div(class = "api-row",
               tags$div(class = "label-container",
                        tags$label("Semantic Scholar API Key:", style="margin-bottom:0;"),
                        if(has_semantic) tags$span(class="status-badge badge-found", icon("check", lib = "font-awesome"), " Key Found") 
                        else tags$span(class="status-badge badge-missing", "Missing")
               ),
               tags$div(class = "input-button-group",
                        passwordInput("semantic_key", label = NULL, placeholder = "Enter Semantic Scholar Key", width = "100%"),
                        tags$div(class = "api-save-wrap",
                                 actionButton("save_semantic", "Save Semantic Scholar Key", class = "btn-success save-btn-custom")
                        )
               )
      ),
      
      footer = modalButton("Close Settings"),
      easyClose = TRUE
    ))
    
    # check for files and update the fields
    if(has_scopus){
      glens_env$scopus_key <- sodium::data_decrypt(readRDS(file.path("keys","scopus.key")), key=sha256(glens_env$privkey_dec))
      updateTextInput(session, "scopus_key", value = trimws(rawToChar(glens_env$scopus_key)))
    }
    if(has_wos){
      glens_env$wos_key <- sodium::data_decrypt(readRDS(file.path("keys","wos.key")), key=sha256(glens_env$privkey_dec))
      updateTextInput(session, "wos_key", value = trimws(rawToChar(glens_env$wos_key)))
    }
    if(has_semantic){
      glens_env$semantic_key <- sodium::data_decrypt(readRDS(file.path("keys","semantic.key")), key=sha256(glens_env$privkey_dec))
      updateTextInput(session, "semantic_key", value = trimws(rawToChar(glens_env$semantic_key)))
    }
  })
  
  # SCOPUS save handlers (repeat for WoS and Semantic)
  observeEvent(input$save_scopus, {
    req(input$scopus_key)
    raw_key <- charToRaw(trimws(input$scopus_key))
    # print(input$scopus_key)
    # print(raw_key)
    # print(glens_env$privkey_dec)
    # print(sha256(glens_env$privkey_dec))
    encrypted_scopus <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_scopus, file = file.path("keys","scopus.key"))
    showNotification("Scopus Key Encrypted and Saved.", type = "message")
    removeModal()
  })
  observeEvent(input$save_wos, {
    fs::dir_create("keys")
    req(input$wos_key)
    raw_key <- charToRaw(trimws(input$wos_key))
    encrypted_wos <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_wos, file = file.path("keys","wos.key"))
    showNotification("Web of Science Key Encrypted and Saved.", type = "message")
    removeModal()
  })
  observeEvent(input$save_semantic, {
    fs::dir_create("keys")
    req(input$semantic_key)
    raw_key <- charToRaw(trimws(input$semantic_key))
    encrypted_semantic <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_semantic, file = file.path("keys","semantic.key"))
    showNotification("Semantic Scholar Key Encrypted and Saved.", type = "message")
    removeModal()
  })
  
  #Slider Event
  observeEvent(c(input$selected_source, input$year_slider), {
    req(rv$glens_etable_final, input$selected_source, input$year_slider)
    if(nrow(rv$glens_input_table)<=0){
      return()
    }
    if(is.na(input$year_slider[1]) || is.na(input$year_slider[2])){
      return()
    }
    if(rv$is_glens_exec){
      warning("input$year_slider - Warning: Executing.")
      return()
    }
    shinyjs::disable(id="year_slider")
    # print("Changed range...")
    # output$log <- renderText("Changed range...")
    # extend_input_table(rv)
    # rv$glens_year_filtered <- rv$glens_etable_final %>%
    #   filter(Year >= input$year_slider[1]) %>%
    #   filter(Year <= input$year_slider[2])
    #   # filter(dplyr::between(
    #   #   Year,
    #   #   input$year_slider[1],
    #   #   input$year_slider[2]
    #   # ))
    rv$glens_year_filtered <- rv$glens_etable_final %>%
      filter(
        Year >= input$year_slider[1],
        Year <= input$year_slider[2],
        Source == input$selected_source # The new source-based filter logic
      )
    
    # print("rv$glens_etable_final===>")
    # print(rv$glens_etable_final %>%
    #         filter(Year >= input$year_slider[1],
    #                Year <= input$year_slider[2]))
    if(nrow(rv$glens_year_filtered) <= 0){
      output$log <- renderText({paste("input$year_slider - Warning: No data found for this year range.")})
      warning("input$year_slider - Warning: No data found for this year range.")
      shinyjs::hide("sh_index")
      shinyjs::hide("summary_table")
      shinyjs::hide("acounts_plot")
      shinyjs::hide("ccounts_plot")
      shinyjs::hide("cdist_plot")
      shinyjs::hide("aprec_plot")
      shinyjs::hide("cprec_plot")
      shinyjs::hide("extended_table")
      shinyjs::enable(id="year_slider")
      return()
    }
    
    rv$log_text <- paste("(Slider:", input$year_slider[1], "-", input$year_slider[2],")","Filtered years to range...", min(rv$glens_year_filtered$Year), "and",max(rv$glens_year_filtered$Year)
    )
    
    output$log <- renderText({ rv$log_text })
    print(paste("(Slider:", input$year_slider[1], "-", input$year_slider[2],")","Filtered years to range...", min(rv$glens_year_filtered$Year), "and",max(rv$glens_year_filtered$Year)))
    # print(str(rv$glens_year_filtered$Year))
    
    compute_indices(rv)
    
    # output$extended_table <- renderTable(rv$glens_year_filtered, striped = TRUE)
    output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
    # output$extended_table <- DT::renderDataTable({
    #   datatable(
    #     rv$glens_year_filtered,
    #     options = list(
    #       scrollY = "600px",
    #       scrollX = TRUE,
    #       paging = TRUE
    #     )
    #   )
    # })
    output$extended_table <- DT::renderDataTable({
      datatable(
        rv$glens_year_filtered,
        extensions = 'Buttons', # 1. Load the extension
        options = list(
          scrollY = "600px",
          scrollX = TRUE,
          paging = TRUE,
          dom = 'Blfrtip',       # 2. Add 'B' to the layout (B = Buttons)
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print') # 3. Define buttons
        )
      )
    })
    
    output$sh_index <- renderUI({
      # Only render if sh_index exists and is not NULL
      req(rv$sh_index) 
      
      tags$div(
        class = "sh-index-container", # Uses the CSS class for the badge look
        style = "display: inline-flex; align-items: baseline; background-color: #f0f7ff; 
             padding: 10px 18px; border-radius: 8px; border: 1px solid #cce4fc; 
             margin-top: 5px;",
        
        # Label part
        tags$span(
          "Sh-Index", 
          style = "color: #4a5568; font-size: 13px; font-weight: 600; text-transform: uppercase; 
               letter-spacing: 0.5px; margin-right: 12px;"
        ),
        
        # Value part (Large and Bold)
        tags$span(
          rv$sh_index, 
          style = "color: #4B8BBE; font-size: 26px; font-weight: 800; line-height: 1;"
        )
      )
    })
    
    plot_glens_table(rv, output, session)
    
    shinyjs::enable(id="year_slider")
  })
  #Submit Button Event
  observeEvent(input$submit_button, {   # same as bindEvent(input$submit_button)
    # basic input guard
    rv$is_cancelled <- FALSE
    rv$is_glens_exec <- T
    rv$log_text <- ""
    rv$glens_etable_final <- NULL
    rv$glens_year_filtered <- NULL
    # rv$scopus_df <- NULL
    # rv$wos_df <- NULL
    # rv$semantic_df <- NULL

    
    # check_orcid_input <- F
    # input_is_orcid <- F
    if (is.null(input$author_list) || stringi::stri_isempty(input$author_list)) {
      rv$log_text <- paste(rv$log_text, "Author list required.\n")
      # #INPUT IS PROLLY ORCID
      # check_orcid_input <- T
      output$log <- renderText({rv$log_text})
      shinyjs::enable(id = "submit_button")
      shinyjs::hide("progress_overlay")
      rv$is_glens_exec <- F
      req(input$author_list)
      return()
    }
    
    shinyjs::disable(id = "submit_button")
    shinyjs::hide(id="year_slider")
    shinyjs::show("progress_overlay")
    
    if (is.null(input$doi_text) || stringi::stri_isempty(input$doi_text)) {
      rv$log_text <- paste(rv$log_text, "No DOIs provided in Input.\n")
      #INPUT IS PROLLY ORCID
      # check_orcid_input <- T
    }
    orcid_list <- str_split(input$orcid_text, "\n")[[1]]
    print(orcid_list)
    # print(length(orcid_list))
    # if(check_orcid_input){
    if (is.null(input$orcid_text) || stringi::stri_isempty(input$orcid_text) || length(orcid_list) == 0) {
      rv$log_text <- paste(rv$log_text, "Empty ORC-ID input.\n")
      output$log <- renderText({rv$log_text})
      # shinyjs::enable(id = "submit_button")
      # rv$is_glens_exec <- F
      # return()
    } 
    #   input_is_orcid <- T
    # }
    
    doi_lines <- c()
    print(orcid_list)
    if(length(orcid_list) == 1 && stringi::stri_isempty(orcid_list)){
      orcid_list<- list()
    }
    # if(input_is_orcid){
    if(length(orcid_list) > 0){
      rv$log_text <- paste(rv$log_text, "ORC-ID(s) provided as input\n")
      output$log <- renderText({rv$log_text})
      
      # "api."(input$orcid_text)
      # "0000-0002-2861-7446" #test orcid
      user_agent_str = paste0("R (", R.version$version.string, ")")
      
      orcid2doi_table <- dplyr::bind_rows(sapply(seq_len(length(orcid_list)), function(x){
        print(str_split(orcid_list[x],"-")[[1]])
        if(length(str_split(orcid_list[x],"-")[[1]]) != 4){
          rv$log_text <- paste(rv$log_text, "Malformed ORCID:", orcid_list[x], "\n")
          output$log <- renderText({rv$log_text})
          # shinyjs::enable(id = "submit_button")
          return()
        }
        xml_txt <- NULL
        clean_orcid <- trimws(as.character(orcid_list[x]))
        target_url <- paste0("https://pub.orcid.org/v3.0/", clean_orcid, "/works")
        if(!is_WASM){
          # 1. Build the httr2 request
          req <- request(target_url) %>%
            req_headers(Accept = "application/xml") %>%
            req_timeout(60) %>%
            req_error(is_error = ~ FALSE) # CRITICAL: Prevents R from halting on 404/500 errors
          
          # 2. Perform the request, safely catching total network disconnections
          res <- tryCatch(req_perform(req), error = function(e) e)
          
          # 3. Check if 'res' is a valid httr2 response AND has a 200 OK status
          if (inherits(res, "httr2_response") && resp_status(res) == 200) {
            
            # Extract as text (httr2 defaults to UTF-8 automatically)
            xml_txt <- resp_body_string(res)
            
          } else {
            
            # 1. Safely determine the status code (or note if it was a connection drop)
            status_val <- if (inherits(res, "httr2_response")) {
              resp_status(res) 
            } else {
              "Network/Connection Error"
            }
            
            # 2. Safely extract the response body OR the error message
            res_val <- if (inherits(res, "httr2_response")) {
              # If it's a 401/404, get the body to see the API's complaint
              resp_body_string(res) 
            } else if (inherits(res, "condition")) {
              # If the internet dropped, get the curl error message
              conditionMessage(res) 
            } else {
              as.character(res)
            }
            
            # 3. Update the Shiny log
            rv$log_text <- paste(
              rv$log_text, 
              "Couldn't find ORC-ID:", clean_orcid,
              "\nStatus:", status_val,
              "\n---"
            )
            
            output$log <- renderText({rv$log_text})
            return()
          }
        }else{ #If WASM using JS fetch()
          res <- tryCatch({
            # Open a connection, passing the Accept header as a named character vector
            con <- url(target_url, headers = c(Accept = "application/xml"))
            
            # Read the lines, close the connection, and collapse into a single string
            lines <- readLines(con, warn = FALSE)
            close(con)
            paste(lines, collapse = "\n")
          }, error = function(e) {
            # If the connection fails, close it safely just in case and return the error
            if (exists("con")) try(close(con), silent = TRUE)
            return(e)
          })
          
          # Handle the result
          if (inherits(res, "error")) {
            error_msg <- conditionMessage(res)
            rv$log_text <- paste(rv$log_text, "\nNetwork Error for ORC-ID:", clean_orcid, "\nDetails:", error_msg)
            output$log <- renderText({rv$log_text})
            return()
          } else {
            # Success! 'res' is pure character text containing your XML
            xml_txt <- res
            message("Successfully fetched XML!")
          }
        }
        
        xml_vec <- xml2::read_xml(xml_txt)
        
        xml_vec_ns <- xml2::xml_ns(xml_vec)
        
        xml_groups <- xml2::xml_find_all(xml_vec, ".//activities:group", xml_vec_ns)
        # print(length(xml_vec))
        # print(length(xml_groups))
        
        orcid_df <- purrr::map_dfr(xml_groups, function(g) {
          tibble(
            source_name = xtext(g, ".//common:source-name", xml_vec_ns),
            title = xtext(g, ".//common:title", xml_vec_ns),
            external_id_value = xtext(g, ".//common:external-id-value", xml_vec_ns),
            external_id_url = xtext(g, ".//common:external-id-url", xml_vec_ns),
            last_modified_date = xtext(g, ".//common:last-modified-date", xml_vec_ns),
            journal_title = xtext(g, ".//work:journal-title", xml_vec_ns),
            work_type = xtext(g, ".//work:type", xml_vec_ns)
          )
        })
        
        # print(colnames(orcid_df))
        # print(head(orcid_df))
        
        #FETCHING SCOPUS
        scopus_df <- data.frame()
        if(fs::file_exists(file.path("keys","scopus.key"))){
          try({
            glens_env$scopus_key <- sodium::data_decrypt(readRDS(file.path("keys","scopus.key")), key=sha256(glens_env$privkey_dec))
            #Get SCOPUS Data using orcid if SCOPUS API key is provided
            # scopus_df <- get_complete_scopus_data(trimws(rawToChar(glens_env$scopus_key)), orcid_list[x], rv, session) %>% dplyr::distinct() %>% dplyr::mutate(Source="SCOPUS")
            scopus_df <- get_complete_scopus_data(trimws(rawToChar(glens_env$scopus_key)), orcid_list[x], rv, output, session) 
            # print(colnames(scopus_df))
            # print(head(scopus_df))
          })
        }
        rv$scopus_df <- scopus_df
        
        return(orcid_df)
      }, simplify = F))
      
      # print(orcid2doi_table)
      
      
      
      if(nrow(orcid2doi_table) <= 0){
        rv$log_text <-  paste(rv$log_text, "\nError: Could not find data for OCR-ID(s).")
        output$log <- renderText({ rv$log_text })
        shinyjs::enable(id = "submit_button")
        shinyjs::hide("progress_overlay")
        rv$is_glens_exec <- F
        return()
      }
      orcid2doi_table[is.na(orcid2doi_table$external_id_url), c("external_id_url")] <- orcid2doi_table[is.na(orcid2doi_table$external_id_url), c("external_id_value")]
      doi_lines <- c(doi_lines, orcid2doi_table$external_id_url) #strsplit("DOIs from the API", "\n")[[1]]
    }else{
      rv$scopus_df <- NULL
      rv$wos_df <- NULL
      rv$semantic_df <- NULL
    }
    
    
    if (!is.null(input$doi_text) && !stringi::stri_isempty(input$doi_text)) {
      rv$log_text <- paste(rv$log_text, "DOI(s) provided as input.\n")
      doi_lines <- c(doi_lines, strsplit(input$doi_text, "\n")[[1]])
    }
    
    if(length(doi_lines) <= 0){
      rv$log_text <- paste(rv$log_text, "Cannot fetch DOI(s) for any input.\n")
      output$log <- renderText({rv$log_text})
      shinyjs::enable(id = "submit_button")
      shinyjs::hide("progress_overlay")
      rv$is_glens_exec <- F
      return()
    }
    doi_lines <- trimws(doi_lines)
    doi_lines <- doi_lines[doi_lines != ""]   # drop empty lines
    doi_count <- length(doi_lines)
    if (doi_count == 0) {
      rv$log_text <- paste(rv$log_text, "No DOIs provided.")
      output$log <- renderText({rv$log_text})
      rv$is_glens_exec <- F
      return()
    } 
    output$log <- renderText({rv$log_text})
    
    
    # progress object for UI (non-blocking)
    # progress <- Progress$new(session, min = 0, max = doi_count)
    # progress$set(message = "Calculation in progress", detail = "Starting...", value = 0)
    
    # reactive storage (local to this observer) to accumulate rows as they finish
    # glens_input_table <- data.frame()
    # glens_etable_final <- data.frame()
    accumulated <- list()        # list of data.frames
    processed_counter <- 0L
    found_counter <- 0L
    
    # control how often to update the log to avoid UI spamming:
    # either update every `update_every_n` DOIs, or when > update_every_secs elapsed.
    update_every_n <- 5      # change to control frequency (e.g. 1 => every DOI)
    update_every_secs <- 2   # minimum seconds between log updates
    last_log_time <- Sys.time()
    
# helper to possibly update the log (throttled)
    # maybe_update_log <- function() {
    #   now <- Sys.time()
    #   if ((processed_counter %% update_every_n == 0L) ||
    #       as.numeric(difftime(now, last_log_time, units = "secs")) >= update_every_secs ||
    #       processed_counter == doi_count) {
    #     # update text
    #     output$log <- renderText({
    #       sprintf("Processed %d/%d DOIs — found %d result rows so far",
    #               processed_counter, doi_count, found_counter)
    #     })
    #     last_log_time <<- now
    #   }
    # }
    
    maybe_update_log <- function() {
      now <- Sys.time()
      if ((processed_counter %% update_every_n == 0L) ||
          as.numeric(difftime(now, last_log_time, units = "secs")) >= update_every_secs ||
          processed_counter == doi_count) {
        
        rv$log_text <- paste0(rv$log_text, sprintf("Processed %d/%d DOIs — found %d result rows so far\n",
                                                   processed_counter, doi_count, found_counter))
        output$log <- renderText({ rv$log_text })
        last_log_time <<- now
      }
    }
    
    # create a promise for each DOI using future()
    promises_list <- lapply(seq_along(doi_lines), function(i) {
      doi <- doi_lines[i]
      
      # create a future that executes doi2gscholarlens(doi) in another R session
      future({
        
        # run the DOI lookup (this happens in the future worker)
        # protect the call with try to return NULL on error instead of stopping everything
        tryCatch({
          return(doi2gscholarlens(doi))
        }, error = function(e) {
          # return a simple data.frame or NULL — we use NULL below to indicate failure / no data
          print(paste("ERROR:",e))
          # NULL
          return(NULL)
        })
      }) %...>% (function(res_df) {
        
        if (rv$is_cancelled) {
          return(NULL) # Skip processing this resolved future
        }
        # this runs on the main R session when the future resolves
        # print(res_df)
        processed_counter <<- processed_counter + 1L
        if (!is.null(res_df) && nrow(res_df) > 0) {
          accumulated[[length(accumulated) + 1]] <<- res_df
          found_counter <<- found_counter + nrow(res_df)
          # output$log <- renderText(paste("Found",found_counter,"DOIs"))
        }
        # increment the progress bar (non-blocking)
        # progress$inc(1)
        # maybe_update_log()
        # print("HERE1")
        pct <- round((processed_counter / doi_count) * 100)
        shinyWidgets::updateProgressBar(
          session, 
          id = "doi_progress", 
          value = processed_counter, 
          total = doi_count,
          title = sprintf("Processing: %d%% (%d/%d DOIs)", pct, processed_counter, doi_count),
          status = if(pct == 100) "success" else "warning" # Turns green when finished
        )
        maybe_update_log()
        
        # resolve to something useful for the final aggregator
        list(index = i, doi = doi, result = res_df)
      }) %...!% (function(err){
        
        if (rv$is_cancelled) {
          return(NULL) # Skip processing this resolved future
        }
        # on future error: increment processed and progress, update log if needed
        processed_counter <<- processed_counter + 1L
        
        pct <- round((processed_counter / doi_count) * 100)
        shinyWidgets::updateProgressBar(
          session, 
          id = "doi_progress", 
          value = processed_counter, 
          total = doi_count,
          title = sprintf("Processing: %d%% (%d/%d DOIs) [Errors detected]", pct, processed_counter, doi_count),
          status = "danger" # Turns red if an error occurs
        )
        # progress$inc(1)
        maybe_update_log()
        # return a list showing error
        list(index = i, doi = doi, result = NULL, error = conditionMessage(err))
      })
    })
    
    # Use promise_all to wait until all DOI futures finish.
    # promise_all accepts a named list; using .list argument
    promise_all(.list = promises_list) %...>% (function(all_results) {
      if (rv$is_cancelled) {
        return(NULL) # Skip processing this resolved future
      }
      
      multi_merge_tbl <- data.frame()
      
      # all_results is a list of resolved values from each DOI promise
      # combine accumulated results (we also have them in accumulated list)
      # glens_input_table <- data.frame()
      accumulated_df <- data.frame()
      if (length(accumulated) > 0) {
        accumulated_df <- unique(bind_rows(accumulated)) %>% dplyr::mutate(Source="DOI/ORCID")
      } 
      
      # print(colnames(rv$scopus_df))
      # # print(head(rv$scopus_df))
      # print(nrow(rv$scopus_df))
      # print(colnames(accumulated_df))
      # # print(head(accumulated_df))
      # print(nrow(accumulated_df))
      
      # saveRDS(rv$scopus_df, file="scopus.rds")
      # saveRDS(accumulated_df, file="accumulated_df.rds")
      # rv$scopus_df <- future::value(rv$scopus_future) # %>% dplyr::distinct() %>% dplyr::mutate(Source="SCOPUS")
      if(nrow(rv$scopus_df) > 0){
        multi_merge_tbl <- dplyr::bind_rows(rv$scopus_df %>% dplyr::distinct() %>% dplyr::mutate(Source="SCOPUS"), accumulated_df)
      }
      
      rv$glens_input_table <- multi_merge_tbl
      
      output$dynamic_source_ui <- renderUI({
        req(rv$glens_input_table)
        
        # Extracts unique values from the 'Source' column (e.g., SCOPUS, DOI/ORCID)
        available_sources <- unique(rv$glens_input_table$Source)
        
        # Fallback if no sources are found yet
        if (is.null(available_sources) || length(available_sources) == 0) {
          return(p("No sources identified yet.", style = "color: #888; font-style: italic;"))
        }
        # available_sources <- c("SCOPUS", "DOI/ORCID", "Web of Science", "Semantic Scholar")
        available_sources <- levels(factor(rv$glens_input_table$Source))
        
        radioButtons("selected_source", 
                     label = NULL, 
                     choices = available_sources, 
                     selected = available_sources[1], # Default selection
                     inline = FALSE)     # Stacked vertically for better sidebar fit
        
      })
      
      # final, guaranteed log update
      rv$log_text <- paste(rv$log_text,sprintf("Done. Processed %d DOIs. Found %d result rows.", doi_count, found_counter))
      output$log <- renderText({
        rv$log_text
      })
      
      # print(glens_input_table)
      # output$data_table <- renderTable(glens_input_table, striped = TRUE)
      
      #SCRIPT 2 STARTS HERE
      # target_variants <- unlist(stringr::str_split(input$author_list, "\\|"))
      target_variants <- unlist(stringr::str_split(input$author_list, "\n"))
      target_variants <- str_trim(target_variants)
      target_variants <- target_variants[target_variants != ""]
      
      # print(colnames(rv$glens_input_table))
      
      # Normalize variants
      rv$target_variants_norm <- list()
      for (v in target_variants) {
        vn <- normalize_name(v)
        parts <- extract_parts(vn)
        rv$target_variants_norm[[v]] <- list(norm = vn, parts = parts)
      }
      rv$author_match_regex <- build_name_regex_for_variants(target_variants)
      
      # print(target_variants)
      # print(rv$target_variants_norm)
      
      extend_input_table(rv)
      
      # print(colnames(rv$glens_etable_final)) #DEBUG
      # print(nrow(rv$glens_etable_final)) #DEBUG
      
      rv$glens_year_filtered <- rv$glens_etable_final
      
      if(nrow(rv$glens_year_filtered) <= 0){
        #No names were matched. return
        output$log <- renderText(sprintf("No names were matched."))
        shinyjs::enable(id = "submit_button")
        shinyjs::hide("progress_overlay")
        # progress$close()
        return()
      }
      
      compute_indices(rv)
      
      # output$extended_table <- renderTable(rv$glens_year_filtered, striped = TRUE)
      output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
      output$sh_index <-  renderUI({
        HTML(paste("<b>Sh-Index:</b>", rv$sh_index))
      })
      output$extended_table <- DT::renderDataTable({
        datatable(
          rv$glens_year_filtered,
          options = list(
            scrollY = "600px",
            scrollX = TRUE,
            paging = TRUE
          )
        )
      })
      
      # min_year <- min(as.numeric(glens_extended_table$Year))
      # max_year <- max(as.numeric(glens_extended_table$Year))
      min_year <- min(as.numeric(rv$glens_etable_final$Year))
      max_year <- max(as.numeric(rv$glens_etable_final$Year))
      # print(output)
      # print(c(min_year,max_year))
      # print(input$year_slider)
      updateSliderInput(session, "year_slider", value = c(min_year,max_year),min = min_year, max=max_year)
      shinyjs::show(id="year_slider")
      
      #SCRIPT3 Starts here
      match_journals(rv)
      rv$glens_year_filtered <- rv$glens_etable_final
      render_skeleton_plots(rv, output)
      plot_glens_table(rv, output, session)
      rv$is_glens_exec <- F   
      shinyjs::delay(1500, shinyjs::hide("progress_overlay"))
      #Enable button after all 3 script flows end
      shinyjs::enable(id = "submit_button")
      # progress$close()
      # NULL
      
      # print(rv)
      return(NULL)
    }) %...!% (function(err) {
      if (rv$is_cancelled) {
        return(NULL) # Skip processing this resolved future
      }
      # overall failure handler
      # progress$close()
      shinyWidgets::updateProgressBar(session, id = "doi_progress", value = 100, status = "danger", title = "Process Failed!")
      print(conditionMessage(err))
      output$log <- renderText(sprintf("Failed: %s", conditionMessage(err)))
      shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
      shinyjs::enable(id = "submit_button")
      # NULL
      return(NULL)
    })
    
    # immediately show a short message so user sees something while promises run
    output$log <- renderText(sprintf("Started processing %d DOIs...", doi_count))
  })
  
}
