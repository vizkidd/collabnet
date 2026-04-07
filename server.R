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
suppressPackageStartupMessages(require(parallel))

is_WASM <- grepl(pattern="wasm",x=Sys.info()["machine"])

# use a multisession plan so futures run in background R sessions
# if(!is_WASM){
  future::plan(future::multisession)
# future::plan(future::multicore)
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

source("GScholarLENS-ProcessJCR.R", local = TRUE)
source("GScholarLENS-DOI2Data.R", local = TRUE)
source("GScholarLENS-ORCID2Data.R", local = TRUE)
source("GScholarLENS-SCOPUS2Data.R", local = TRUE)
source("GScholarLENS-Data2GLENS.R", local = TRUE)
source("GScholarLENS-PlotGLENS.R", local = TRUE)

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
  
  need_cols <- c("Title","Authors","Adjusted_Citations","Journal",
                 "First_Author","Second_Author","Co_Author","Corresponding_Author")
  missing_cols <- setdiff(need_cols, names(rv$glens_etable_final))
  if (length(missing_cols) > 0) {
    warning(paste("Author-level file missing columns:", paste(missing_cols, collapse = ", ")))
    return()
  }
  
  unique_journals <- unique(rv$glens_etable_final$Journal)
  cat("Unique journals to match:", length(unique_journals), "\n")
  
  rv$glens_etable_final$Name_norm <- sapply(rv$glens_etable_final$Journal, function(x) normalize_journal(x))
  
  # Note: This assumes 'jcr' and 'jcr_names_norm' are loaded in your global environment 
  # since the reading code was commented out!
  
  match_idx <- unique(
    bind_rows(
      future_sapply(
        seq_len(length(unique_journals)),
        getExcelColumns,
        unique_journals = unique_journals,
        jsonData = jcr_names_norm,
        simplify = FALSE,
        future.packages = c("stringr", "dplyr"),
        # EXPLICITLY pass the large object and the function
        future.globals = c("jcr_names_norm", "getExcelColumns"),
        future.seed = TRUE
      )
    )
  )
  print("HERE0")
  # Safely handle the case where absolutely NO journals were matched in pass 1
  if (nrow(match_idx) > 0) {
    jcr_matched <- inner_join(jcr_names_norm, match_idx, by = c("Name_norm", "Qscore", "JIF5Years"))
    print(str(match_idx))
    print(str(jcr_matched))
    print("HERE0.1")  
  } else {
    jcr_matched <- jcr[0, ] # Creates an empty df that still has the Qscore column
    print("HERE0.2")
  }
  
  # # Clean up old columns just in case
  rv$glens_etable_final$Qscore <- NULL
  rv$glens_etable_final$JIF5Years <- NULL
  print("HERE1")
  print(str(rv$glens_etable_final))
  df_auth_joined <- left_join(rv$glens_etable_final, jcr_matched, by = c("Name_norm"))
  print("HERE2")
  
  # --- 1. Handle Journal Naming ---
  if ("Journal.x" %in% names(df_auth_joined)) {
    df_auth_joined <- df_auth_joined %>% rename("User_Journal" = Journal.x)
  } else if ("Journal" %in% names(df_auth_joined) && !"User_Journal" %in% names(df_auth_joined)) {
    df_auth_joined <- df_auth_joined %>% rename("User_Journal" = Journal)
  }
  
  if ("Journal.y" %in% names(df_auth_joined)) {
    df_auth_joined <- df_auth_joined %>% rename("JCR_Journal" = Journal.y)
  } else if (!"JCR_Journal" %in% names(df_auth_joined)) {
    df_auth_joined$JCR_Journal <- NA_character_
  }
  
  # --- 2. CRITICAL FIX: Resolve Qscore .x and .y collisions ---
  if ("Qscore.x" %in% names(df_auth_joined) && "Qscore.y" %in% names(df_auth_joined)) {
    df_auth_joined <- df_auth_joined %>%
      mutate(Qscore = coalesce(Qscore.y, Qscore.x)) %>% # Prefer new match (.y), fallback to old (.x)
      select(-Qscore.x, -Qscore.y)                      # Remove the messy collision columns
  } else if (!"Qscore" %in% names(df_auth_joined)) {
    df_auth_joined$Qscore <- NA_character_
  }
  
  # --- 3. Resolve JIF5Years .x and .y collisions ---
  if ("JIF5Years.x" %in% names(df_auth_joined) && "JIF5Years.y" %in% names(df_auth_joined)) {
    df_auth_joined <- df_auth_joined %>%
      mutate(JIF5Years = coalesce(JIF5Years.y, JIF5Years.x)) %>%
      select(-JIF5Years.x, -JIF5Years.y)
  }
  print(str(df_auth_joined))
  print("HERE3")
  # For any unmatched journals, try a fallback: look for exact substring match in Name
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
  print("HERE4")
  # CRITICAL FIX 2: Convert all remaining NAs to "Unranked" so dplyr plotting doesn't crash
  df_auth_joined <- df_auth_joined %>%
    mutate(Qscore = if_else(is.na(Qscore), "Unranked", as.character(Qscore)))
  print("HERE5")
  n_unmatched <- length(which(is.na(df_auth_joined$JCR_Journal)))
  cat("Number of unmatched journal rows:", n_unmatched, "\n")
  print("HERE6")
  rv$glens_etable_final <- df_auth_joined
  print(str(rv$glens_etable_final))
  print("HERE6.1")
}

# match_journals <- function(rv){
#   # jcr_base <- "2024-JCR_IMPACT_FACTOR"
#   # jcr_file_xlsx <- paste0(jcr_base, ".xlsx")
#   # jcr_file_xls  <- paste0(jcr_base, ".xls")
#   # jcr_file_csv  <- paste0(jcr_base, ".csv")
#   # 
#   # jcr_path <- NULL
#   # if (file.exists(jcr_file_xlsx)) jcr_path <- jcr_file_xlsx
#   # if (is.null(jcr_path) && file.exists(jcr_file_xls)) jcr_path <- jcr_file_xls
#   # if (is.null(jcr_path) && file.exists(jcr_file_csv)) jcr_path <- jcr_file_csv
#   # 
#   # if (is.null(jcr_path)) {
#   #   warning("Cannot find '2024-JCR_IMPACT_FACTOR(.xlsx/.csv)' in working directory.\n")
#   #   # jcr_path <- readline(prompt = "Enter full path to JCR file (xlsx or csv): ")
#   #   # jcr_path <- str_trim(jcr_path)
#   #   warning("JCR file not found. Exiting.")
#   #   return()
#   # } else {
#   #   cat("Found JCR file:", jcr_path, "\n")
#   # }
#   # jcr <- read_jcr(jcr_path)
#   # 
#   # # If JIF columns exist, ensure numeric
#   # if ("JIF" %in% names(jcr)) jcr$JIF <- suppressWarnings(as.numeric(jcr$JIF))
#   # if ("JIF5Years" %in% names(jcr)) jcr$JIF5Years <- suppressWarnings(as.numeric(jcr$JIF5Years))
#   # 
#   # need_cols <- c("Title","Authors","Adjusted_Citations","Journal",
#   #                "First_Author","Second_Author","Co_Author","Corresponding_Author")
#   # missing_cols <- setdiff(need_cols, names(rv$glens_etable_final))
#   # if (length(missing_cols) > 0) {
#   #   warning(paste("Author-level file missing columns:", paste(missing_cols, collapse = ", ")))
#   #   return()
#   # }
#   # 
#   # unique_journals <- unique(rv$glens_etable_final$Journal)
#   # print(cat("Unique journals to match:", length(unique_journals), "\n"))
#   # 
#   # jcr$Name_norm <- sapply(jcr$Name, function(x) normalize_journal(x))
#   # rv$glens_etable_final$Name_norm <- sapply(rv$glens_etable_final$Journal, function(x) normalize_journal(x))
#   # 
#   # jcr_names_norm <- jcr |>
#   #   select(Name, Name_norm, JIF5Years, Qscore) 
#   # 
#   # match_idx <- unique(
#   #   bind_rows(
#   #     future_sapply(
#   #       seq_len(length(unique_journals)),
#   #       getExcelColumns,
#   #       unique_journals = unique_journals,
#   #       jsonData = jcr_names_norm,
#   #       simplify = FALSE,
#   #       future.packages = c("stringr", "dplyr")
#   #     )
#   #   )
#   # )
#   # 
#   # jcr_matched <- inner_join(jcr,match_idx)
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
#   print(cat("Unique journals to match:", length(unique_journals), "\n"))
#   
#   rv$glens_etable_final$Name_norm <- sapply(rv$glens_etable_final$Journal, function(x) normalize_journal(x))
#   
#   match_idx <- unique(
#     bind_rows(
#       future_sapply(
#         seq_len(length(unique_journals)),
#         getExcelColumns,
#         unique_journals = unique_journals,
#         jsonData = jcr_names_norm,
#         simplify = FALSE,
#         future.packages = c("stringr", "dplyr")
#       )
#     )
#   )
#   
#   jcr_matched <- inner_join(jcr,match_idx)
#   
#   rv$glens_etable_final[c("Qscore", "JIF5Years")] <- NULL
#   df_auth_joined <- left_join(rv$glens_etable_final, jcr_matched, by = c("Name_norm"))#, relationship = "many-to-many") 
#   df_auth_joined <- df_auth_joined |> rename("User_Journal" = Journal.x) |> rename("JCR_Journal" = Journal.y)
#   
#   # For any unmatched journals, try a fallback: look for exact substring match in Name
#   unmatched <- which(is.na(df_auth_joined$JCR_Journal))
#   if (length(unmatched) > 0) {
#     cat("Trying fallback substring match for", length(unmatched), "journals...\n")
#     for (i in unmatched) {
#       jn <- df_auth_joined$Name_norm[i]
#       if (is.na(jn) || nchar(jn) < 3) next
#       hits <- grep(jn, jcr_names_norm$Name_norm, value = TRUE)
#       # print(hits)
#       if (length(hits) == 1) {
#         idx <- which(jcr_names_norm$Name_norm == hits)[1]
#         df_auth_joined$JCR_Journal[i] <- jcr$Name[idx]
#         df_auth_joined$Qscore[i] <- jcr$Qscore[idx]
#         # df_auth_joined$ISSN[i] <- if ("ISSN" %in% names(jcr)) jcr$ISSN[idx] else NA_character_
#         # df_auth_joined$EISSN[i] <- if ("EISSN" %in% names(jcr)) jcr$EISSN[idx] else NA_character_
#       }
#     }
#   }
#   
#   # If still many unmatched, notify user (they can inspect sortedfile.csv)
#   n_unmatched <- length(which(is.na(df_auth_joined$JCR_Journal)))
#   print(paste("Number of unmatched journal rows:", n_unmatched, "\n"))
#   rv$glens_etable_final <- df_auth_joined
#   
# }

plot_glens_table <- function(rv, output, session){
  if(nrow(rv$glens_year_filtered) <= 0){
    output$log <- renderText({paste(rv$log, "plot_glens_table() - Warning: No data available for these filters!", sep="\n")})
    warning("plot_glens_table() - Warning: No data available for these filters!")
    shinyjs::hide("sh_index")
    shinyjs::hide("summary_table")
    shinyjs::hide("acounts_plot")
    shinyjs::hide("ccounts_plot")
    shinyjs::hide("cdist_plot")
    shinyjs::hide("aperc_plot")
    shinyjs::hide("cperc_plot")
    shinyjs::hide("extended_table")
    return()
  }
  
  # Render Filtered Subset Network
  output$network_filtered <- renderVisNetwork({
    req(rv$glens_year_filtered) # Assuming this is your filtered reactive variable
    
    net_data <- build_collaboration_network(rv$glens_year_filtered, rv$author_list)
    
    visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
      visNodes(font = list(size = 14)) %>%
      visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
      # visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -50)) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE) %>%
      # visLegend() %>%
      addFontAwesome()
  })
  
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
    # output$log <- renderText({ rv$log_text })
    warning("plot_glens_table() - Warning: Need more than one group and atleast 1 paper with 1 citation per-group for plotting distribution.")
    shinyjs::hide("cdist_plot")
    # return()
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
  
  pub_pdata <- df_plot %>% 
    group_by(position_rank) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(pcontrib = if (is.na(total_pubs) || total_pubs == 0) 0 else (n / total_pubs) * 100)
  # print(pub_pdata)
  
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
  
  # print("HERE1")
  req(pub_pdata)
  # print("HERE2")
  
  if(nrow(pub_pdata) <= 0){
    shinyjs::hide("aperc_plot")
    shinyjs::hide("cperc_plot")
    # return()
  }
  
  aperc_proxy <- plotlyProxy("aperc_plot", session)
  
  n <- length(all_positions)
  
  # 1. Calculate values
  aperc_vals <- sapply(all_positions, function(pos) {
    i <- which(pub_pdata$position_rank == pos)
    
    # Extract value if it exists, otherwise default to 0
    val <- if (length(i) == 1) pub_pdata$pcontrib[i] else 0
    
    # Final safety net to strip any lingering NAs or NaNs
    if (is.na(val) || is.nan(val)) 0 else val
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
  
  cites_pdata <- df_plot %>% 
    group_by(position_rank) %>% 
    summarise(TotalCitations = sum(Citations), .groups = 'drop') %>% 
    # Safely check for NA first, and use the double || 
    mutate(pcontrib = if (is.na(total_cites) || total_cites == 0) 0 else (TotalCitations / total_cites) * 100)
  
  # print(cites_pdata)
  
  req(cites_pdata)
  cperc_proxy <- plotlyProxy("cperc_plot", session)
  
  n <- length(all_positions)
  
  # 1. Map the citation data to match the order of all_positions
  cperc_vals <- sapply(all_positions, function(pos) {
    idx <- which(cites_pdata$position_rank == pos)
    
    # Extract value if it exists, otherwise default to 0
    val <- if (length(idx) == 1) cites_pdata$pcontrib[idx] else 0
    
    # Final safety net to strip any lingering NAs or NaNs before Plotly gets it
    if (is.na(val) || is.nan(val)) 0 else val
  })
  
  # print("HERE3")
  # print(cites_pdata)
  # print(cperc_vals)
  # print(n)
  # print(all_positions)
  # print("HERE4")
  
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
  print("HERE8.1")
  print(str(df_ordered_debug))
  agg_first <- make_agg(df_ordered_debug, "First_Author", "First Author")
  agg_second <- make_agg(df_ordered_debug, "Second_Author", "Second Author")
  agg_co <- make_agg(df_ordered_debug, "Co_Author", "Co-Author")
  agg_cor <- make_agg(df_ordered_debug, "Corresponding_Author", "Corresponding Author")
  print("HERE8.2")
  agg_all <- bind_rows(agg_first, agg_second, agg_co, agg_cor)
  
  # Ensure all quartiles present per position (fill zeros)
  all_positions <- c("First Author","Second Author","Co-Author","Corresponding Author")
  all_quartiles <- c("Q1","Q2","Q3","Q4", "NA")
  print("HERE8.3")
  full_grid <- expand.grid(Position = all_positions, Qscore = all_quartiles, stringsAsFactors = FALSE)
  print("HERE8.4")
  agg_all <- full_grid %>%
    left_join(agg_all, by = c("Position","Qscore")) %>%
    mutate(Count = tidyr::replace_na(Count, 0L),
           SumCitations = tidyr::replace_na(SumCitations, 0.0))
  
  print("HERE9")
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
  
  print("HERE10")
  # Order positions for plotting (Left to right as in your image: First, Second, Co, Corresponding)
  agg_all$Position <- factor(agg_all$Position, levels = all_positions)
  agg_all$Qscore <- factor(agg_all$Qscore, levels = all_quartiles)
  print("HERE11")
  agg_all <- agg_all %>%
    group_by(Position) %>%
    mutate(Total_Position = sum(Count, na.rm = TRUE)) %>%
    ungroup()
  agg_all <- agg_all %>%
    group_by(Position) %>%
    mutate(Total_Citations = sum(SumCitations, na.rm = TRUE)) %>%
    ungroup()
  print("HERE1.1")
  agg_all <- agg_all %>%
    group_by(Qscore) %>%
    mutate(Total_QCitations = sum(SumCitations, na.rm = TRUE)) %>%
    ungroup()
  print("HERE1.2")
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
    author_list=list(),
    glens_input_table = data.frame(),
    glens_etable_final = data.frame(),
    glens_year_filtered = data.frame(),
    summary_table = data.frame(),
    scopus_df = data.frame(),
    scopus_future_list = list(),
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
  #   value = 0, 
  #   total = doi_count,
  #   title = sprintf("Starting calculation for %d DOIs...", doi_count)
  # )
  output$log <- renderText({
    rv$log_text
  })
  
  # output$log <- renderText({
  #   files <- list.files(getwd(), all.files = TRUE, recursive = TRUE)
  #   paste(
  #     "Current working dir:", getwd(),
  #     "\n\nFiles in VFS:\n", 
  #     paste(files, collapse = "\n")
  #   )
  # })
  
  output$dynamic_author_filter <- renderUI({
    req(input$author_list)
    # 1. Get the raw text from the text area
    raw_text <- input$author_list
    # 2. If it's empty or NULL, don't show the panel
    if (is.null(raw_text) || trimws(raw_text) == "") {
      return(NULL)
    }
    # 3. Split by newline and remove any empty/blank lines
    author_list <- strsplit(raw_text, "\n")[[1]]
    author_list <- author_list[trimws(author_list) != ""]
    # 4. If there is more than 1 valid author name, render the wellPanel
    if (length(author_list) > 1) {
      wellPanel(
        tags$h5(icon("users-cog"), " Author Relationship Filter", class = "text-primary"),
        tags$p("Filter the publication list based on how the selected authors interact.", class = "text-muted"),
        
        radioButtons(
          inputId = "author_logic_gate",
          label = NULL,
          choices = c(
            "Co-patriot/Collaborator (OR)" = "OR",
            "Companion (AND)"              = "AND",
            "Rival (XOR)"                  = "XOR",
            "Ignore All (NOR)"                 = "NOR",
            "Divide & Exclude (NAND)"                = "NAND"
          ),
          selected = "OR",
          width = "100%"
        )
      )
    } else {
      # If 1 or 0 authors, hide the panel
      return(NULL)
    }
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
    # req(input$scopus_key)
    if(is.null(input$scopus_key) || stringi::stri_isempty(input$scopus_key)){
      if(fs::file_exists(file.path("keys","scopus.key")))
        fs::file_delete(file.path("keys","scopus.key"))
      removeModal()
      return()
    }
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
    # req(input$wos_key)
    if(is.null(input$wos_key) || stringi::stri_isempty(input$wos_key)){
      if(fs::file_exists(file.path("keys","wos.key")))
        fs::file_delete(file.path("keys","wos.key"))
      removeModal()
      return()
    }
    raw_key <- charToRaw(trimws(input$wos_key))
    encrypted_wos <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_wos, file = file.path("keys","wos.key"))
    showNotification("Web of Science Key Encrypted and Saved.", type = "message")
    removeModal()
  })
  observeEvent(input$save_semantic, {
    # fs::dir_create("keys")
    # req(input$semantic_key)
    if(is.null(input$semantic_key) || stringi::stri_isempty(input$semantic_key)){
      if(fs::file_exists(file.path("keys","semantic.key")))
        fs::file_delete(file.path("keys","semantic.key"))
      removeModal()
      return()
    }
    raw_key <- charToRaw(trimws(input$semantic_key))
    encrypted_semantic <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_semantic, file = file.path("keys","semantic.key"))
    showNotification("Semantic Scholar Key Encrypted and Saved.", type = "message")
    removeModal()
  })
  
  #Slider Event
  observeEvent(c(input$selected_source, input$year_slider, input$author_list, input$author_logic_gate), {
      req(rv$glens_etable_final, input$selected_source, input$year_slider, input$author_list)
      
      if(isTRUE(is.null(input$author_logic_gate))){
        author_logic_gate <- "OR"
      }else{
        author_logic_gate <- input$author_logic_gate
      }
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
        shinyjs::hide("aperc_plot")
        shinyjs::hide("cperc_plot")
        shinyjs::hide("extended_table")
        shinyjs::enable(id="year_slider")
        return()
      }
      
      rv$log_text <- paste(rv$log_text,paste("(Slider:", input$year_slider[1], "-", input$year_slider[2],")","Filtered years to range...", min(rv$glens_year_filtered$Year), "and",max(rv$glens_year_filtered$Year)
      ), sep="\n")
      
      # output$log <- renderText({ rv$log_text })
      print(paste("(Slider:", input$year_slider[1], "-", input$year_slider[2],")","Filtered years to range...", min(rv$glens_year_filtered$Year), "and",max(rv$glens_year_filtered$Year)))
      # print(str(rv$glens_year_filtered$Year))
      
      raw_text <- input$author_list
      # Only apply the logic gate if the user has actually typed something
      if (!is.null(raw_text) && trimws(raw_text) != "") {
        author_list <- unlist(strsplit(input$author_list, "[\n,]"))
        author_list <- stringr::str_squish(author_list)
        author_list <- stringr::str_to_title(author_list)
        author_list <- author_list[author_list != ""]
        
        rv$author_list <- unique(author_list)
        # Apply the logic gate function we built earlier
        if (length(author_list) > 0) {
          filtered_df <- apply_author_logic(
            pubs_df          = rv$glens_year_filtered,
            primary_regex    = rv$author_match_regex,
            selected_authors = author_list, 
            gate             = author_logic_gate        
          )
          # 3. Save the newly filtered data to your reactive variable
          rv$glens_year_filtered <- filtered_df
        }
      }
      
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
    
    # Reset the UI Progress Bars to 0%
    shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 0, 
                                    title = "DOI / ORCID Resolver: 0%", status = "info")
    shinyWidgets::updateProgressBar(session, id = "prog_scopus", value = 0, 
                                    title = "Scopus API: 0%", status = "info")
    
    # check_orcid_input <- F
    # input_is_orcid <- F
    if (is.null(input$author_list) || stringi::stri_isempty(input$author_list)) {
      rv$log_text <- paste(rv$log_text, "Author list required.\n")
      # #INPUT IS PROLLY ORCID
      # check_orcid_input <- T
      # output$log <- renderText({rv$log_text})
      shinyjs::enable(id = "submit_button")
      shinyjs::hide("progress_overlay")
      rv$is_glens_exec <- F
      req(input$author_list)
      return()
    }
    
    shinyjs::disable(id = "submit_button")
    shinyjs::hide(id="year_slider")
    shinyjs::show("progress_overlay")
  
    observeEvent(input$cancel_button, {
      rv$is_cancelled <- TRUE
      print("HERE1")
      removeModal()
      
      # Immediately hide the overlay and re-enable the UI
      shinyjs::hide("progress_overlay")
      shinyjs::enable(id = "submit_button")
      
      # Update logs
      rv$log_text <- paste(rv$log_text, paste0("Process cancelled by user.\n"),sep="\n")
      # output$log <- renderText({ rv$log_text })
    })
      
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
      # output$log <- renderText({rv$log_text})
      # shinyjs::enable(id = "submit_button")
      # rv$is_glens_exec <- F
      # return()
    } 
    #   input_is_orcid <- T
    # }
    
    doi_lines <- c()
    # print(orcid_list)
    if(length(orcid_list) == 1 && stringi::stri_isempty(orcid_list)){
      orcid_list<- list()
    }
    
    # Ensure lists are clean and empty strings are removed
    orcid_list <- orcid_list[trimws(orcid_list) != ""]
    doi_lines <- doi_lines[trimws(doi_lines) != ""]
    
    # We use a reactiveValues object to safely track progress across all async streams on the main thread
    progress_state <- reactiveValues(orcid_done = 0, doi_done = 0, scopus_done = 0, doi_found = 0)
    
    # ==============================================================================
    # PHASE 1: ORCID -> DOI EXTRACTION (ASYNC)
    # ==============================================================================
    
    if (length(orcid_list) > 0) {
      rv$log_text <- paste(rv$log_text, sprintf("Processing %d ORC-ID(s)...\n", length(orcid_list)))
      # output$log <- renderText({rv$log_text})
      
      # Stream 1: Fetch all ORCIDs in parallel
      orcid_promises <- lapply(orcid_list, function(orcid_str) {
        future({
          clean_orcid <- trimws(orcid_str)
          if (length(strsplit(clean_orcid, "-")[[1]]) != 4) return(list(error = "Malformed ORCID"))
          
          target_url <- paste0("https://pub.orcid.org/v3.0/", clean_orcid, "/works")
          
          # Use base R connection to avoid httr2 serialization/timeout issues inside futures
          res <- tryCatch({
            con <- url(target_url, headers = c(Accept = "application/xml"))
            lines <- readLines(con, warn = FALSE)
            close(con)
            paste(lines, collapse = "\n")
          }, error = function(e) {
            if (exists("con")) try(close(con), silent = TRUE)
            return(e)
          })
          
          if (inherits(res, "error")) return(list(error = conditionMessage(res)))
          
          # Parse XML safely inside the worker
          xml_vec <- xml2::read_xml(res)
          xml_vec_ns <- xml2::xml_ns(xml_vec)
          xml_groups <- xml2::xml_find_all(xml_vec, ".//activities:group", xml_vec_ns)
          
          # Extract DOI details (assuming xtext is available)
          orcid_df <- purrr::map_dfr(xml_groups, function(g) {
            tibble::tibble(
              source_name = xtext(g, ".//common:source-name", xml_vec_ns),
              title = xtext(g, ".//common:title", xml_vec_ns),
              external_id_value = xtext(g, ".//common:external-id-value", xml_vec_ns),
              external_id_url = xtext(g, ".//common:external-id-url", xml_vec_ns),
              last_modified_date = xtext(g, ".//common:last-modified-date", xml_vec_ns),
              journal_title = xtext(g, ".//work:journal-title", xml_vec_ns),
              work_type = xtext(g, ".//work:type", xml_vec_ns)
            )
          })
          orcid_df$orcid <- clean_orcid
          return(list(df = orcid_df, error = NULL))
        }, globals = c("xtext", "orcid_str")) %...>% (function(res) {
          # Resolves on main thread
          if (rv$is_cancelled) return(NULL)
          
          progress_state$orcid_done <- progress_state$orcid_done + 1
          if (!is.null(res$error)) {
            rv$log_text <- paste(rv$log_text, "ORCID Error:", res$error, "\n")
            # output$log <- renderText({rv$log_text})
          }
          return(res$df)
        })
      })
      
      master_orcid_promise <- promise_all(.list = orcid_promises)
    } else {
      # Fallback: if no ORCIDs were provided, resolve immediately to an empty list
      master_orcid_promise <- promise_resolve(list())
    }
    
    
    # ==============================================================================
    # PHASE 2: LAUNCH SCOPUS IMMEDIATELY (Doesn't wait for ORCID extraction)
    # ==============================================================================
    scopus_count <- length(orcid_list)
    has_scopus_key <- fs::file_exists(file.path("keys","scopus.key"))
    if (has_scopus_key) {
      shinyjs::show("scopus_bar_container")
      scopus_key_val <- trimws(rawToChar(sodium::data_decrypt(readRDS(file.path("keys","scopus.key")), key=openssl::sha256(glens_env$privkey_dec))))
      rv$log_text <- paste(rv$log_text, "Found Scopus API key!.\n")
    }else{
      shinyjs::hide("scopus_bar_container")
      rv$log_text <- paste(rv$log_text, "No Scopus key found. Skipping Scopus.\n")
    }
    
    rv$log_text <- paste(rv$log_text, "Launching Scopus fetching in parallel...\n")
    # output$log <- renderText({rv$log_text})
    
    # --- STREAM B: PARALLEL SCOPUS PROCESSING ---
    scopus_promises <- lapply(seq_along(orcid_list), function(i) {
      orcid_target <- orcid_list[i]
      
      # 1. Handle missing key gracefully & update progress bar
      if (!has_scopus_key) {
        progress_state$scopus_done <- progress_state$scopus_done + 1
        pct <- round((progress_state$scopus_done / max(1, scopus_count)) * 100)
        shinyWidgets::updateProgressBar(
          session, id = "prog_scopus", value = progress_state$scopus_done, total = max(1, scopus_count),
          title = sprintf("Scopus Skipped (No Key): %d%%", pct), status = "warning"
        )
        return(promise_resolve(NULL))
      }
      
      # 2. Launch the Future Worker
      future({
        tryCatch({ 
          get_complete_scopus_data(scopus_key_val, orcid_target) 
        }, error = function(e) list(error = conditionMessage(e)))
      }, 
      globals = c("get_complete_scopus_data", "scopus_key_val", "orcid_target"),
      packages = c("dplyr", "httr", "jsonlite", "tidyr", "purrr") 
      ) %...>% (function(res) {
        
        if (rv$is_cancelled) return(NULL)
        
        # 3. INCREMENT PROGRESS BAR
        progress_state$scopus_done <- progress_state$scopus_done + 1
        pct <- round((progress_state$scopus_done / max(1, scopus_count)) * 100)
        shinyWidgets::updateProgressBar(
          session, id = "prog_scopus", value = progress_state$scopus_done, total = max(1, scopus_count),
          title = sprintf("Scopus: %d%% (%d/%d)", pct, progress_state$scopus_done, scopus_count),
          status = if(pct == 100) "success" else "info"
        )
        
        # 4. Check error
        if (is.list(res) && !is.null(res$error)) {
          rv$log_text <- paste(rv$log_text, "\nScopus Error for", orcid_target, ":", res$error)
          # output$log <- renderText({rv$log_text})
          return(NULL)
        }
        
        return(res) 
      })
    })
    
    # Wrap all Scopus promises into one master promise
    master_scopus_promise <- promise_all(.list = scopus_promises)
    
    
    # ==============================================================================
    # PHASE 3: WAIT FOR ORCIDS -> THEN LAUNCH DOI
    # ==============================================================================
    # Notice we assign this to `master_doi_promise`
    master_doi_promise <- master_orcid_promise %...>% (function(orcid_results) {
      if (rv$is_cancelled) return(NULL)
      
      # 1. Combine DOIs extracted from ORCIDs with manually typed DOIs
      extracted_orcid_dfs <- purrr::compact(orcid_results) 
      if (length(extracted_orcid_dfs) > 0) {
        orcid_combo <- dplyr::bind_rows(extracted_orcid_dfs)
        missing_url <- is.na(orcid_combo$external_id_url)
        orcid_combo[missing_url, "external_id_url"] <- orcid_combo[missing_url, "external_id_value"]
        doi_lines <<- unique(c(doi_lines, orcid_combo$external_id_url))
      }
      
      doi_lines <<- doi_lines[!is.na(doi_lines) & trimws(doi_lines) != ""]
      doi_count <- length(doi_lines)
      
      rv$log_text <- paste(rv$log_text, sprintf("Extracted %d total DOIs. Launching DOIs...\n", doi_count))
      # output$log <- renderText({rv$log_text})
      
      # --- STREAM A: PARALLEL DOI PROCESSING ---
      doi_promises <- lapply(seq_along(doi_lines), function(i) {
        doi_target <- doi_lines[i]
        future({
          tryCatch({ doi2gscholarlens(doi_target) }, error = function(e) NULL)
        }) %...>% (function(res_df) {
          if (rv$is_cancelled) return(NULL)
          
          progress_state$doi_done <- progress_state$doi_done + 1
          pct <- round((progress_state$doi_done / max(1, doi_count)) * 100)
          
          shinyWidgets::updateProgressBar(
            session, id = "prog_doi", value = progress_state$doi_done, total = max(1, doi_count),
            title = sprintf("DOI: %d%% (%d/%d)", pct, progress_state$doi_done, doi_count),
            status = if(pct == 100) "success" else "warning"
          )
          return(res_df)
        })
      })
      
      # RETURN the resolved DOI promises to `master_doi_promise`
      return(promise_all(.list = doi_promises))
    })
    
    promise_all(
      dois = master_doi_promise,
      scopus = master_scopus_promise
    ) %...>% (function(results) {
      if (rv$is_cancelled) return(NULL)
      
      # Merge DOIs
      accumulated_df <- dplyr::bind_rows(purrr::compact(results$dois))
      if (nrow(accumulated_df) > 0) accumulated_df <- accumulated_df %>% dplyr::distinct() %>% dplyr::mutate(Source = "DOI/ORCID")
      
      # Merge Scopus
      rv$scopus_df <- dplyr::bind_rows(purrr::compact(results$scopus))
      if (nrow(rv$scopus_df) > 0) rv$scopus_df <- rv$scopus_df %>% dplyr::distinct() %>% dplyr::mutate(Source = "SCOPUS")
      
      # Final Table
      rv$glens_input_table <- dplyr::bind_rows(accumulated_df, rv$scopus_df)
      
      rv$log_text <- paste(rv$log_text, sprintf("\nDone. Found %d total records.\n", nrow(rv$glens_input_table)))
      # output$log <- renderText(rv$log_text)
      
      output$dynamic_source_ui <- renderUI({
        req(rv$glens_input_table)
        available_sources <- levels(factor(rv$glens_input_table$Source))
        if (length(available_sources) == 0) return(p("No sources identified yet.", style = "color: #888;"))
        radioButtons("selected_source", label = NULL, choices = available_sources, selected = available_sources[1], inline = FALSE)
      })
      
      # --- SCRIPTS 2 & 3: STATS & PLOTTING ---
      target_variants <- stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n")))
      target_variants <- target_variants[target_variants != ""]
      
      rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
        vn <- normalize_name(v)
        list(norm = vn, parts = extract_parts(vn))
      })
      rv$author_match_regex <- build_name_regex_for_variants(target_variants)
      
      extend_input_table(rv)
      rv$glens_year_filtered <- rv$glens_etable_final
      
      if (nrow(rv$glens_year_filtered) <= 0) {
        output$log <- renderText(paste(rv$log, "No names were matched.",sep="\n"))
        shinyjs::enable("submit_button")
        shinyjs::hide("progress_overlay")
        return()
      }
      
      compute_indices(rv)
      
      output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
      output$sh_index <- renderUI(HTML(paste("<b>Sh-Index:</b>", rv$sh_index)))
      output$extended_table <- DT::renderDataTable({
        DT::datatable(rv$glens_year_filtered, options = list(scrollY = "600px", scrollX = TRUE, paging = TRUE))
      })
      
      print(str(jcr_names_norm))
      # 1. Match Journals and create Qscore FIRST
      match_journals(rv)
      print("HERE7")
      print(str(rv$glens_etable_final))
      print(str(rv$glens_year_filtered))
      
      rv$glens_year_filtered <- rv$glens_etable_final
      
      # 2. Update the Slider SECOND (now it's safe to trigger observers)
      min_year <- min(as.numeric(rv$glens_year_filtered$Year), na.rm = TRUE)
      max_year <- max(as.numeric(rv$glens_year_filtered$Year), na.rm = TRUE)
      if (is.finite(min_year) && is.finite(max_year)) {
        updateSliderInput(session, "year_slider", value = c(min_year, max_year), min = min_year, max = max_year)
        shinyjs::show("year_slider")
      }
      print("HERE8")
      # 3. Render the initial plots
      render_skeleton_plots(rv, output)
      plot_glens_table(rv, output, session)
      
      # Render Full Data Network
      output$network_full <- renderVisNetwork({
        req(rv$glens_etable_final) # Ensure data exists
        
        net_data <- build_collaboration_network(rv$glens_etable_final, rv$author_list)
        
        visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
          visNodes(font = list(size = 14)) %>%
          visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
          # Add physics for a nice floating layout
          # visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -50)) %>%
          # visPhysics(
          #   solver = "barnesHut", 
          #   barnesHut = list(
          #     gravitationalConstant = -2000, 
          #     springConstant = 0.04, # Stiffer springs respect 'length' better
          #     avoidOverlap = 0.1     # Prevents nodes from perfectly stacking
          #   ),
          #   stabilization = list(enabled = TRUE, iterations = 200)
          # ) %>%
          #IgraphLayout overrides physics and is fast
          visIgraphLayout(layout = "layout_with_fr") %>%
          visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE) %>%
          # visLegend() %>%
          addFontAwesome() # CRITICAL: This is required to render the user icons!
      })
      
      rv$is_glens_exec <- FALSE   
      shinyjs::delay(1500, shinyjs::hide("progress_overlay"))
      shinyjs::enable("submit_button")
      
    }) %...!% (function(err) {
      if (rv$is_cancelled) return(NULL)
      shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 100, status = "danger", title = "Process Failed!")
      output$log <- renderText(sprintf("Failed in DOI/Scopus Processing: %s", conditionMessage(err)))
      shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
      shinyjs::enable("submit_button")
    })
    
  }) #observeEVENT(submit_button)
  
}
