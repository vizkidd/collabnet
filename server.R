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
suppressPackageStartupMessages(require(future.callr))
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
suppressPackageStartupMessages(require(ipc))
suppressPackageStartupMessages(require(parallel))
suppressPackageStartupMessages(require(ipc))
# suppressPackageStartupMessages(require(QuickBLAST))

is_WASM <- grepl(pattern="wasm",x=Sys.info()["machine"])

# use a multisession plan so futures run in background R sessions
# if(!is_WASM){
  # future::plan(future::multisession)
# future::plan(future.callr::callr)
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

source("GScholarLENS-ProcessJCR.R", local = TRUE)
source("GScholarLENS-DOI2Data.R", local = TRUE)
source("GScholarLENS-ORCID2Data.R", local = TRUE)
source("GScholarLENS-SCOPUS2Data.R", local = TRUE)
source("GScholarLENS-Data2GLENS.R", local = TRUE)
source("GScholarLENS-PlotGLENS.R", local = TRUE)
source("GScholarLENS-helpers.R",local=TRUE)

server <- function(input, output, session) {
  rv <- reactiveValues(
    author_list=list(),
    glens_full_table = data.frame(),
    glens_etable_final = data.frame(),
    glens_year_filtered = data.frame(),
    # glens_dropped_data = data.frame(),
    summary_table = data.frame(),
    scopus_df = data.frame(),
    scopus_future_list = list(),
    wos_df = data.frame(),
    semantic_df = data.frame(),
    doi_count = 0,
    sh_index = 0,
    is_glens_exec = F,
    is_cancelled = F,
    extended_controls = F,
    auto_refresh_lookup = T,
    ext_match = T,
    ignore_case = T,
    log_text = NULL,
    acounts_plotly = NULL,
    ccounts_plotly = NULL,
    cdist_plotly = NULL,
    aperc_plotly = NULL,
    cperc_plotly = NULL,
    has_key_scopus = F,
    has_key_wos = F,
    has_key_opencites = F,
    has_key_semantic = F,
    has_key_crossref = F
  )
  
  shinyjs::hide(id =  "year_slider")
  # shinyjs::hide("sh_index")
  # shinyjs::hide("summary_table")
  shinyjs::hide("acounts_plot")
  shinyjs::hide("ccounts_plot")
  shinyjs::hide("cdist_plot")
  shinyjs::hide("aperc_plot")
  shinyjs::hide("cperc_plot")
  shinyjs::hide("network_filtered")
  # shinyjs::hide("network_full")
  # shinyjs::hide("extended_table")
  shinyjs::hide("progress_overlay") 
  
  # Try VFS first. If it fails or doesn't exist, fall back to what's already in glens_env
  get_resolved_key <- function(filename, env_fallback) {
    path <- file.path("keys", filename)
    message(paste("Looking for key:", path))
    if (fs::file_exists(path)) {
      tryCatch({
        dec <- sodium::data_decrypt(readRDS(path), key=openssl::sha256(glens_env$privkey_dec))
        return(trimws(rawToChar(dec)))
      }, error = function(e) { return(env_fallback) }) # Fallback on decrypt error
    }
    return(env_fallback) # Fallback if file is missing (WebR refresh)
  }
  
  # --- 1. Scopus ---
  glens_env$scopus_key <- get_resolved_key("scopus.key", glens_env$scopus_key)
  if (!is.null(glens_env$scopus_key) && glens_env$scopus_key != "") {
    shinyjs::show("scopus_bar_container")
    shinyjs::show("scopusid_label_wrapper")
    shinyjs::show("scopusid_text")
  } else {
    shinyjs::hide("scopus_bar_container")
    shinyjs::hide("scopusid_label_wrapper")
    shinyjs::hide("scopusid_text")
  }
  # --- 2. Web of Science ---
  glens_env$wos_key <- get_resolved_key("wos.key", glens_env$wos_key)
  if (!is.null(glens_env$wos_key) && glens_env$wos_key != "") {
    shinyjs::show("wos_bar_container")
  } else {
    shinyjs::hide("wos_bar_container")
  }
  # --- 3. Semantic Scholar ---
  glens_env$semantic_key <- get_resolved_key("semantic.key", glens_env$semantic_key)
  if (!is.null(glens_env$semantic_key) && glens_env$semantic_key != "") {
    shinyjs::show("semantic_bar_container")
  } else {
    shinyjs::hide("semantic_bar_container")
  }
  # --- 4. Crossref ---
  glens_env$crossref_key  <- get_resolved_key("crossref.key", glens_env$crossref_key)
  if (!is.null(glens_env$crossref_key) && glens_env$crossref_key != "") {
    shinyjs::show("crossref_bar_container")
  } else {
    shinyjs::hide("crossref_bar_container")
  }
  # --- 5. OpenCitations ---
  glens_env$opencites_key <- get_resolved_key("opencites.key", glens_env$opencites_key)
  if (!is.null(glens_env$opencites_key) && glens_env$opencites_key != "") {
    shinyjs::show("opencites_bar_container")
  } else {
    shinyjs::hide("opencites_bar_container")
  }

  output$log <- renderUI({
    # Safely handle empty or missing log text so it never sends 'undefined' to JS
    if (is.null(rv$log_text) || length(rv$log_text) == 0 || is.na(rv$log_text)) {
      return(HTML("")) 
    }
    
    # Properly render the HTML tags inside the log
    return(HTML(rv$log_text))
  })
  
  observeEvent(input$browser_stored_keys, {
    keys <- input$browser_stored_keys
    
    if (!is.null(keys$scopus_key) && keys$scopus_key != "") glens_env$scopus_key <- keys$scopus_key
    if (!is.null(keys$wos_key) && keys$wos_key != "") glens_env$wos_key <- keys$wos_key
    if (!is.null(keys$semantic_key) && keys$semantic_key != "") glens_env$semantic_key <- keys$semantic_key
    if (!is.null(keys$crossref_key) && keys$crossref_key != "") glens_env$crossref_key <- keys$crossref_key
    if (!is.null(keys$opencites_key) && keys$opencites_key != "") glens_env$opencites_key <- keys$opencites_key
    
    if (!is.null(glens_env$scopus_key) && glens_env$scopus_key != "") {
      shinyjs::show("scopus_bar_container")
      shinyjs::show("scopusid_label_wrapper")
      shinyjs::show("scopusid_text")
    } else {
      shinyjs::hide("scopus_bar_container")
      shinyjs::hide("scopusid_label_wrapper")
      shinyjs::hide("scopusid_text")
    }
    # --- 2. Web of Science ---
    if (!is.null(glens_env$wos_key) && glens_env$wos_key != "") {
      shinyjs::show("wos_bar_container")
    } else {
      shinyjs::hide("wos_bar_container")
    }
    # --- 3. Semantic Scholar ---
    if (!is.null(glens_env$semantic_key) && glens_env$semantic_key != "") {
      shinyjs::show("semantic_bar_container")
    } else {
      shinyjs::hide("semantic_bar_container")
    }
    # --- 4. Crossref ---
    if (!is.null(glens_env$crossref_key) && glens_env$crossref_key != "") {
      shinyjs::show("crossref_bar_container")
    } else {
      shinyjs::hide("crossref_bar_container")
    }
    # --- 5. OpenCitations ---
    if (!is.null(glens_env$opencites_key) && glens_env$opencites_key != "") {
      shinyjs::show("opencites_bar_container")
    } else {
      shinyjs::hide("opencites_bar_container")
    }
    
  })
  
  # output$log <- renderText({
  #   files <- list.files(getwd(), all.files = TRUE, recursive = TRUE)
  #   paste(
  #     "Current working dir:", getwd(),
  #     "\n\nFiles in VFS:\n", 
  #     paste(files, collapse = "\n")
  #   )
  # })
  
  # output$extended_table <- DT::renderDataTable({
  #   req(rv$glens_year_filtered, nrow(rv$glens_year_filtered) > 0)
  #   # print(paste("Rows:", nrow(rv$glens_year_filtered)))
  #   datatable(
  #     rv$glens_year_filtered,
  #     extensions = 'Buttons', # 1. Load the extension
  #     options = list(
  #       scrollY = "600px",
  #       scrollX = TRUE,
  #       paging = TRUE,
  #       dom = 'Blfrtip',       # 2. Add 'B' to the layout (B = Buttons)
  #       lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
  #       buttons = c('copy', 'csv', 'excel', 'pdf', 'print') # 3. Define buttons
  #     )
  #   )
  # })
  # 
  # output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
  # 
  # output$sh_index <- renderUI({
  #   # Only render if sh_index exists and is not NULL
  #   req(rv$sh_index) 
  #   
  #   tags$div(
  #     class = "sh-index-container", # Uses the CSS class for the badge look
  #     style = "display: inline-flex; align-items: baseline; background-color: #f0f7ff; 
  #              padding: 10px 18px; border-radius: 8px; border: 1px solid #cce4fc; 
  #              margin-top: 5px;",
  #     
  #     # Label part
  #     tags$span(
  #       "Sh-Index", 
  #       style = "color: #4a5568; font-size: 13px; font-weight: 600; text-transform: uppercase; 
  #                letter-spacing: 0.5px; margin-right: 12px;"
  #     ),
  #     
  #     # Value part (Large and Bold)
  #     tags$span(
  #       rv$sh_index, 
  #       style = "color: #4B8BBE; font-size: 26px; font-weight: 800; line-height: 1;"
  #     )
  #   )
  # })
  
  observeEvent(input$upload_btn, {
    
    tryCatch({
      # --- SUCCESS STATE ---
      shinyjs::runjs("
      document.getElementById('upload_text').innerText = ' Upload Success!';
      document.getElementById('upload_icon').className = 'fa fa-check';
      document.getElementById('upload_icon').style.color = '#28a745';
    ")
      
      message("FILE UPLOADED!")
      
      # 2. Iterate through each uploaded file
      imported_data_list <- lapply(seq_len(nrow(input$upload_btn)), function(i) {
        file_name <- input$upload_btn$name[i]
        file_path <- input$upload_btn$datapath[i]
        ext <- tolower(tools::file_ext(file_name))
        
        df <- switch(ext,
                     "csv"  = read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE),
                     "tsv"  = read.delim(file_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE),
                     "xlsx" = readxl::read_excel(file_path),
                     "xls"  = readxl::read_excel(file_path),
                     {
                       warning(paste("Unsupported file extension:", ext))
                       NULL 
                     }
        )
        return(df)
      })
      
      rv$imported_data_list <- Filter(Negate(is.null), imported_data_list)
      
      # --- TRANSITION DELAY ---
      shinyjs::delay(1500, {
        shinyjs::runjs("
        document.getElementById('upload_text').innerText = '';
        document.getElementById('upload_icon').className = 'fa fa-upload';
        document.getElementById('upload_icon').style.color = ''; 
        document.getElementById('upload_btn').value = ''; 
      ")
        
        showModal(modalDialog(
          title = tags$span(icon("columns", lib = "font-awesome"), " Step 1: Column Mapping"),
          size = "m",
          radioButtons("col_import_type", label = "Choose Column import type:", inline = TRUE, choices = c("Common Columns", "All Columns", "Map")),
          uiOutput("column_mapping_ui"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("next_row_merge", "Next: Configure Rows", 
                         class = "btn-primary", 
                         disabled = "disabled", 
                         style = "pointer-events: none; opacity: 0.5;")
          ),
          easyClose = FALSE
        ))
        
        if (is.null(rv$glens_full_table) || ncol(rv$glens_full_table) == 0) {
          updateRadioButtons(session, "col_import_type", selected = "All Columns")
          shinyjs::delay(100, {
            shinyjs::runjs("$('input[name=\"col_import_type\"][value=\"Common Columns\"]').prop('disabled', true);")
          })
        }
      })
    }, error = function(e) {
      shinyjs::runjs("
      document.getElementById('upload_text').innerText = ' Upload Failed';
      document.getElementById('upload_icon').className = 'fa fa-times';
      document.getElementById('upload_icon').style.color = '#dc3545';
    ")
      showNotification(paste("Failed to process file:", e$message), type = "error", duration = 5)
      rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>Failed to process file:", e$message, "</span>"), sep="<br>")
      
      shinyjs::delay(3000, {
        shinyjs::runjs("
        document.getElementById('upload_text').innerText = '';
        document.getElementById('upload_icon').className = 'fa fa-upload';
        document.getElementById('upload_icon').style.color = '';
        document.getElementById('upload_btn').value = '';
      ")
      })
    })
  })
  
  observeEvent(input$next_row_merge, {
    req(rv$imported_data_list)
    imported_data_list <- rv$imported_data_list
    print("HERE2!!!!")
    rv$saved_col_import_type <- input$col_import_type 
    all_uploaded_cols <- unique(unlist(lapply(imported_data_list, names)))
    
    cols_to_keep <- c()
    rename_map <- list()
    detected_mv_cols <- list()
    
    for (col in all_uploaded_cols) {
      safe_id <- make.names(col)
      is_checked <- input[[paste0("map_chk_", safe_id)]]
      if (is.null(is_checked)) is_checked <- TRUE 
      
      if (isTRUE(is_checked)) {
        cols_to_keep <- c(cols_to_keep, col)
        mapped_name <- input[[paste0("map_name_", safe_id)]]
        final_name <- col
        
        if (!is.null(mapped_name) && trimws(mapped_name) != "" && trimws(mapped_name) != col) {
          rename_map[[col]] <- trimws(mapped_name)
          final_name <- trimws(mapped_name)
        }
        
        delim_val <- input[[paste0("map_delim_", safe_id)]]
        if (!is.null(delim_val) && trimws(delim_val) != "") {
          detected_mv_cols[[final_name]] <- trimws(delim_val)
        }
      }
    }
    
    imported_data_list <- lapply(imported_data_list, function(df) {
      valid_cols <- intersect(names(df), cols_to_keep)
      df <- df[, valid_cols, drop = FALSE]
      current_names <- names(df)
      for (i in seq_along(current_names)) {
        if (current_names[i] %in% names(rename_map)) {
          current_names[i] <- rename_map[[current_names[i]]]
        }
      }
      names(df) <- make.unique(current_names, sep = "_")
      return(df)
    })
    
    merged_df <- dplyr::bind_rows(imported_data_list) %>% dplyr::distinct()
    rv$intermediate_merged_df <- merged_df
    rv$detected_mv_cols <- detected_mv_cols
    
    removeModal()
    
    if (length(detected_mv_cols) > 0) {
      showModal(modalDialog(
        title = tags$span(icon("cut", lib = "font-awesome"), " Step 1.5: Confirm Splits"),
        size = "l",
        uiOutput("delimiter_ui"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("next_after_delim", "Apply Splits & Continue to Step 2", class = "btn-warning")
        ),
        easyClose = FALSE
      ))
    } else {
      show_row_merge_modal(rv, session)
    }
  })
  
  observeEvent(input$next_after_delim, {
    req(rv$intermediate_merged_df, rv$detected_mv_cols)
    merged_df <- rv$intermediate_merged_df
    
    for(col in names(rv$detected_mv_cols)) {
      action <- input[[paste0("delim_action_", make.names(col))]]
      delim_val <- input[[paste0("delim_val_", make.names(col))]]
      
      if (!is.null(action) && action == "rows" && !is.null(delim_val) && trimws(delim_val) != "") {
        sep_regex <- paste0("\\s*", escape_regex_inline(delim_val), "\\s*")
        merged_df <- merged_df %>% 
          tidyr::separate_rows(dplyr::all_of(col), sep = sep_regex) %>%
          dplyr::mutate(!!col := trimws(.data[[col]]))
      }
    }
    
    rv$intermediate_merged_df <- merged_df
    removeModal()
    show_row_merge_modal(rv, session)
  })
  
  observeEvent(input$confirm_import, {
    req(rv$intermediate_merged_df)
    merged_df <- rv$intermediate_merged_df
    rv$glens_full_table_tmp <- rv$glens_full_table
    
    # Force all columns in both datasets to be character text.
    raw_df <- merged_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    
    # -------------------------------------------------------------------------
    # --- 1. SAFELY CONSOLIDATE & RENAME KNOWN COLUMNS (MOVED UP) ---
    # -------------------------------------------------------------------------
    target_mappings <- list(
      "orcid" = c("orcid", "ORCiD", "Orcid", "ORCID"),
      "SCOPUS_ID" = c("SCOPUS_ID", "SCOPUS ID", "Scopus ID", "Author(s) ID"),
      "Citations" = c("Citations", "Cited by", "citedby-count"),
      "User_Journal" = c("User_Journal", "Source title", "prism:publicationName"),
      "doi" = c("doi", "DOI"),
      "Authors" = c("Authors", "author")
    )
    
    for(targ in names(target_mappings)) {
      aliases <- target_mappings[[targ]]
      # Find which of the aliases actually exist in the current dataframe
      found_cols <- intersect(aliases, colnames(raw_df))
      
      if (length(found_cols) > 0) {
        master_vec <- rep(NA_character_, nrow(raw_df))
        
        # Coalesce all found columns into one master vector
        for(fc in found_cols) {
          master_vec <- dplyr::coalesce(master_vec, as.character(raw_df[[fc]]))
        }
        
        # Assign the master merged column
        raw_df[[targ]] <- master_vec
        
        # Drop the old alias columns so the dataset stays clean
        drop_cols <- setdiff(found_cols, targ)
        if (length(drop_cols) > 0) {
          raw_df <- raw_df %>% dplyr::select(-dplyr::all_of(drop_cols))
        }
      }
    }
    
    # --- 2. PARSE THE TARGET AUTHORS ---
    target_variants <- stringi::stri_omit_empty(stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n"))))
    target_variants <- target_variants[target_variants != ""]
    
    if(length(target_variants) > 0) {
      rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
        vn <- normalize_name(v)
        list(norm = vn, parts = extract_parts(vn))
      })
      rv$author_match_regex <- build_name_regex_for_variants(target_variants)
    } else {
      rv$target_variants_norm <- NULL
      rv$author_match_regex <- NULL
    }
    
    print("confirm_import:extend_input_table():")
    # --- 3. THE HEAVY LIFTING (Hybrid Paradigm) ---
    # NOW raw_df has standardized column names!
    extended_df <- extend_input_table(rv, raw_df, rv$author_match_regex, rv$target_variants_norm)
    merged_df <- match_journals(rv, extended_df)
    
    # --- 4. NA REMOVAL ---
    # Keep rows if ANY column has a non-NA value (drops rows where ALL are NA)
    merged_df <- merged_df %>%
      dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))
    
    # Keep columns if they don't have ALL NA values (drops columns where ALL are NA)
    merged_df <- merged_df %>%
      dplyr::select(dplyr::where(~ !all(is.na(.))))
    
    # --- 5. HANDLE ROW LOGIC ---
    if (input$row_import_type == "New" || is.null(rv$glens_full_table)) {
      glens_full_table <- merged_df
      
    } else if (input$row_import_type == "Append") {
      
      if (rv$saved_col_import_type == "Common Columns") {
        final_common_cols <- intersect(names(rv$glens_full_table), names(merged_df))
        glens_full_table <- dplyr::bind_rows(
          rv$glens_full_table[, final_common_cols, drop = FALSE],
          merged_df[, final_common_cols, drop = FALSE]
        )
      } else {
        print("MERGING:")
        glens_full_table <- dplyr::bind_rows(rv$glens_full_table %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)), merged_df)
      }
      
    } else if (input$row_import_type == "Merge") {
      
      join_keys <- input$row_merge_keys
      join_type <- input$join_type 
      
      print("join_keys:")
      print(join_keys)
      print("join_type:")
      print(join_type)
      
      if (!is.null(join_keys) && length(join_keys) > 0) {
        
        overlap_cols <- setdiff(intersect(names(rv$glens_full_table), names(merged_df)), join_keys)
        
        join_func <- switch(join_type,
                            "inner" = dplyr::inner_join,
                            "left"  = dplyr::left_join,
                            "right" = dplyr::right_join,
                            "full"  = dplyr::full_join)
        
        joined_df <- join_func(
          rv$glens_full_table, 
          merged_df, 
          by = join_keys,  
          suffix = c(".old", ".new"),
          relationship = "many-to-many" 
        )
        
        for(col in overlap_cols) {
          old_col <- paste0(col, ".old")
          new_col <- paste0(col, ".new")
          
          old_vals <- as.character(joined_df[[old_col]])
          new_vals <- as.character(joined_df[[new_col]])
          
          joined_df[[col]] <- dplyr::coalesce(old_vals, new_vals)
          
          joined_df[[old_col]] <- NULL
          joined_df[[new_col]] <- NULL
        }
        
        joined_df <- joined_df %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
          tidyr::fill(dplyr::everything(), .direction = "downup") %>%
          dplyr::ungroup()
        
        glens_full_table <- joined_df
        
      } else {
        warning("No join keys selected. Falling back to Append.")
        glens_full_table <- dplyr::bind_rows(rv$glens_full_table %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)), merged_df)
      }
    }
    
    if(nrow(glens_full_table) <= 0){
      showNotification("Data import/merge returned empty rows. Try different options", type = "error", duration = 10)
      rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>Data import/merge returned empty rows. Try different options </span>"),sep="<br>")
      rv$imported_data_list <- NULL
      rv$intermediate_merged_df <- NULL
      rv$saved_col_import_type <- NULL
      rv$glens_full_table <- rv$glens_full_table_tmp
      removeModal()
      return()
    }
    
    # --- 6. CHECK FOR MISSING REQUIRED COLUMNS ---
    missing_cols <- setdiff(collabnet_required_cols, colnames(glens_full_table))
    if(length(missing_cols) > 0) {
      missing_str <- paste(missing_cols, collapse=", ")
      rv$log_text <- paste(rv$log_text, 
                           paste0("<span style='color: red;'>Missing required columns: ", missing_str, "</span>"), 
                           sep="<br>")
      showNotification(paste("Missing columns:", missing_str), type = "error", duration = 10)
    }
    
    glens_full_table <- dplyr::distinct(glens_full_table)
    # Cleanup & Finalize
    rv$glens_full_table <- glens_full_table
    
    # Initialize empty Skeletons so they are ready for the Proxy
    render_skeleton_plots(rv, glens_full_table, output)
    
    # Configure Slider safely
    years <- as.numeric(na.omit(glens_full_table$Year))
    if (length(years) > 0) {
      min_yr <- min(years)
      max_yr <- max(years)
      updateSliderInput(session, "year_slider", min = min_yr, max = max_yr, value = c(min_yr, max_yr))
    }
    
    # Trigger a manual update if auto-refresh is OFF
    if (!isTRUE(input$auto_refresh_lookup)) {
      # Increment a counter to signal the reactive graph
      rv$manual_submit <- if(is.null(rv$manual_submit)) 1 else rv$manual_submit + 1
    }
    
    rv$imported_data_list <- NULL
    rv$intermediate_merged_df <- NULL
    rv$saved_col_import_type <- NULL
    rv$log_text <- paste(rv$log_text, paste("Post-Import Total:",nrow(rv$glens_full_table),"lines..."),sep="<br>")
    
    saveRDS(rv$glens_full_table, "glens_full_table.rds")
    
    removeModal()
  })
  
  # output$dynamic_author_filter <- renderUI({
  #   req(input$author_list)
  #   # 1. Get the raw text from the text area
  #   raw_text <- input$author_list
  #   # 2. If it's empty or NULL, don't show the panel
  #   if (is.null(raw_text) || trimws(raw_text) == "") {
  #     return(NULL)
  #   }
  #   # 3. Split by newline and remove any empty/blank lines
  #   author_list <- strsplit(raw_text, "\n")[[1]]
  #   author_list <- author_list[trimws(author_list) != ""]
  #   # 4. If there is more than 1 valid author name, render the wellPanel
  #   if (length(author_list) > 1) {
  #     wellPanel(
  #       tags$h5(icon("users-cog"), " Relationship Filter", class = "text-primary"),
  #       tags$p("Filter the publication list based on how the lookup keywords interact.", class = "text-muted"),
  #       
  #       radioButtons(
  #         inputId = "author_logic_gate",
  #         label = NULL,
  #         choices = c(
  #           "Full Data" = "FULL",
  #           "Co-patriot/Collaborator (OR)" = "OR",
  #           "Companion (AND)"              = "AND",
  #           "Rival (XOR)"                  = "XOR",
  #           "Ignore (NOR)"                 = "NOR",
  #           "Divide & Exclude (NAND)"                = "NAND"
  #         ),
  #         selected = "OR",
  #         width = "100%"
  #       )
  #     )
  #   } else {
  #     # If 1 or 0 authors, hide the panel
  #     return(NULL)
  #   }
  # })
  
  output$dynamic_author_filter <- renderUI({
    req(input$author_list)
    
    # 1. Get the raw text
    raw_text <- input$author_list
    if (is.null(raw_text) || trimws(raw_text) == "") return(NULL)
    
    # 2. Parse the author list
    author_list <- strsplit(raw_text, "\n")[[1]]
    author_list <- author_list[trimws(author_list) != ""]
    
    # --- STATE PRESERVATION LOGIC ---
    # Check if the user has already selected something. 
    # Use isolate() so that changing the radio button itself doesn't trigger this renderUI.
    current_selection <- isolate(input$author_logic_gate)
    
    # Fallback to "OR" if nothing is selected yet
    final_selected <- if (!is.null(current_selection)) current_selection else "OR"
    # --------------------------------
    
    # 3. Render the panel
    if (length(author_list) > 1) {
      wellPanel(
        tags$h5(icon("users-cog"), " Relationship Filter", class = "text-primary"),
        tags$p("Filter the publication list based on how the lookup keywords interact.", class = "text-muted"),
        
        radioButtons(
          inputId = "author_logic_gate",
          label = NULL,
          choices = c(
            "Full Data" = "FULL",
            "Co-patriot/Collaborator (OR)" = "OR",
            "Companion (AND)"              = "AND",
            "Rival (XOR)"                  = "XOR",
            "Ignore (NOR)"                 = "NOR",
            "Divide & Exclude (NAND)"      = "NAND"
          ),
          selected = final_selected, # Use the preserved state here!
          width = "100%"
        )
      )
    } else {
      return(NULL)
    }
  })
  
  
  output$row_merge_ui <- renderUI({
    # if (is.null(rv$glens_etable_final) || ncol(rv$glens_etable_final) == 0) {
    #   return(tags$div(class = "alert alert-warning", "No existing data to merge with. Selecting 'New'."))
    # }
    
    unlock_script <- tags$script(HTML("
      setTimeout(function(){ 
        var btn = $('#confirm_import');
        btn.prop('disabled', false);
        btn.css('pointer-events', 'auto');
        btn.css('opacity', '1');
      }, 300);
    "))
    
    if (is.null(rv$glens_full_table) || ncol(rv$glens_full_table) == 0) {
      return(tagList(
        tags$div(class = "alert alert-warning", "No existing data to merge with. Selecting 'New'."),
        unlock_script # Make sure they can click confirm!
      ))
    }
    req(input$row_import_type == "Merge")
    
    if (!is.null(rv$glens_full_table) && !is.null(rv$intermediate_merged_df)) {
      # Only show keys that exist in BOTH datasets to prevent join errors
      common_keys <- intersect(names(rv$glens_full_table), names(rv$intermediate_merged_df))
      
      tagList(
        tags$div(
          style = "margin-bottom: 15px; padding: 10px; border-left: 3px solid #17a2b8; background-color: #f8f9fa;",
          
          # Primary Key Selector
          selectizeInput("row_merge_keys", "Select Primary Key(s) to Join By:", 
                         choices = common_keys, multiple = TRUE, width = "100%",
                         options = list(placeholder = "Select one or more keys (e.g., orcid, Title)")),
          
          # NEW: Join Type Selector
          selectInput("join_type", "Select Join Type:", width = "100%",
                      choices = c(
                        "Full Join (Keep ALL rows from both)" = "full",
                        "Inner Join (Keep ONLY rows that match exactly)" = "inner",
                        "Left Join (Keep ALL Existing App Data, drop unmapped Uploaded Data)" = "left",
                        "Right Join (Keep ALL Uploaded Data, drop unmapped App Data)" = "right"
                      ), 
                      selected = "full"),
          
          tags$small(style = "color: #666;", 
                     "Missing data within joined rows will be intelligently backfilled regardless of join type.")
        ),
        unlock_script # Unlock when the merge UI finishes painting
      )
    }
  })
  
  output$delimiter_ui <- renderUI({
    req(rv$detected_mv_cols)
    
    mapping_rows <- lapply(names(rv$detected_mv_cols), function(col) {
      safe_id <- paste0("delim_action_", make.names(col))
      delim_id <- paste0("delim_val_", make.names(col))
      
      suggested_delim <- rv$detected_mv_cols[[col]]
      
      fluidRow(
        style = "margin-bottom: 10px; align-items: flex-end; display: flex; background: #f8f9fa; padding: 10px; border-radius: 5px;",
        column(4, tags$strong(col, style="word-break: break-all; color: #333;")),
        column(4, textInput(delim_id, "Delimiter:", value = suggested_delim)),
        column(4, 
               selectizeInput(safe_id, "Action:",  # <--- Changed to selectizeInput
                              choices = c("Split to Rows (Lengthen)" = "rows", "Do Not Split" = "none"), 
                              selected = "none",
                              options = list(dropdownParent = 'body')) # <--- Now this is perfectly valid!
        )
      )
    })
    
    tagList(
      tags$div(style = "max-height: 450px; overflow-y: auto; overflow-x: hidden; padding-right: 10px;",
               mapping_rows)
    )
  })
  
  observeEvent(input$row_import_type, {
    if (input$row_import_type %in% c("New", "Append")) {
      # If New or Append, no UI is needed, so unlock the button immediately
      shinyjs::runjs("
      var btn = $('#confirm_import');
      btn.prop('disabled', false);
      btn.css('pointer-events', 'auto');
      btn.css('opacity', '1');
    ")
    } else if (input$row_import_type == "Merge") {
      # If they click Merge, immediately lock the button until the UI finishes loading it
      shinyjs::runjs("
      var btn = $('#confirm_import');
      btn.prop('disabled', true);
      btn.css('pointer-events', 'none');
      btn.css('opacity', '0.5');
    ")
    }
  }, ignoreInit = TRUE)
  
  #observe for in-place split, combine, drop buttons
  observe({
    req(rv$imported_data_list, length(rv$imported_data_list) > 0)
    uploaded_cols <- names(rv$imported_data_list[[1]])
    
    lapply(uploaded_cols, function(col) {
      safe_id <- make.names(col)
      
      # Observe Split Button
      observeEvent(input[[paste0("btn_split_", safe_id)]], {
        message("Split triggered for column: ", col)
        # Trigger split modal or logic here
      }, ignoreInit = TRUE)
      
      # Observe Combine Button
      observeEvent(input[[paste0("btn_combine_", safe_id)]], {
        message("Combine triggered for column: ", col)
        # Trigger combine modal or logic here
      }, ignoreInit = TRUE)
      
      # Observe Drop Button
      observeEvent(input[[paste0("btn_drop_", safe_id)]], {
        message("Drop triggered for column: ", col)
        # Example: update the checkbox to FALSE when dropped
        updateCheckboxInput(session, paste0("map_chk_", safe_id), value = FALSE)
      }, ignoreInit = TRUE)
    })
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
  
  # observeEvent(input$confirm_import, {
  #   req(rv$intermediate_merged_df)
  #   merged_df <- rv$intermediate_merged_df
  #   rv$glens_full_table_tmp <- rv$glens_full_table
  #   
  #   # Force all columns in both datasets to be character text.
  #   raw_df <- merged_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  #   
  #   # --- 2. PARSE THE TARGET AUTHORS ---
  #   # We must do this here so the extend function knows exactly who to search for
  #   target_variants <- stringi::stri_omit_empty(stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n"))))
  #   target_variants <- target_variants[target_variants != ""]
  #   
  #   if(length(target_variants) > 0) {
  #     rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
  #       vn <- normalize_name(v)
  #       list(norm = vn, parts = extract_parts(vn))
  #     })
  #     rv$author_match_regex <- build_name_regex_for_variants(target_variants)
  #   } else {
  #     rv$target_variants_norm <- NULL
  #     rv$author_match_regex <- NULL
  #   }
  #   
  #   
  #   # --- 3. THE HEAVY LIFTING (Hybrid Paradigm) ---
  #   # Pass raw_df directly into the extension and matching pipeline
  #   extended_df <- extend_input_table(rv, raw_df, rv$author_match_regex, rv$target_variants_norm)
  #   merged_df <- match_journals(rv, extended_df)
  #   
  #   # --- Apply NA Removal ---
  #   # if (input$drop_na_rows) {
  #   #   # Drops any row that has an NA in ANY column
  #   #   rv$glens_etable_final <- tidyr::drop_na(rv$glens_etable_final)
  #   # }
  #   # 
  #   # if (input$drop_na_cols) {
  #   #   # Drops any column that has an NA in ANY row
  #   #   rv$glens_etable_final <- rv$glens_etable_final %>%
  #   #     dplyr::select(dplyr::where(~ !any(is.na(.))))
  #   # }
  #   # Keep rows if ANY column has a non-NA value (drops rows where ALL are NA)
  #   merged_df <- merged_df %>%
  #     dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))
  #   
  #   # Keep columns if they don't have ALL NA values (drops columns where ALL are NA)
  #   merged_df <- merged_df %>%
  #     dplyr::select(dplyr::where(~ !all(is.na(.))))
  #   
  #   # ------------------------------
  #   # --- 1. Safely Consolidate & Rename Known Columns ---
  #   # Define all the variations of names that might come from different files
  #   target_mappings <- list(
  #     "orcid" = c("orcid", "ORCiD", "Orcid", "ORCID"),
  #     "SCOPUS_ID" = c("SCOPUS_ID", "SCOPUS ID", "Scopus ID", "Author(s) ID"),
  #     "Citations" = c("Citations", "Cited by"),
  #     "User_Journal" = c("User_Journal", "Source title"),
  #     "doi" = c("doi", "DOI")
  #   )
  #   
  #   for(targ in names(target_mappings)) {
  #     aliases <- target_mappings[[targ]]
  #     # Find which of the aliases actually exist in the current dataframe
  #     found_cols <- intersect(aliases, colnames(merged_df))
  #     
  #     if (length(found_cols) > 0) {
  #       master_vec <- rep(NA_character_, nrow(merged_df))
  #       
  #       # Coalesce all found columns into one master vector (forcing character to avoid type crashes)
  #       for(fc in found_cols) {
  #         master_vec <- dplyr::coalesce(master_vec, as.character(merged_df[[fc]]))
  #       }
  #       
  #       # Assign the master merged column
  #       merged_df[[targ]] <- master_vec
  #       
  #       # Drop the old alias columns so the dataset stays clean
  #       drop_cols <- setdiff(found_cols, targ)
  #       if (length(drop_cols) > 0) {
  #         merged_df <- merged_df %>% dplyr::select(-dplyr::all_of(drop_cols))
  #       }
  #     }
  #   }
  #   
  #   # 5. Handle Row Logic
  #   if (input$row_import_type == "New" || is.null(rv$glens_full_table)) {
  #     glens_full_table <- merged_df
  #     
  #   } else if (input$row_import_type == "Append") {
  #     
  #       if (rv$saved_col_import_type == "Common Columns") {
  #       final_common_cols <- intersect(names(rv$glens_full_table), names(merged_df))
  #       glens_full_table <- dplyr::bind_rows(
  #         rv$glens_full_table[, final_common_cols, drop = FALSE],
  #         merged_df[, final_common_cols, drop = FALSE]
  #       )
  #     } else {
  #       print("MERGING:")
  #       glens_full_table <- dplyr::bind_rows(rv$glens_full_table %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)), merged_df)
  #     }
  #     
  #   } else if (input$row_import_type == "Merge") {
  #     
  #     join_keys <- input$row_merge_keys
  #     join_type <- input$join_type # Grab the selected join type
  #     
  #     print("join_keys:")
  #     print(join_keys)
  #     print("join_type:")
  #     print(join_type)
  #     
  #     if (!is.null(join_keys) && length(join_keys) > 0) {
  #       
  #       overlap_cols <- setdiff(intersect(names(rv$glens_full_table), names(merged_df)), join_keys)
  #       
  #       # 1. Dynamically select the join function based on the dropdown
  #       join_func <- switch(join_type,
  #                           "inner" = dplyr::inner_join,
  #                           "left"  = dplyr::left_join,
  #                           "right" = dplyr::right_join,
  #                           "full"  = dplyr::full_join)
  #       
  #       # 2. Execute the join
  #       joined_df <- join_func(
  #         rv$glens_full_table, 
  #         merged_df, 
  #         by = join_keys,  
  #         suffix = c(".old", ".new"),
  #         relationship = "many-to-many" 
  #       )
  #       
  #       # 3. Coalesce overlapping columns (prioritizing old data, filling gaps with new data)
  #       for(col in overlap_cols) {
  #         old_col <- paste0(col, ".old")
  #         new_col <- paste0(col, ".new")
  #         
  #         # Force both to character to prevent integer/character mismatch crashes
  #         old_vals <- as.character(joined_df[[old_col]])
  #         new_vals <- as.character(joined_df[[new_col]])
  #         
  #         joined_df[[col]] <- dplyr::coalesce(old_vals, new_vals)
  #         
  #         joined_df[[old_col]] <- NULL
  #         joined_df[[new_col]] <- NULL
  #       }
  #       
  #       # 4. Spread metadata up and down grouped by ALL selected keys
  #       joined_df <- joined_df %>%
  #         dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
  #         tidyr::fill(dplyr::everything(), .direction = "downup") %>%
  #         dplyr::ungroup()
  #       
  #       glens_full_table <- joined_df
  #       
  #     } else {
  #       warning("No join keys selected. Falling back to Append.")
  #       # rv$glens_full_table <- dplyr::bind_rows(rv$glens_full_table, merged_df)
  #       glens_full_table <- dplyr::bind_rows(rv$glens_full_table %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)), merged_df)
  #     }
  #   }
  #   
  #   if(nrow(glens_full_table) <= 0){
  #     showNotification("Data import/merge returned empty rows. Try different options", type = "error", duration = 10)
  #     rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>Data import/merge returned empty rows. Try different options </span>"),sep="<br>")
  #     rv$imported_data_list <- NULL
  #     rv$intermediate_merged_df <- NULL
  #     rv$saved_col_import_type <- NULL
  #     rv$glens_full_table <- rv$glens_full_table_tmp
  #     # rv$glens_input_table <- NULL
  #     # rv$glens_year_filtered <- NULL
  #     removeModal()
  #     return()
  #   }
  #   
  #   # #Checks to make sure CollabNET columns exist
  #   # # Auto-fill any missing columns with NA. 
  #   # # allows multi-file upload without it getting rejected for missing columns.
  #   # for(col in collabnet_required_cols) {
  #   #   if (!(col %in% colnames(rv$glens_etable_final))) {
  #   #     rv$glens_etable_final[[col]] <- NA_character_
  #   #   }
  #   # }
  #   
  #   # print(colnames(rv$glens_etable_final))
  #   # print(nrow(rv$glens_etable_final))
  #   # print(str(rv$glens_etable_final))
  #   # print("MERGED_DF:")
  #   # print(colnames(merged_df))
  #   # print(nrow(merged_df))
  #   # print(str(merged_df))
  #   missing_cols <- setdiff(collabnet_required_cols, colnames(glens_full_table))
  #   if(length(missing_cols) > 0) {
  #     
  #     # Format the missing columns into a clean string
  #     missing_str <- paste(missing_cols, collapse=", ")
  #     # Update the log
  #     rv$log_text <- paste(rv$log_text, 
  #                          paste0("<span style='color: red;'>Missing required columns: ", missing_str, "</span>"), 
  #                          sep="<br>")
  #     # Show the smaller, targeted notification
  #     showNotification(paste("Missing columns:", missing_str), type = "error", duration = 10)
  #     # rv$imported_data_list <- NULL
  #     # rv$intermediate_merged_df <- NULL
  #     # rv$saved_col_import_type <- NULL
  #     # rv$glens_etable_final <- rv$glens_etable_final_tmp
  #     # # rv$glens_input_table <- NULL
  #     # # rv$glens_year_filtered <- NULL
  #     # removeModal()
  #     # return()
  #   }
  #   
  #   # Cleanup
  #   rv$glens_full_table <- dplyr::distinct(glens_full_table)
  #   rv$imported_data_list <- NULL
  #   rv$intermediate_merged_df <- NULL
  #   rv$saved_col_import_type <- NULL
  #   rv$log_text <- paste(rv$log_text, paste("Post-Import Total:",nrow(rv$glens_full_table),"lines..."),sep="<br>")
  #   # rv$glens_input_table <- rv$glens_etable_final
  #   
  #   
  #   # target_variants <- stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n")))
  #   # target_variants <- target_variants[target_variants != ""]
  #   # 
  #   # # Apply normalization based on Extended Matching checkbox
  #   # if (isTRUE(input$ext_match)) {
  #   #   rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
  #   #     vn <- normalize_name(v)
  #   #     list(norm = vn, parts = extract_parts(vn))
  #   #   })
  #   # } else {
  #   #   # If Extended Matching is off, skip strict normalization but respect case preference
  #   #   rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
  #   #     vn <- if(isTRUE(input$ignore_case)) tolower(v) else v
  #   #     list(norm = vn, parts = list(vn))
  #   #   })
  #   # }
  #   # 
  #   # # Pass the ignore_case UI value into the regex builder
  #   # rv$author_match_regex <- build_name_regex_for_variants(target_variants, ignore_case = input$ignore_case)
  #   # 
  #   # extend_input_table(rv)
  #   
  #   # rv$glens_year_filtered <- rv$glens_full_table
  #   
  #   # target_variants <- stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n")))
  #   # target_variants <- target_variants[target_variants != ""]
  #   # rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
  #   #   vn <- normalize_name(v)
  #   #   list(norm = vn, parts = extract_parts(vn))
  #   # })
  #   # rv$author_match_regex <- build_name_regex_for_variants(target_variants)
  #   # 
  #   # extend_input_table(rv)
  #   # 
  #   # rv$glens_year_filtered <- rv$glens_etable_final
  #   
  #   # if (nrow(rv$glens_year_filtered) <= 0) {
  #   #   rv$log_text <- paste(rv$log_text, "Import: No keywords were matched.",sep="<br>")
  #   #   # shinyjs::enable("submit_button")
  #   #   # removeModal()
  #   #   # return()
  #   # }else{
  #   #   
  #   #   # compute_indices(rv)
  #   #   # output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
  #   #   # output$sh_index <- renderUI(HTML(paste("<b>Sh-Index:</b>", "NA")))
  #   #   # output$extended_table <- DT::renderDataTable({
  #   #   #   DT::datatable(rv$glens_year_filtered, options = list(scrollY = "600px", scrollX = TRUE, paging = TRUE))
  #   #   # })
  #   #   # match_journals(rv)
  #   #   rv$glens_year_filtered <- rv$glens_full_table
  #   #   shinyjs::show("extended_table")
  #   # }
  #   # 
  #   # if(length(na.omit(levels(factor(rv$glens_year_filtered$Year)))) > 1){
  #   #   min_year <- min(as.numeric(rv$glens_year_filtered$Year), na.rm = TRUE)
  #   #   max_year <- max(as.numeric(rv$glens_year_filtered$Year), na.rm = TRUE)
  #   #   if (is.finite(min_year) && is.finite(max_year)) {
  #   #     updateSliderInput(session, "year_slider", value = c(min_year, max_year), min = min_year, max = max_year)
  #   #     shinyjs::show("year_slider")
  #   #   }else{
  #   #     shinyjs::hide("year_slider")
  #   #   }
  #   # }else{
  #   #   shinyjs::hide("year_slider")
  #   # }
  #   # # rv$extended_controls <- TRUE
  #   saveRDS(rv$glens_full_table, "glens_full_table.rds")
  #   
  #   removeModal()
  # })
  
  # --- Column Mapping UI Generation ---
  output$column_mapping_ui <- renderUI({
    req(rv$imported_data_list)
    
    # 1. Target Variants Dictionary
    target_mappings <- list(
      "orcid" = c("orcid", "ORCiD", "Orcid", "ORCID"),
      "SCOPUS_ID" = c("SCOPUS_ID", "SCOPUS ID", "Scopus ID", "Author(s) ID"),
      "Citations" = c("Citations", "Cited by", "citedby-count"),
      "User_Journal" = c("User_Journal", "Source title", "prism:publicationName", "Journal"),
      "doi" = c("doi", "DOI"),
      "Authors" = c("Authors", "author", "Author(s)"),
      "Year" = c("Year", "year", "Publication Year"),
      "Title" = c("Title", "title", "Document Title", "Article Title"),
      "Source" = c("Source", "source")
    )
    
    # 2. Define Required vs Optional Columns
    required_cols <- c("Citations", "User_Journal", "Title", "Authors", "Year", "Source")
    optional_cols <- c("Qscore", "JIF5Years", "SCOPUS_ID", "doi", "JCR_Journal", "orcid", "Name")
    
    all_uploaded_cols <- unique(unlist(lapply(rv$imported_data_list, names)))
    
    # 3. Header Panel Tags with Clickable CSS
    base_badge_style <- "display: inline-block; padding: 4px 8px; margin: 2px; border-radius: 12px; font-size: 12px; font-weight: bold; color: white; transition: transform 0.2s ease, opacity 0.2s; cursor: pointer; user-select: none;"
    
    custom_css <- tags$style(HTML("
      .clickable-badge:hover, .clickable-selected-badge:hover {
        transform: scale(1.05);
        opacity: 0.85;
      }
    "))
    
    header_tags <- tags$div(
      custom_css,
      style = "margin-bottom: 15px; padding: 10px; background: #f8f9fa; border-radius: 5px; border: 1px solid #ddd;",
      
      tags$strong("Required Columns:"),
      tags$div(
        style = "margin-top: 5px; margin-bottom: 10px; display: flex; flex-wrap: wrap;",
        lapply(required_cols, function(col) {
          tags$span(id = paste0("req-badge-", col), class = "clickable-badge", style = paste(base_badge_style, "background-color: #dc3545;"), col)
        })
      ),
      
      tags$strong("Optional Columns:"),
      tags$div(
        style = "margin-top: 5px; margin-bottom: 15px; display: flex; flex-wrap: wrap;",
        lapply(optional_cols, function(col) {
          tags$span(id = paste0("opt-badge-", col), class = "clickable-badge", style = paste(base_badge_style, "background-color: #6c757d;"), col)
        })
      ),
      
      tags$strong("Selected Columns (Click to remove):"),
      tags$div(
        id = "selected-cols-container",
        style = "margin-top: 5px; display: flex; flex-wrap: wrap;"
      )
    )
    
    # 4. Dynamic Rows Generation
    rows_tags <- lapply(all_uploaded_cols, function(col) {
      safe_id <- make.names(col)
      safe_col_compare <- trimws(tolower(col))
      prefill_name <- col
      
      for (targ in names(target_mappings)) {
        safe_targs <- trimws(tolower(target_mappings[[targ]]))
        if (safe_col_compare %in% safe_targs) {
          prefill_name <- targ
          break
        }
      }
      
      tags$div(
        class = "col-mapping-row",
        style = "display: flex; align-items: center; padding: 8px; border-bottom: 1px solid #eee; border-radius: 4px; transition: background-color 0.3s ease;",
        tags$div(style = "width: 5%;", checkboxInput(paste0("map_chk_", safe_id), "", value = TRUE)),
        tags$div(style = "width: 30%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;", tags$strong(col)),
        tags$div(style = "width: 10%; text-align: center;", icon("arrow-right")),
        tags$div(style = "width: 35%;", textInput(paste0("map_name_", safe_id), "", value = prefill_name, width = "90%")),
        tags$div(style = "width: 20%;", textInput(paste0("map_delim_", safe_id), "", placeholder = "Delim (e.g. ,)", width = "100%"))
      )
    })
    
    js_req_array <- paste0("['", paste(required_cols, collapse = "','"), "']")
    js_opt_array <- paste0("['", paste(optional_cols, collapse = "','"), "']")
    
    # 5. Javascript with Click Toggles
    js_script <- tags$script(HTML(paste0("
    function updateRequiredColumns() {
      var required = ", js_req_array, ";
      var optional = ", js_opt_array, ";
      
      $('.col-mapping-row').css('background-color', '');
      var foundReq = [];
      var foundOpt = [];
      var selectedCols = [];
      
      $('.col-mapping-row input[type=\"text\"][id^=\"map_name_\"]').each(function() {
         var val = $(this).val().trim();
         var row = $(this).closest('.col-mapping-row');
         var isChecked = row.find('input[type=\"checkbox\"]').is(':checked');
         
         if (!isChecked) {
            row.css('background-color', 'rgba(220, 53, 69, 0.15)');
         } else {
            var origName = row.find('strong').text().trim();
            var displayCol = (val !== '') ? val : origName;
            selectedCols.push(displayCol);
            
            if (required.includes(val)) {
               row.css('background-color', 'rgba(40, 167, 69, 0.2)');
               foundReq.push(val);
            } else if (optional.includes(val)) {
               row.css('background-color', 'rgba(255, 193, 7, 0.2)');
               foundOpt.push(val);
            }
         }
      });
      
      required.forEach(function(col) {
         var badge = $('#req-badge-' + col);
         if(foundReq.includes(col)) {
            badge.css('background-color', '#28a745');
         } else {
            badge.css('background-color', '#dc3545');
         }
      });
      
      optional.forEach(function(col) {
         var badge = $('#opt-badge-' + col);
         if(foundOpt.includes(col)) {
            badge.css({'background-color': '#ffc107', 'color': '#212529'});
         } else {
            badge.css({'background-color': '#6c757d', 'color': 'white'});
         }
      });
      
      var selContainer = $('#selected-cols-container');
      selContainer.empty();
      selectedCols.forEach(function(colName) {
         // Added clickable class for the selected badges
         selContainer.append('<span class=\"clickable-selected-badge\" style=\"display: inline-block; padding: 4px 8px; margin: 2px; border-radius: 12px; font-size: 12px; font-weight: bold; background-color: #17a2b8; color: white; cursor: pointer; user-select: none; transition: transform 0.2s;\">' + colName + '</span>');
      });
      
      var allRequiredFound = required.every(function(reqCol) { return foundReq.includes(reqCol); });
      var nextBtn = $('#next_row_merge');
      
      if (allRequiredFound) {
         nextBtn.prop('disabled', false).removeClass('disabled').css({ 'pointer-events': 'auto', 'opacity': '1' });
      } else {
         nextBtn.prop('disabled', true).addClass('disabled').css({ 'pointer-events': 'none', 'opacity': '0.5' });
      }
    }
    
    var checkExist = setInterval(function() {
       if ($('.col-mapping-row').length) {
          clearInterval(checkExist); 
          updateRequiredColumns();   
          
          $(document).off('input change', '.col-mapping-row input').on('input change', '.col-mapping-row input', updateRequiredColumns);
          
          // --- CLICK LOGIC: Required & Optional Badges ---
          $(document).off('click', '.clickable-badge').on('click', '.clickable-badge', function() {
             var badgeText = $(this).text().trim();
             var targetRow = null;
             
             // Find the corresponding row
             $('.col-mapping-row').each(function() {
                var val = $(this).find('input[type=\"text\"][id^=\"map_name_\"]').val().trim();
                var origName = $(this).find('strong').text().trim();
                var displayCol = (val !== '') ? val : origName;
                
                if (displayCol === badgeText || val === badgeText) {
                   targetRow = $(this);
                   return false; // Break loop on first match
                }
             });
             
             // Toggle the checkbox
             if (targetRow) {
                var checkbox = targetRow.find('input[type=\"checkbox\"]');
                checkbox.prop('checked', !checkbox.prop('checked')).trigger('change');
             }
          });
          
          // --- CLICK LOGIC: Selected Badges ---
          $(document).off('click', '.clickable-selected-badge').on('click', '.clickable-selected-badge', function() {
             var badgeText = $(this).text().trim();
             var targetRow = null;
             
             $('.col-mapping-row').each(function() {
                var val = $(this).find('input[type=\"text\"][id^=\"map_name_\"]').val().trim();
                var origName = $(this).find('strong').text().trim();
                var displayCol = (val !== '') ? val : origName;
                var isChecked = $(this).find('input[type=\"checkbox\"]').is(':checked');
                
                if (displayCol === badgeText && isChecked) {
                   targetRow = $(this);
                   return false; 
                }
             });
             
             // Turn off the checkbox
             if (targetRow) {
                var checkbox = targetRow.find('input[type=\"checkbox\"]');
                checkbox.prop('checked', false).trigger('change');
             }
          });
       }
    }, 100); 
    ")))
    
    tagList(
      header_tags,
      tags$div(style = "max-height: 400px; overflow-y: auto; overflow-x: hidden;", rows_tags),
      js_script
    )
  })
  
  observeEvent(input$toggle_extended, {
    rv$extended_controls <- !isTRUE(rv$extended_controls)
    
    if (rv$extended_controls) {
      # --- TURNING ON EXTENDED MODE ---
      
      # instantly unhide the panel client-side
      shinyjs::show("extended_controls_container") 
      
      updateActionButton(session, "toggle_extended", 
                         label = "Hide Lookup Controls", 
                         icon = icon("lock"))
      shinyjs::removeClass("toggle_extended", "btn-secondary")
      shinyjs::addClass("toggle_extended", "btn-warning") 
      
      shinyjs::removeClass("submit_button", "btn-primary") 
      shinyjs::removeClass("submit_button", "btn-success")
      shinyjs::addClass("submit_button", "btn-warning")
      
    } else {
      # --- TURNING OFF EXTENDED MODE ---
      
      # instantly hide the panel client-side
      shinyjs::hide("extended_controls_container")
      
      updateActionButton(session, "toggle_extended", 
                         label = "Show Lookup Controls", 
                         icon = icon("magnifying-glass"))
      shinyjs::removeClass("toggle_extended", "btn-warning")
      shinyjs::addClass("toggle_extended", "btn-secondary")
      
      shinyjs::removeClass("submit_button", "btn-warning")
      shinyjs::addClass("submit_button", "btn-primary") 
    }
  })
  
  observeEvent(input$ext_match, {
    rv$ext_match <- input$ext_match 
    if (isTRUE(input$ext_match)) {
      shinyjs::disable("ignore_case")
      updateCheckboxInput(session, "ignore_case", value = TRUE)
    } else {
      updateCheckboxInput(session, "ignore_case", value = rv$ignore_case)
      shinyjs::enable("ignore_case")
    }
    print(paste("input$ext_match toggle:", rv$ext_match))
  })
  
  observeEvent(input$ignore_case, {
    rv$ignore_case <- input$ignore_case 
    updateCheckboxInput(session, "ignore_case", value = rv$ignore_case)
  })
  
  
  # API Keys Settings Button: Build UI safely
  observeEvent(input$keys_btn, {
    
    # --- RESOLUTION LAYER ---
    # Helper function to safely read from VFS if the file exists
    get_vfs_key <- function(filename) {
      path <- file.path("keys", filename)
      if (fs::file_exists(path)) {
        tryCatch({
          dec <- sodium::data_decrypt(readRDS(path), key=sha256(glens_env$privkey_dec))
          return(trimws(rawToChar(dec)))
        }, error = function(e) return(NULL))
      }
      return(NULL)
    }
    
    # Resolve keys: Try VFS first. If NULL, fallback to the browser-stored glens_env
    val_scopus    <- if(!is.null(get_vfs_key("scopus.key"))) get_vfs_key("scopus.key") else glens_env$scopus_key
    val_wos       <- if(!is.null(get_vfs_key("wos.key"))) get_vfs_key("wos.key") else glens_env$wos_key
    val_semantic  <- if(!is.null(get_vfs_key("semantic.key"))) get_vfs_key("semantic.key") else glens_env$semantic_key
    val_crossref  <- if(!is.null(get_vfs_key("crossref.key"))) get_vfs_key("crossref.key") else glens_env$crossref_key
    val_opencites <- if(!is.null(get_vfs_key("opencites.key"))) get_vfs_key("opencites.key") else glens_env$opencites_key
    
    # Evaluate boolean flags for the UI Badges (TRUE if we found a key anywhere)
    rv$has_key_scopus    <- !is.null(val_scopus) && val_scopus != ""
    rv$has_key_wos       <- !is.null(val_wos) && val_wos != ""
    rv$has_key_semantic  <- !is.null(val_semantic) && val_semantic != ""
    rv$has_key_crossref  <- !is.null(val_crossref) && val_crossref != ""
    rv$has_key_opencites <- !is.null(val_opencites) && val_opencites != ""
    
    # --- BUILD MODAL ---
    showModal(modalDialog(
      title = tags$span(icon("gears", lib = "font-awesome"), " API Configuration Settings"),
      size = "m",
      
      # Scopus
      tags$div(class = "api-row",
               tags$div(class = "input-button-group",
                        passwordInput("scopus_key", 
                                      label = HTML(paste0('
             <div style="display: flex; align-items: center;">
               Scopus API Key :
               <span class="api-help-container" style="position: relative; display: inline-block;">
                 <span class="help-icon" style="cursor: pointer; margin-left: 5px; color: #17a2b8; font-size: 16px;">&#9432;</span>
                 <div class="api-help-content" style="display: none; position: absolute; bottom: 130%; left: 50%; transform: translateX(-50%); width: 220px; background: #ffffff; padding: 12px; border: 1px solid #ccc; border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); z-index: 9999; font-weight: normal; font-size: 13px; text-align: left;">
                   Need a key? Register at the <br>
                   <a href="https://dev.elsevier.com/" target="_blank" style="text-decoration: underline; color: #007bff; font-weight: bold;">Elsevier Developer Portal</a>.
                 </div>
               </span>
               
               ', if(rv$has_key_scopus) {
                 '<span class="status-badge badge-found" style="margin-left: auto; font-weight: normal; font-size: 12px;"><i class="fa fa-check"></i> Key Found</span>'
               } else {
                 '<span class="status-badge badge-missing" style="margin-left: auto; font-weight: normal; font-size: 12px; color: #dc3545;">Missing</span>'
               }, 
               '</div>'
                                      )), 
               placeholder = "Enter Scopus Key", 
               width = "100%"
                        ),
               tags$div(class = "api-save-wrap",
                        actionButton("save_scopus", "Save Scopus Key", class = "btn-success save-btn-custom")
               )
               )
      ),
      
      # Web of Science
      tags$div(class = "api-row",
               tags$div(class = "input-button-group",
                        passwordInput("wos_key",
                                      label = HTML(paste0('
             <div style="display: flex; align-items: center;">
                          Web of Science API Key :
                          <span class="api-help-container" style="position: relative; display: inline-block;">
                            <span class="help-icon" style="cursor: pointer; margin-left: 5px; color: #17a2b8; font-size: 16px;">&#9432;</span>
                            <div class="api-help-content" style="display: none; position: absolute; bottom: 130%; left: 50%; transform: translateX(-50%); width: 220px; background: #ffffff; padding: 12px; border: 1px solid #ccc; border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); z-index: 9999; font-weight: normal; font-size: 13px; text-align: left;">
                              Need a key? Register at the <br>
                              <a href="https://developer.clarivate.com/apis" target="_blank" style="text-decoration: underline; color: #007bff; font-weight: bold;">Clarivate Developer Portal</a>.
                            </div>
                          </span>
                        ', if(rv$has_key_wos) {
                          '<span class="status-badge badge-found" style="margin-left: auto; font-weight: normal; font-size: 12px;"><i class="fa fa-check"></i> Key Found</span>'
                        } else {
                          '<span class="status-badge badge-missing" style="margin-left: auto; font-weight: normal; font-size: 12px; color: #dc3545;">Missing</span>'
                        }, 
                        '</div>'
                                      )), placeholder = "Enter Web of Science Key", width = "100%"),
                        tags$div(class = "api-save-wrap",
                                 actionButton("save_wos", "Save Web of Science Key", class = "btn-success save-btn-custom")
                        )
               )
      ),
      
      # Semantic Scholar
      tags$div(class = "api-row",
               tags$div(class = "input-button-group",
                        passwordInput("semantic_key",
                                      label = HTML(paste0('
             <div style="display: flex; align-items: center;">
                          Semantic Scholar API Key :
                          <span class="api-help-container" style="position: relative; display: inline-block;">
                            <span class="help-icon" style="cursor: pointer; margin-left: 5px; color: #17a2b8; font-size: 16px;">&#9432;</span>
                            <div class="api-help-content" style="display: none; position: absolute; bottom: 130%; left: 50%; transform: translateX(-50%); width: 220px; background: #ffffff; padding: 12px; border: 1px solid #ccc; border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); z-index: 9999; font-weight: normal; font-size: 13px; text-align: left;">
                              Need a key? Request one from the <br>
                              <a href="https://www.semanticscholar.org/product/api#api-key" target="_blank" style="text-decoration: underline; color: #007bff; font-weight: bold;">Semantic Scholar API Form</a>.
                            </div>
                          </span>
                        ', if(rv$has_key_semantic) {
                          '<span class="status-badge badge-found" style="margin-left: auto; font-weight: normal; font-size: 12px;"><i class="fa fa-check"></i> Key Found</span>'
                        } else {
                          '<span class="status-badge badge-missing" style="margin-left: auto; font-weight: normal; font-size: 12px; color: #dc3545;">Missing</span>'
                        }, 
                        '</div>'
                                      )), placeholder = "Enter Semantic Scholar Key", width = "100%"),
                        tags$div(class = "api-save-wrap",
                                 actionButton("save_semantic", "Save Semantic Scholar Key", class = "btn-success save-btn-custom")
                        )
               )
      ),
      
      # Crossref
      tags$div(class = "api-row",
               tags$div(class = "input-button-group",
                        passwordInput("crossref_key", 
                                      label = HTML(paste0('
             <div style="display: flex; align-items: center;">
               Crossref API Key :
               <span class="api-help-container" style="position: relative; display: inline-block;">
                 <span class="help-icon" style="cursor: pointer; margin-left: 5px; color: #17a2b8; font-size: 16px;">&#9432;</span>
                 <div class="api-help-content" style="display: none; position: absolute; bottom: 130%; left: 50%; transform: translateX(-50%); width: 220px; background: #ffffff; padding: 12px; border: 1px solid #ccc; border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); z-index: 9999; font-weight: normal; font-size: 13px; text-align: left;">
                   Need a key? Register at <br>
                   <a href="https://manage.crossref.org/keys" target="_blank" style="text-decoration: underline; color: #007bff; font-weight: bold;">Crossref Key Manager</a>.
                 </div>
               </span>
               
               ', if(rv$has_key_crossref) {
                 '<span class="status-badge badge-found" style="margin-left: auto; font-weight: normal; font-size: 12px;"><i class="fa fa-check"></i> Key Found</span>'
               } else {
                 '<span class="status-badge badge-missing" style="margin-left: auto; font-weight: normal; font-size: 12px; color: #dc3545;">Missing</span>'
               }, 
               '</div>'
                                      )), 
               placeholder = "Enter Crossref Key", 
               width = "100%"
                        ),
               tags$div(class = "api-save-wrap",
                        actionButton("save_crossref", "Save Crossref Key", class = "btn-success save-btn-custom")
               )
               )
      ),
      
      # OpenCitations
      tags$div(class = "api-row",
               tags$div(class = "input-button-group",
                        passwordInput("opencites_key", 
                                      label = HTML(paste0('
             <div style="display: flex; align-items: center;">
               OpenCitations API Key :
               <span class="api-help-container" style="position: relative; display: inline-block;">
                 <span class="help-icon" style="cursor: pointer; margin-left: 5px; color: #17a2b8; font-size: 16px;">&#9432;</span>
                 <div class="api-help-content" style="display: none; position: absolute; bottom: 130%; left: 50%; transform: translateX(-50%); width: 220px; background: #ffffff; padding: 12px; border: 1px solid #ccc; border-radius: 6px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); z-index: 9999; font-weight: normal; font-size: 13px; text-align: left;">
                   Need a key? Register at <br>
                   <a href="https://opencitations.net/accesstoken/" target="_blank" style="text-decoration: underline; color: #007bff; font-weight: bold;">OpenCitations Access Token</a>.
                 </div>
               </span>
               
               ', if(rv$has_key_opencites) {
                 '<span class="status-badge badge-found" style="margin-left: auto; font-weight: normal; font-size: 12px;"><i class="fa fa-check"></i> Key Found</span>'
               } else {
                 '<span class="status-badge badge-missing" style="margin-left: auto; font-weight: normal; font-size: 12px; color: #dc3545;">Missing</span>'
               }, 
               '</div>'
                                      )), 
               placeholder = "Enter OpenCites Token", 
               width = "100%"
                        ),
               tags$div(class = "api-save-wrap",
                        actionButton("save_opencites", "Save OpenCitations Token", class = "btn-success save-btn-custom")
               )
               )
      ),
      
      footer = modalButton("Close API Key Settings"),
      easyClose = TRUE
    ))
    
    # --- FILL TEXT INPUTS ---
    # Update the input boxes safely using the unified values
    if(rv$has_key_scopus) updateTextInput(session, "scopus_key", value = val_scopus)
    if(rv$has_key_wos) updateTextInput(session, "wos_key", value = val_wos)
    if(rv$has_key_semantic) updateTextInput(session, "semantic_key", value = val_semantic)
    if(rv$has_key_crossref) updateTextInput(session, "crossref_key", value = val_crossref)
    if(rv$has_key_opencites) updateTextInput(session, "opencites_key", value = val_opencites)
  })
  
  # Save handlers
  observeEvent(input$save_scopus, {
    # req(input$scopus_key)
    raw_key <- charToRaw(trimws(input$scopus_key))
    # print(input$scopus_key)
    # print(raw_key)
    # print(glens_env$privkey_dec)
    # print(sha256(glens_env$privkey_dec))
    encrypted_scopus <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_scopus, file = file.path("keys","scopus.key"))
    if(is_WASM){
      session$sendCustomMessage("save_key_to_browser", list(platform = "scopus_key", key = raw_key))
    }
    if(is.null(input$scopus_key) || stringi::stri_isempty(input$scopus_key)){
      if(fs::file_exists(file.path("keys","scopus.key")))
        fs::file_delete(file.path("keys","scopus.key"))
      # removeModal()
      # return()
    }else{
      showNotification("Scopus Key Encrypted and Saved.", type = "message")
    }
    # removeModal()
  })
  observeEvent(input$save_wos, {
    # req(input$wos_key)
    if(is.null(input$wos_key) || stringi::stri_isempty(input$wos_key)){
      if(fs::file_exists(file.path("keys","wos.key")))
        fs::file_delete(file.path("keys","wos.key"))
      # removeModal()
      return()
    }
    raw_key <- charToRaw(trimws(input$wos_key))
    encrypted_wos <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_wos, file = file.path("keys","wos.key"))
    if(is_WASM){
      session$sendCustomMessage("save_key_to_browser", list(platform = "wos_key", key = raw_key))
    }
    showNotification("Web of Science Key Encrypted and Saved.", type = "message")
    # removeModal()
  })
  observeEvent(input$save_semantic, {
    # fs::dir_create("keys")
    # req(input$semantic_key)
    if(is.null(input$semantic_key) || stringi::stri_isempty(input$semantic_key)){
      if(fs::file_exists(file.path("keys","semantic.key")))
        fs::file_delete(file.path("keys","semantic.key"))
      # removeModal()
      return()
    }
    raw_key <- charToRaw(trimws(input$semantic_key))
    encrypted_semantic <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_semantic, file = file.path("keys","semantic.key"))
    if(is_WASM){
      session$sendCustomMessage("save_key_to_browser", list(platform = "semantic_key", key = raw_key))
    }
    showNotification("Semantic Scholar Key Encrypted and Saved.", type = "message")
    # removeModal()
  })
  observeEvent(input$save_crossref, {
    # fs::dir_create("keys")
    # req(input$semantic_key)
    if(is.null(input$crossref_key) || stringi::stri_isempty(input$crossref_key)){
      if(fs::file_exists(file.path("keys","crossref.key")))
        fs::file_delete(file.path("keys","crossref.key"))
      # removeModal()
      return()
    }
    raw_key <- charToRaw(trimws(input$crossref_key))
    encrypted_crossref <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_crossref, file = file.path("keys","crossref.key"))
    if(is_WASM){
      session$sendCustomMessage("save_key_to_browser", list(platform = "crossref_key", key = raw_key))
    }
    showNotification("Crossref Key Encrypted and Saved.", type = "message")
    # removeModal()
  })
  observeEvent(input$save_opencites, {
    # fs::dir_create("keys")
    # req(input$semantic_key)
    if(is.null(input$opencites_key) || stringi::stri_isempty(input$opencites_key)){
      if(fs::file_exists(file.path("keys","opencites.key")))
        fs::file_delete(file.path("keys","opencites.key"))
      # removeModal()
      return()
    }
    raw_key <- charToRaw(trimws(input$opencites_key))
    encrypted_opencites <- sodium::data_encrypt(raw_key, key=sha256(glens_env$privkey_dec))
    saveRDS(encrypted_opencites, file = file.path("keys","opencites.key"))
    if(is_WASM){
      session$sendCustomMessage("save_key_to_browser", list(platform = "opencites_key", key = raw_key))
    }
    showNotification("OpenCitations Key Encrypted and Saved.", type = "message")
    # removeModal()
  })
  
  observeEvent(input$cancel_button,{
      message("Cancel signal received.")
      if(fs::file_exists(file.path("run.lock"))){
        fs::file_delete(file.path("run.lock"))
      }
      rv$is_cancelled <- TRUE
      # Immediately hide the overlay and re-enable the UI
      # shinyjs::hide("sh_index")
      # shinyjs::hide("summary_table")
      shinyjs::hide("acounts_plot")
      shinyjs::hide("ccounts_plot")
      shinyjs::hide("cdist_plot")
      shinyjs::hide("aperc_plot")
      shinyjs::hide("cperc_plot")
      shinyjs::hide("network_filtered")
      # shinyjs::hide("network_full")
      # shinyjs::hide("extended_table")
      shinyjs::hide(id="year_slider")
      shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
      shinyjs::enable(id = "submit_button")
      # Update logs
      rv$log_text <- paste(rv$log_text, paste0("Process cancelled by user.\n"),sep="<br>")
      
      removeModal()
  })
  
  # observeEvent(rv$glens_year_filtered, {
  #   filters <- debounced_inputs()
  #   # print(paste("length(filters$authors):",length(filters$authors)))
  #   
  #   if("SCOPUS_ID" %in% colnames(rv$glens_year_filtered)){
  #     # req("SCOPUS_ID" %in% colnames(rv$glens_year_filtered))
  #     # 1. Safely extract the column (handles NULL if the table isn't ready)
  #     scopus_col <- rv$glens_year_filtered$SCOPUS_ID
  #     
  #     if (is.null(scopus_col) || length(scopus_col) == 0) {
  #       # Safe fallback if data isn't loaded yet
  #       available_scoupusids <- character(0) 
  #       
  #     } else {
  #       # 2. Split by comma, semicolon, or literal double-quote
  #       raw_splits <- unlist(strsplit(as.character(scopus_col), split = "[,;\"]", perl = TRUE))
  #       
  #       # 3. Trim whitespace
  #       trimmed_splits <- trimws(raw_splits)
  #       
  #       # 4. Remove empty strings and get unique values directly
  #       available_scoupusids <- unique(trimmed_splits[trimmed_splits != ""])
  #     }
  #     # print(available_scoupusids)
  #     req(length(na.omit(available_scoupusids)) > 0)
  #     if (isTRUE(input$autofill_scopusid_input)) {
  #       updateTextAreaInput(session, "scopusid_text", value=paste(available_scoupusids, collapse="\n"))
  #     } else {
  #       updateTextAreaInput(session, "scopusid_text", value=NULL)
  #     }
  #   }
  #   print(paste("length(filters$authors):",length(filters$authors)))
  #   print(paste("colnames(rv$glens_year_filtered):",paste(colnames(rv$glens_year_filtered),collapse=",")))
  #   req(length(filters$authors) > 0)
  #   
  #   req(all(c("First_Author","Second_Author","Co_Author","Corresponding_Author", "Adjusted_Citations", "Qscore") %in% colnames(rv$glens_year_filtered)))
  #   
  #   rv$glens_year_filtered <- extend_input_table(rv, rv$glens_year_filtered)
  #   compute_indices(rv, rv$glens_year_filtered)
  #   plot_glens_table(rv, session)
  # })
  # # #source selection, slider, author_list ,Slider Events
  # # observeEvent(c(rv$glens_etable_final, input$selected_source, input$year_slider, input$author_list, input$author_logic_gate), {
  # # 3. Execute the logic when the debounced inputs finally settle
  # observeEvent(debounced_inputs(), {
  # # observe({
  #     filters <- debounced_inputs()
  #     req(filters$source,filters$year)
  #     req(rv$glens_etable_final, input$selected_source, input$year_slider) #input$author_list
  #     # message(paste("(post)nrow(rv$glens_etable_final):",nrow(rv$glens_etable_final)))
  #     # message(paste("(post)colnames(rv$glens_etable_final):",colnames(rv$glens_etable_final)))
  #     # req("Source" %in% names(rv$glens_etable_final))
  #     # req("Qscore" %in% names(rv$glens_etable_final))
  #   
  #     if(isTRUE(is.null(filters$logic_gate))){
  #       author_logic_gate <- "OR"
  #     }else{
  #       author_logic_gate <- filters$logic_gate
  #     }
  #     if(nrow(rv$glens_etable_final)<=0){
  #       return()
  #     }
  #     if(is.na(filters$year[1]) || is.na(filters$year[2])){
  #       return()
  #     }
  #     if(rv$is_glens_exec){
  #       warning("ColabNET is Executing...")
  #       return()
  #     }
  #     # shinyjs::disable(id="year_slider")
  #   
  #     # print("Changed range...")
  #     # output$log <- renderText("Changed range...")
  #     # extend_input_table(rv)
  #     # rv$glens_year_filtered <- rv$glens_etable_final %>%
  #     #   filter(Year >= input$year_slider[1]) %>%
  #     #   filter(Year <= input$year_slider[2])
  #     #   # filter(dplyr::between(
  #     #   #   Year,
  #     #   #   input$year_slider[1],
  #     #   #   input$year_slider[2]
  #     #   # ))
  #     
  #     yeardata_tmp <- data.frame()
  #     if("Year" %in% colnames(rv$glens_etable_final) && "Source" %in% colnames(rv$glens_etable_final)){
  #         year_levels <- levels(factor(rv$glens_etable_final[["Year"]]))
  #         if(length(year_levels) > 1){
  #           yeardata_tmp <- rv$glens_etable_final %>%
  #             filter(
  #               Year >= filters$year[1],
  #               Year <= filters$year[2],
  #               Source == filters$source # The new source-based filter logic
  #             )
  #           
  #           # print("rv$glens_etable_final===>")
  #           # print(rv$glens_etable_final %>%
  #           #         filter(Year >= input$year_slider[1],
  #           #                Year <= input$year_slider[2]))
  #           if(nrow(yeardata_tmp) <= 0){
  #             # output$log <- renderText({paste("input$year_slider - Warning: No data found for this year range.")})
  #             rv$log_text <- paste(rv$log_text, paste("input$year_slider - Warning: No data found for this year range."), sep="<br>")
  #             warning("input$year_slider - Warning: No data found for this year range.")
  #             # shinyjs::hide("sh_index")
  #             shinyjs::hide("summary_table")
  #             shinyjs::hide("acounts_plot")
  #             shinyjs::hide("ccounts_plot")
  #             shinyjs::hide("cdist_plot")
  #             shinyjs::hide("aperc_plot")
  #             shinyjs::hide("cperc_plot")
  #             shinyjs::hide("network_filtered")
  #             # shinyjs::hide("extended_table")
  #             shinyjs::enable(id="year_slider")
  #             return()
  #           }
  #           
  #           rv$log_text <- paste(rv$log_text,paste("(Slider:", input$year_slider[1], "-", input$year_slider[2],")","Filtered years to range...", min(yeardata_tmp$Year), "and",max(yeardata_tmp$Year)
  #           ),paste("Source:", input$selected_source), sep="<br>")
  #         }else if(length(year_levels) == 1){
  #           yeardata_tmp <- rv$glens_etable_final %>%
  #             filter(
  #               Year >= year_levels,
  #               Year <= year_levels,
  #               Source == filters$source 
  #             )
  #         }else{
  #           yeardata_tmp <- rv$glens_etable_final  
  #         }
  #     }else{
  #       rv$log_text <- paste(rv$log_text,"'Year' & 'Source' columns are missing. Skipping filters", sep="<br>")  
  #       yeardata_tmp <- rv$glens_etable_final  
  #       }
  #     # output$log <- renderText({ rv$log_text })
  #     # print(paste("(Slider:", input$year_slider[1], "-", input$year_slider[2],")","Filtered years to range...", min(rv$glens_year_filtered$Year), "and",max(rv$glens_year_filtered$Year)))
  #     # print(str(rv$glens_year_filtered$Year))
  #     #Fetch author info only when auto_refresh_lookup is enabled
  #     yeardata_tmp[["matched_token"]] <- NULL  
  #   req(input$auto_refresh_lookup)
  #     # if (isTRUE(input$auto_refresh_lookup)) {   
  #     # raw_text <- filters$authors
  #     print(paste("HERE2:RT:", filters$authors))
  #     # Only apply the logic gate if the user has actually typed something
  #     if (!is.null(filters$authors) && length(filters$authors) > 0){ #&& trimws(raw_text) != "") {
  #       author_list <- filters$authors #unlist(strsplit(filters$authors, "[\n,]"))
  #       author_list <- stringr::str_squish(author_list)
  #       author_list <- stringr::str_to_title(author_list)
  #       author_list <- author_list[author_list != ""]
  #       
  #       rv$author_list <- unique(author_list)
  #       # Apply the logic gate function we built earlier
  #       if (length(author_list) > 0) {
  #         
  #         # 1. Grab current toggle states (fallback to TRUE if NULL)
  #         ext_match_flag <- if (!is.null(input$ext_match)) input$ext_match else TRUE
  #         ignore_case_flag <- if (!is.null(input$ignore_case)) input$ignore_case else TRUE
  #         
  #         # 2. Identify which columns the user selected in the Lookup Controls
  #         search_cols <- names(rv$detected_mv_cols)
  #         if (is.null(search_cols) || length(search_cols) == 0) search_cols <- "Authors"
  #         valid_search_cols <- intersect(search_cols, colnames(yeardata_tmp))
  #         if (length(valid_search_cols) == 0) valid_search_cols <- "Authors"
  #         
  #         # 3. Apply the logic filter
  #         filtered_df <- apply_author_logic(
  #           pubs_df          = yeardata_tmp,
  #           selected_authors = author_list, 
  #           gate             = author_logic_gate,
  #           ext_match        = ext_match_flag,
  #           ignore_case      = ignore_case_flag,
  #           search_cols      = valid_search_cols
  #         )
  #         print(paste("colnames(filtered_df):", paste(colnames(filtered_df), collapse=",")))
  #         # 4. Save the newly filtered data to your reactive variable
  #         yeardata_tmp <- filtered_df
  #         
  #         shinyjs::show("summary_table")
  #         shinyjs::show("acounts_plot")
  #         shinyjs::show("ccounts_plot")
  #         shinyjs::show("cdist_plot")
  #         shinyjs::show("aperc_plot")
  #         shinyjs::show("cperc_plot")
  #         shinyjs::show("network_filtered")
  #       }
  #     } else{
  #       # hide plots because no keywords were given
  #       # shinyjs::hide("sh_index")
  #       # shinyjs::hide("summary_table")
  #       print(paste("HERE2.1!!!!:", length(filters$authors)))
  #       shinyjs::hide("acounts_plot")
  #       shinyjs::hide("ccounts_plot")
  #       shinyjs::hide("cdist_plot")
  #       shinyjs::hide("aperc_plot")
  #       shinyjs::hide("cperc_plot")
  #       shinyjs::hide("network_filtered")
  #       shinyjs::enable(id="year_slider")
  #       return()
  #     }
  #     # }
  #     
  #     # output$extended_table <- renderTable(rv$glens_year_filtered, striped = TRUE)
  #     # output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
  #     # output$extended_table <- DT::renderDataTable({
  #     #   datatable(
  #     #     rv$glens_year_filtered,
  #     #     options = list(
  #     #       scrollY = "600px",
  #     #       scrollX = TRUE,
  #     #       paging = TRUE
  #     #     )
  #     #   )
  #     # })
  #     
  #     # output$extended_table <- DT::renderDataTable({
  #     #   datatable(
  #     #     rv$glens_year_filtered,
  #     #     extensions = 'Buttons', # 1. Load the extension
  #     #     options = list(
  #     #       scrollY = "600px",
  #     #       scrollX = TRUE,
  #     #       paging = TRUE,
  #     #       dom = 'Blfrtip',       # 2. Add 'B' to the layout (B = Buttons)
  #     #       lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
  #     #       buttons = c('copy', 'csv', 'excel', 'pdf', 'print') # 3. Define buttons
  #     #     )
  #     #   )
  #     # })
  #     
  #     req(rv$author_match_regex)
  #     print("observeEvent(): rv$author_match_regex:")
  #     print(str(rv$author_match_regex))
  #     rv$glens_year_filtered <- extend_input_table(rv, yeardata_tmp)
  #     # rv$glens_year_filtered <- rv$glens_etable_final
  #     if (nrow(rv$glens_year_filtered) <= 0) {
  #       rv$log_text <- paste(rv$log_text, "No keywords were matched.",sep="<br>")
  #       # shinyjs::enable("submit_button")
  #       # shinyjs::hide("progress_overlay")
  #       # # req(nrow(rv$glens_year_filtered) > 0)
  #     } else {
  #       compute_indices(rv, yeardata_tmp)
  #       
  #       rv$glens_year_filtered <- yeardata_tmp
  #       # shinyjs::show("extended_table")
  #     }
  #     
  #     # plot_glens_table(rv, session)
  #     # plot_glens_table(rv, output, session)
  #     # plot_glens_table()
  #     
  #     shinyjs::enable(id="year_slider")
  # 
  # })
  
  
  # ==============================================================================
  # AUTOMATIC DATA PIPELINE (Replaces manual observers)
  # ==============================================================================
  
  # # 1. Base Extended Table (Runs ONCE when new data is imported via submit/merge)
  # glens_extended_rx <- reactive({
  #   req(rv$glens_input_table, rv$author_match_regex)
  #   print("reactive():glens_extended_rx")
  #   return(extend_input_table(rv, rv$glens_input_table, rv$author_match_regex, rv$target_variants_norm))
  # })
  # 
  # # 2. MATCH JOURNALS (Runs ONCE on the full extended table)
  # # This becomes your "glens_full_table" equivalent.
  # glens_full_table_rx <- reactive({
  #   req(glens_extended_rx())
  #   print("reactive():glens_full_table_rx")
  #   return(match_journals(rv, glens_extended_rx()))
  # })
  # 
  # # ---------------------------------------------------------
  # # MANAGER OBSERVER: Sets up the UI when data is ready
  # # ---------------------------------------------------------
  # observeEvent(glens_full_table_rx(), {
  #   df <- glens_full_table_rx()
  #   req(nrow(df) > 0)
  #   
  #   # 1. Register the skeletons (This queues the JS creation)
  #   render_skeleton_plots(rv, df, output)
  #   
  #   # 2. Calculate years
  #   years <- as.numeric(df$Year)
  #   min_yr <- min(years, na.rm = TRUE)
  #   max_yr <- max(years, na.rm = TRUE)
  #   
  #   # 3. SHOW the UI elements FIRST
  #   shinyjs::show("year_slider")
  #   shinyjs::show("sh_index")
  #   shinyjs::show("summary_table")
  #   shinyjs::show("lookup_controls_panel")
  #   
  #   # 4. Update the slider
  #   updateSliderInput(session, "year_slider",
  #                     min = min_yr,
  #                     max = max_yr,
  #                     value = c(min_yr, max_yr))
  #   
  #   rv$log_text <- paste(rv$log_text, "Journal matching complete. UI updated.", sep="<br>")
  # })
  
  # ---------------------------------------------------------
  # PLOT PROXY OBSERVER: Pushes data only after UI exists
  # ---------------------------------------------------------
  
  raw_lookup_inputs <- reactive({
    req(rv$glens_full_table)
    df <- rv$glens_full_table
    
    cols <- names(df)
    
    # # Use hex strings here too!
    # dynamic_chks <- lapply(cols, function(c) {
    #   hex_str <- paste(as.character(charToRaw(c)), collapse = "")
    #   input[[paste0("lookup_chk_", hex_str)]]
    # })
    # 
    # dynamic_delims <- lapply(cols, function(c) {
    #   hex_str <- paste(as.character(charToRaw(c)), collapse = "")
    #   input[[paste0("lookup_delim_", hex_str)]]
    # })
    # 
    # dynamic_keywords <- lapply(cols, function(c) {
    #   hex_str <- paste(as.character(charToRaw(c)), collapse = "")
    #   input[[paste0("lookup_text_", hex_str)]]
    # })
    
    target_variants <- ""
    if(length(input$author_list) > 0){
      target_variants <- stringi::stri_omit_empty(stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n"))))
      target_variants <- target_variants[target_variants != ""]
      # isolate({ rv$author_list <- target_variants })
      
      # if(length(target_variants) > 0){
      #   # FIX 1: Isolate the updates to rv so they don't trigger upstream dependencies
      #   isolate({
      #     rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
      #       vn <- normalize_name(v)
      #       list(norm = vn, parts = extract_parts(vn))
      #     })
      #     rv$author_match_regex <- build_name_regex_for_variants(target_variants)
      #   })
      # }
    }
    
    list(
      source = input$selected_source,
      year = input$year_slider,
      authors = target_variants,
      logic_gate = input$author_logic_gate,
      # chks = dynamic_chks,
      # delims = dynamic_delims,
      # keywords = dynamic_keywords,
      dataset_trigger = nrow(df),
      ext_match = input$ext_match,     
      ignore_case = input$ignore_case  
    )
  })
  
  # 2. Add a delay (debounce). 
  debounced_inputs <- raw_lookup_inputs %>% debounce(800)
  
  active_filters <- reactive({
    if (isTRUE(input$auto_refresh_lookup)) {
      # AUTO MODE: React to every change in the debounced inputs
      debounced_inputs()
    } else {
      # MANUAL MODE: Wait for the Submit button to be clicked...
      req(rv$manual_submit)
      # ...then silently grab the current state of the inputs without creating a live dependency
      isolate(debounced_inputs()) 
    }
  })
  
  # 3. Year Filtered Table
  glens_year_filtered_rx <- reactive({
    # req(isTRUE(isTRUE(input$auto_refresh_lookup) || rv$is_glens_exec))
    req(rv$glens_full_table, nrow(rv$glens_full_table) > 0)
    df <- rv$glens_full_table
    # filters <- debounced_inputs()
    
    # INJECT ROUTER: Use the smart active_filters instead of raw debounced_inputs
    filters <- active_filters()
    
    print("HERE1:")
    print(nrow(df))
    print(str(filters))
    # if(isTRUE(rv$is_glens_exec)) return(df) #return(data.frame())
    
    # if(isFALSE(rv$is_glens_exec)){
      # --- A. SOURCE FILTER ---
      if (!is.null(filters$source) && "Source" %in% colnames(df)) {
        df <- df %>% filter(Source == filters$source)
      }
      
      # --- B. YEAR FILTER ---
      if (!is.null(filters$year) && length(filters$year) == 2 && !any(is.na(filters$year))) {
        # Ignore the filter if the slider is in its uninitialized state c(0, 0)
        if (filters$year[1] != 0 || filters$year[2] != 0) {
          df <- df %>%
            mutate(Year = as.numeric(Year)) %>%
            filter(Year >= filters$year[1] & Year <= filters$year[2])
        }
      }
    # }
    
    if (nrow(df) == 0) return(df)
    
    print("filters$authors:")
    print(filters$authors)
    
    # --- C. AUTHOR FILTER ---
    if (!is.null(filters$authors) && length(filters$authors) > 0) {
      author_list <- stringr::str_squish(filters$authors)
      author_list <- stringr::str_to_title(author_list)
      author_list <- author_list[author_list != ""]
      
      # FIX 2: Isolate rv writes inside the reactive
      isolate({ rv$author_list <- unique(author_list) })
      
      isolate({
        rv$target_variants_norm <- lapply(setNames(author_list, author_list), function(v) {
          vn <- normalize_name(v)
          list(norm = vn, parts = extract_parts(vn))
        })
        rv$author_match_regex <- build_name_regex_for_variants(author_list)
      })
      
      if (length(author_list) > 0) {
        gate        <- if(!is.null(filters$logic_gate)) filters$logic_gate else "OR"
        ext_match   <- if(!is.null(filters$ext_match)) filters$ext_match else TRUE
        ignore_case <- if(!is.null(filters$ignore_case)) filters$ignore_case else TRUE
        
        search_cols <- if(!is.null(rv$detected_mv_cols)) intersect(names(rv$detected_mv_cols), colnames(df)) else "Authors"
        if(length(search_cols) == 0) search_cols <- "Authors"
        
        print("rv$detected_mv_cols:")
        print(rv$detected_mv_cols)
        print("search_cols:")
        print(search_cols)
        
        # FIX 3: Isolate the function call so it doesn't accidentally bind to rv reads
        df <- isolate({
          apply_author_logic(
            pubs_df          = df,
            selected_authors = author_list,
            gate             = gate,
            ext_match        = ext_match,
            ignore_case      = ignore_case,
            search_cols      = search_cols
          )
        })
      }
    } else {
      isolate({ 
        rv$author_list <- character(0) 
        rv$target_variants_norm <- NULL
        rv$author_match_regex <- NULL
      })
    }
    
    if (nrow(df) == 0) return(df)
    
    print("author_list:")
    print(rv$author_list)
    print("glens_year_filtered_rx:extend_input_table():")
    # --- D. CALCULATE CITATION WEIGHTS ---
    # FIX 4: Wrap extend_input_table in isolate() to break the infinite loop!
    df <- isolate(extend_input_table(rv, df, rv$author_match_regex, rv$target_variants_norm))
    # if(stringi::stri_isempty(input$author_list)){
    #   df <- df %>% 
    #     select(-any_of(c(
    #       "First_Author", 
    #       "Second_Author", 
    #       "Co_Author", 
    #       "Corresponding_Author", 
    #       "matched_token", 
    #       "label", 
    #       "position_rank"
    #     )))
    # }
    return(df)
  })
  
  # This observer handles all visual side-effects
  observeEvent(glens_year_filtered_rx(), {
    df <- glens_year_filtered_rx()
    print("glens_year_filtered_rx():")
    print(str(df))
    # 1. Handle Visibility
    if (is.null(df) || nrow(df) == 0) {
      shinyjs::hide("summary_table")
      # Hide plot containers
      lapply(c("acounts_plot", "ccounts_plot", "cdist_plot", "aperc_plot", "cperc_plot"), shinyjs::hide)
      return()
    }
    
    if(stringi::stri_isempty(input$author_list)){
      lapply(c("acounts_plot", "ccounts_plot", "cdist_plot", "aperc_plot", "cperc_plot"), shinyjs::hide)
    }else{
      lapply(c("acounts_plot", "ccounts_plot", "cdist_plot", "aperc_plot", "cperc_plot"), shinyjs::show)  
    }
    
    shinyjs::show("summary_table")
    
    # 3. UPDATE THE PLOTS
    # We use try() because if the skeleton isn't fully rendered in the UI yet, 
    # the proxy might throw a temporary error.
    try({
      print("Updating plots via Proxy...")
      plot_glens_table(rv, df, session)
    }, silent = TRUE)
  })
  
  # 4. Indices Computation (Calculates based on what is currently visible/filtered)
  # 1. The Reactive Calculation
  indices_rx <- reactive({
    df <- glens_year_filtered_rx()
    req(df, nrow(df) > 0)
    return(compute_indices(rv, df))
  })
  
  # 5. Extract Scopus IDs safely from the FILTERED data
  available_scopus_ids_rx <- reactive({
    df <- glens_year_filtered_rx()
    req(df, nrow(df) > 0, "SCOPUS_ID" %in% colnames(df))
    scopus_col <- df$SCOPUS_ID
    
    if (is.null(scopus_col) || length(scopus_col) == 0) return(character(0))
    
    raw_splits <- unlist(strsplit(as.character(scopus_col), split = "[,;\"]", perl = TRUE))
    trimmed_splits <- trimws(raw_splits)
    available_scoupusids <- unique(trimmed_splits[trimmed_splits != ""])
    # print(paste("available_scoupusids:",length(available_scoupusids)))
    return(available_scoupusids)
  })
  
  observeEvent(available_scopus_ids_rx(),{
    available_scoupusids <- available_scopus_ids_rx()
    req(length(na.omit(available_scoupusids)) > 0)
    # print(paste("available_scoupusids:",paste(available_scoupusids, collapse=",")))
    # print(input$autofill_scopusid_input)
    if (isTRUE(input$autofill_scopusid_input)) {
      updateTextAreaInput(session, "scopusid_text", value=paste(available_scoupusids, collapse="\n"))
    } else {
      updateTextAreaInput(session, "scopusid_text", value=NULL)
    }
  })
  
  # ==============================================================================
  # DYNAMIC UI UPDATES 
  # ==============================================================================
  observe({
    # Wait for the table to be ready
    df <- rv$glens_full_table
    req(nrow(df) > 0)

    years <- as.numeric(na.omit(df$Year))

    if (length(years) > 0) {
      min_yr <- min(years)
      max_yr <- max(years)

      updateSliderInput(session, "year_slider",
                        min = min_yr,
                        max = max_yr,
                        value = c(min_yr, max_yr))

      shinyjs::show("year_slider")
    }
  })
  
  # --- UI OUTPUTS ---
  output$summary_table <- renderTable({
    idx <- indices_rx()
    # print("HERE2")
    req(idx, idx$summary_table) # Wait for valid data
    # print("HERE2.1")
    shinyjs::show("summary_table")
    return(idx$summary_table)
    }, striped = T)
  
  output$sh_index <- renderUI({
    # Only render if sh_index exists and is not NULL
    idx <- indices_rx()
    # print("HERE1")
    req(idx, idx$sh_index) # Wait for valid data
    # print(paste("HERE1.1", idx$sh_index))
    shinyjs::show("sh_index")
    
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
        idx$sh_index, 
        style = "color: #4B8BBE; font-size: 26px; font-weight: 800; line-height: 1;"
      )
    )
  })
  
  output$extended_table <- DT::renderDT({
    df <- glens_year_filtered_rx() # Or however you pull your dataframe
    
    req(df, nrow(df) > 0)
    # if(is.null(df) || nrow(df) == 0) {
    #   return(DT::datatable(
    #     data.frame(Status = "No data available for current filters"),
    #     options = list(dom = 't') # Just show the table (no buttons/search)
    #   ))
    # }
    
    return(DT::datatable(
        df,
        extensions = 'Buttons', # 1. Load the extension
        options = list(
          scrollY = "600px",
          scrollX = TRUE,
          scrollCollapse = TRUE,
          paging = TRUE,
          dom = 'Blfrtip',       # 2. Add 'B' to the layout (B = Buttons)
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print') # 3. Define buttons
        )
      ))
  }, server = FALSE)
  
  # output$network_full <- renderVisNetwork({
  #   df <- glens_full_table_rx() #glens_extended_rx() #glens_etable_final_rx()
  #   req(nrow(df) > 0)
  #   
  #   net_data <- build_collaboration_network(df, input$author_list) # Or rv$author_list if saved
  #   
  #   visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
  #     visNodes(font = list(size = 14)) %>%
  #     visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
  #     visIgraphLayout(layout = "layout_with_fr") %>%
  #     visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE) %>%
  #     addFontAwesome() 
  # })
  
  # Sync network dropdowns with the selected lookup columns
  ## Render the Filtered Network Dropdown
  # output$ui_net_col_filtered <- renderUI({
  #   req(rv$detected_mv_cols)
  #   
  #   cols <- names(rv$detected_mv_cols)
  #   if (length(cols) == 0) cols <- c("Authors")
  #   
  #   # Isolate prevents circular rendering while maintaining the user's choice
  #   curr_filt <- isolate(input$net_col_filtered)
  #   sel_filt <- if (!is.null(curr_filt) && curr_filt %in% cols) curr_filt else cols[1]
  #   
  #   selectInput("net_col_filtered", NULL, choices = cols, selected = sel_filt, width = "250px")
  # })
  
  # Instantly update network dropdowns when Lookup Controls change
  observeEvent(rv$detected_mv_cols, {
    active_cols <- names(rv$detected_mv_cols)
    if (length(active_cols) == 0) active_cols <- "Authors"
    
    # Update Filtered Network dropdown
    curr_filt <- input$net_col_filtered
    sel_filt <- if (isTruthy(curr_filt) && curr_filt %in% active_cols) curr_filt else active_cols[1]
    updateSelectInput(session, "net_col_filtered", choices = active_cols, selected = sel_filt)
    
    # # Update Full Network dropdown
    # curr_full <- input$net_col_full
    # sel_full <- if (isTruthy(curr_full) && curr_full %in% active_cols) curr_full else active_cols[1]
    # updateSelectInput(session, "net_col_full", choices = active_cols, selected = sel_full)
  }, ignoreNULL = FALSE)
  
  # # Calculate Full Network Data (Debounced)
  # net_data_full_raw <- reactive({
  #   req(input$net_col_full, rv$glens_full_table, nrow(rv$glens_full_table) > 0, input$net_col_full)
  #   
  #   target_col <- input$net_col_full
  #   target_delim <- if (!is.null(rv$detected_mv_cols[[target_col]])) rv$detected_mv_cols[[target_col]] else ","
  #   
  #   build_collaboration_network(rv$glens_full_table, rv$author_list, target_col, target_delim)
  # })
  # net_data_full_debounced <- net_data_full_raw %>% debounce(800)
  
  # Calculate Filtered Network Data (Debounced)
  net_data_filtered_raw <- reactive({
    # Explicitly depend on the column choice AND the delimiter list
    col <- input$net_col_filtered
    delims <- rv$detected_mv_cols
    df <- glens_year_filtered_rx()
    
    req(col, df, nrow(df) > 0)
    
    # Get the specific delimiter for this column
    target_delim <- if (!is.null(delims[[col]])) delims[[col]] else ","
    
    # Debug print to verify it's firing
    message(paste("Generating network for:", col, "with delim:", target_delim))
    
    build_collaboration_network(df, rv$author_list, col, target_delim)
  })
  net_data_filtered_debounced <- net_data_filtered_raw %>% debounce(800)
  
  # # Render Full Data Network
  # output$network_full <- renderVisNetwork({
  #   net_data <- net_data_full_debounced()
  #   req(net_data, nrow(net_data$edges) > 0)
  #   
  #   visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
  #     visNodes(font = list(size = 14)) %>%
  #     # smooth = FALSE is critical for large graph rendering performance
  #     visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = FALSE) %>%
  #     
  #     # REMOVED: visIgraphLayout() 
  #     # ADDED: Browser-side physics calculation (frees up the R thread)
  #     visPhysics(solver = "forceAtlas2Based", 
  #                forceAtlas2Based = list(gravitationalConstant = -50),
  #                stabilization = list(iterations = 150)) %>%
  #     
  #     visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE, autoResize = TRUE) %>%
  #     addFontAwesome()
  # })

  # # Render Filtered Subset Network (Apply the exact same changes here)
  # output$network_filtered <- renderVisNetwork({
  #   net_data <- net_data_filtered_debounced()
  #   # req(net_data, nrow(net_data$edges) > 0)
  # 
  #   # req(net_data, nrow(net_data$nodes) > 0)
  #   # If the network is empty, draw a single placeholder node
  #   if (is.null(net_data) || nrow(net_data$nodes) == 0) {
  #     empty_nodes <- data.frame(
  #       id = 1,
  #       label = "No collaborative links\nfound for this selection",
  #       shape = "text",
  #       font.size = 20,
  #       font.color = "red"
  #     )
  #     empty_edges <- data.frame(from = integer(0), to = integer(0))
  # 
  #     return(visNetwork(empty_nodes, empty_edges, width = "100%", height = "500px"))
  #   }
  # 
  #   # visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
  #   #   visNodes(font = list(size = 14)) %>%
  #   #   visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = FALSE) %>%
  #   #   visPhysics(solver = "forceAtlas2Based",
  #   #              forceAtlas2Based = list(gravitationalConstant = -50),
  #   #              stabilization = list(iterations = 150)) %>%
  #   #   visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE, autoResize = TRUE) %>%
  #   #   addFontAwesome()
  # 
  #   # rv$is_submitted <- F #FINISH THE SUBMISSION FLOW before the last graph/plot
  # 
  #   visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
  #         visNodes(font = list(size = 14)) %>%
  #         visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
  #         visIgraphLayout(layout = "layout_with_fr") %>%
  #         visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE, autoResize= TRUE) %>%
  #         addFontAwesome()
  # })
  
  output$network_filtered <- renderVisNetwork({
    init_nodes <- data.frame(id = "init_node", hidden = TRUE)
    init_edges <- data.frame(from = character(0), to = character(0))
    
    visNetwork(init_nodes, init_edges, width = "100%", height = "500px") %>%
      visLayout(improvedLayout = FALSE) %>%
      visNodes(
        font = list(size = 14),
        color = list(highlight = list(background = "red", border = "darkred"))
      ) %>%
      visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -50,
          springConstant = 0.08,
          springLength = 100,
          damping = 0.7 # <-- The higher this is (0 to 1), the less "jitter" and bounce you get.
        ),
        stabilization = list(
          enabled = TRUE,
          iterations = 300, # Runs the physics invisibly 300 times before displaying
          updateInterval = 50,
          onlyDynamicEdges = FALSE,
          fit = TRUE
        )
      ) %>%
      # ADDED: multiselect = TRUE allows Ctrl+Click on the canvas
      visInteraction(hover = TRUE, multiselect = TRUE) %>% 
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        autoResize = TRUE
      ) %>%
      # ADDED: Manually send Javascript click events back to Shiny!
      visEvents(
        selectNode = "function(properties) {
          Shiny.setInputValue('network_filtered_clicked', properties.nodes);
        }",
        deselectNode = "function(properties) {
          Shiny.setInputValue('network_filtered_clicked', properties.nodes);
        }"
        # stabilizationIterationsDone = "function () {this.setOptions( { physics: false } );}"
      ) %>%
      addFontAwesome()
    
  })
  
  observeEvent(net_data_filtered_debounced(), {
    net_data <- net_data_filtered_debounced()
    proxy <- visNetworkProxy("network_filtered")
    
    if (is.null(net_data) || nrow(net_data$nodes) == 0) {
      empty_nodes <- data.frame(id = "placeholder_empty", label = "No links", shape = "text", font.size = 20, font.color = "red")
      empty_edges <- data.frame(from = character(0), to = character(0))
      proxy %>% visSetData(nodes = empty_nodes, edges = empty_edges)
      updateSelectizeInput(session, "custom_node_selector", choices = character(0))
      return()
    }
    
    # --- ADDED: Force all IDs to be strings to match the Shiny dropdown! ---
    net_data$nodes$id <- as.character(net_data$nodes$id)
    net_data$edges$from <- as.character(net_data$edges$from)
    net_data$edges$to <- as.character(net_data$edges$to)
    
    proxy %>% visSetData(nodes = net_data$nodes, edges = net_data$edges)
    
    dropdown_choices <- setNames(net_data$nodes$id, net_data$nodes$label)
    updateSelectizeInput(session, "custom_node_selector", 
                         choices = c("Select keyword(s)..." = "", dropdown_choices))
    
  }, ignoreNULL = FALSE)
  
  # 1. Dropdown -> Canvas (Highlights nodes when you type in the dropdown)
  observeEvent(input$custom_node_selector, {
    proxy <- visNetworkProxy("network_filtered")
    
    if (is.null(input$custom_node_selector) || length(input$custom_node_selector) == 0 || all(input$custom_node_selector == "")) {
      proxy %>% visUnselectAll() 
    } else {
      proxy %>% visSelectNodes(id = input$custom_node_selector)
    }
  }, ignoreInit = TRUE, ignoreNULL = FALSE)
  
  # 2. Canvas -> Dropdown (Updates the dropdown when you click the graph)
  observeEvent(input$network_filtered_clicked, {
    # This captures the array of IDs sent from Javascript
    updateSelectizeInput(session, "custom_node_selector", selected = input$network_filtered_clicked)
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  # # Render Full Data Network
  # output$network_full <- renderVisNetwork({
  #   net_data <- net_data_full_debounced()
  #   req(net_data, nrow(net_data$edges) > 0)
  #   
  #   visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
  #     visNodes(font = list(size = 14)) %>%
  #     visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
  #     visIgraphLayout(layout = "layout_with_fr") %>%
  #     visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE, autoResize= TRUE) %>%
  #     addFontAwesome()
  # })
  # 
  # # Render Filtered Subset Network
  # output$network_filtered <- renderVisNetwork({
  #   net_data <- net_data_filtered_debounced()
  #   req(net_data, nrow(net_data$edges) > 0)
  #   
  #   visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
  #     visNodes(font = list(size = 14)) %>%
  #     visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
  #     visIgraphLayout(layout = "layout_with_fr") %>%
  #     visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE, autoResize= TRUE) %>%
  #     addFontAwesome()
  # })
  # 
  # # # Render Full Data Network
  # # output$network_full <- renderVisNetwork({
  # #   req(rv$glens_full_table) # Ensure data exists
  # #   req(nrow(rv$glens_full_table) > 0)
  # #   
  # #   net_data <- build_collaboration_network(rv$glens_full_table, rv$author_list)
  # #   
  # #   visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
  # #     visNodes(font = list(size = 14)) %>%
  # #     visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
  # #     visIgraphLayout(layout = "layout_with_fr") %>%
  # #     visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE, autoResize= TRUE) %>%
  # #     addFontAwesome()
  # # })
  # # 
  # # # Render Filtered Subset Network
  # # output$network_filtered <- renderVisNetwork({
  # #   df <- glens_year_filtered_rx()
  # #   req(df, nrow(df) > 0) # Assuming this is your filtered reactive variable
  # #   net_data <- build_collaboration_network(df, rv$author_list)
  # #   
  # #   visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
  # #     visNodes(font = list(size = 14)) %>%
  # #     visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
  # #     # visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -50)) %>%
  # #     visIgraphLayout(layout = "layout_with_fr") %>%
  # #     visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), nodesIdSelection = TRUE, autoResize= TRUE) %>%
  # #     # visLegend() %>%
  # #     addFontAwesome()
  # # })
  
  output$dynamic_source_ui <- renderUI({
    req(rv$glens_full_table)
    df <- rv$glens_full_table
    
    if("Source" %in% colnames(df)){
      # Safely extract sources and strip out NAs
      available_sources <- as.character(na.omit(unique(df$Source)))
      
      if (length(available_sources) == 0) {
        return(p("Import: No sources identified yet.", style = "color: #888;"))
      }
      
      return(radioButtons("selected_source", label = NULL, 
                          choices = available_sources, 
                          selected = available_sources[1], 
                          inline = FALSE))
    } else {
      # Note: It's best practice not to use shinyjs::hide() inside a renderUI.
      # Returning NULL is enough to naturally empty the UI element!
      return(NULL)
    }
  })
  
  # output$dynamic_source_ui <- renderUI({
  #   # Wait for the full table to be generated by the submit button
  #   req(rv$glens_full_table)
  #   df <- rv$glens_full_table
  #   
  #   if("Source" %in% colnames(df)){
  #     available_sources <- levels(factor(df$Source))
  #     if (length(available_sources) == 0) return(p("Import: No sources identified yet.", style = "color: #888;"))
  #     radioButtons("selected_source", label = NULL, choices = available_sources, selected = available_sources[1], inline = FALSE)
  #   } else {
  #     shinyjs::hide("selected_source") 
  #     # Explicitly return NULL so Shiny doesn't try to render the shinyjs command
  #     return(NULL)
  #   }
  # })
  
  observeEvent(input$reset_ext_controls, {
    req(rv$glens_full_table)
    df <- rv$glens_full_table
    current_cols <- names(df)
    
    for(col_name in current_cols) {
      hex_str <- paste(as.character(charToRaw(col_name)), collapse = "")
      chk_id <- paste0("lookup_chk_", hex_str)
      delim_id <- paste0("lookup_delim_", hex_str)
      
      if (grepl("^(Authors\\(s\\) ID)$", col_name, ignore.case = TRUE)) {
        updateCheckboxInput(session, chk_id, value = TRUE)
        updateTextInput(session, delim_id, value = ",")
      } else {
        updateCheckboxInput(session, chk_id, value = FALSE)
        updateTextInput(session, delim_id, value = "")
      }
    }
  })
  
  #observe for extended panel columns with delimiters
  observe({
    req(rv$glens_full_table)
    df <- rv$glens_full_table
    
    current_cols <- names(df)
    req(length(current_cols) > 0)
    
    new_mv_cols <- list()
    
    for(col_name in current_cols) {
      # USE THE SAME HEX ENCODING AS renderUI
      hex_str <- paste(as.character(charToRaw(col_name)), collapse = "")
      
      is_checked <- input[[paste0("lookup_chk_", hex_str)]]
      
      if (isTRUE(is_checked)) {
        delim <- input[[paste0("lookup_delim_", hex_str)]]
        # if (is.null(delim) || trimws(delim) == "") delim <- "," 
        if (is.null(delim) || trimws(delim) == "") delim <- "" 
        new_mv_cols[[col_name]] <- delim
      }
    }
    
    rv$detected_mv_cols <- new_mv_cols
    
    print("new_mv_cols updated to:")
    print(new_mv_cols)
  })
  
  output$lookup_controls_panel <- renderUI({
    # Abort rendering if table isn't ready
    req(rv$glens_full_table)
    df <- rv$glens_full_table
    
    # --- PRESERVE EXISTING UI STATES ---
    current_auto_refresh <- if (!is.null(isolate(input$auto_refresh_lookup))) isolate(input$auto_refresh_lookup) else TRUE
    current_map_orcid    <- if (!is.null(isolate(input$map_orcid2scopusid))) isolate(input$map_orcid2scopusid) else TRUE
    current_autofill     <- if (!is.null(isolate(input$autofill_scopusid_input))) isolate(input$autofill_scopusid_input) else FALSE
    current_ext_match    <- if (!is.null(isolate(input$ext_match))) isolate(input$ext_match) else TRUE
    # -----------------------------------
    
    cols_to_show <- if (ncol(df) > 0) names(df) else collabnet_required_cols
    
    control_rows <- lapply(cols_to_show, function(col) {
      hex_str <- paste(as.character(charToRaw(col)), collapse = "")
      chk_id <- paste0("lookup_chk_", hex_str)
      delim_id <- paste0("lookup_delim_", hex_str)
      
      existing_chk <- isolate(input[[chk_id]])
      existing_delim <- isolate(input[[delim_id]])
      
      is_checked <- FALSE
      delim_val <- ""
      
      if (!is.null(existing_chk)) {
        is_checked <- existing_chk
        delim_val <- if(!is.null(existing_delim)) existing_delim else ""
      } else if (!is.null(isolate(rv$detected_mv_cols)) && (col %in% names(isolate(rv$detected_mv_cols)))) {
        is_checked <- TRUE
        delim_val <- isolate(rv$detected_mv_cols)[[col]]
      } else {
        if (grepl("^(Authors|DOI|ORCID|Author\\(s\\) ID)$", col, ignore.case = TRUE)) {
          is_checked <- TRUE
          delim_val <- ","
        }
      }
      
      fluidRow(
        style = "margin-bottom: 5px; align-items: center; display: flex;",
        column(6, checkboxInput(chk_id, col, value = is_checked)),
        column(6, textInput(delim_id, label = NULL, value = delim_val, placeholder = "Delimiters (e.g., ; , |)", width = "100%"))
      )
    })
    
    tags$div(
      style = "background-color: #fff3e0; border: 2px solid #ff9800; border-radius: 8px; padding: 15px; margin-top: 15px;",
      
      tags$div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
               tags$h4(icon("cogs"), " Lookup Controls", style = "color: #e65100; margin-top: 0; margin-bottom: 0;"),
               actionButton("reset_ext_controls", "Reset Defaults", icon = icon("undo"), class = "btn-danger", style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 2px 8px; font-size: 0.8em;")
      ),
      
      tags$p(style = "font-size: 0.9em; color: #555;", "Select columns for look-up and their delimiters (if any)."),
      
      # Use the preserved variable here
      checkboxInput("auto_refresh_lookup", "Auto-Refresh Lookup", value = current_auto_refresh),
      
      if (isTRUE(rv$has_scopus_key) || !is.null(glens_env$scopus_key)) {
        tagList(
          checkboxInput(
            inputId = "map_orcid2scopusid", 
            label = tagList(
              "Map ORCiD -> SCOPUS ID ",
              tags$span(
                icon("circle-question"),
                "data-toggle" = "tooltip",
                title = "Map ORCiD(s) to SCOPUS ID(s) one-way for faster retrieval. Auto-refresh does NOT apply to ORCiD -> SCOPUS ID Mapping. Run CollabNET.",
                style = "color: #007bc2; cursor: help; margin-left: 5px; display: inline-block;"
              )
            ),
            value = current_map_orcid # Use preserved state
          ),
          checkboxInput(
            inputId = "autofill_scopusid_input", 
            label = tagList(
              "Autofill SCOPUS ID Input ",
              tags$span(
                icon("circle-question"),
                "data-toggle" = "tooltip",
                title = "Automatically append the discovered SCOPUS IDs into the SCOPUS ID input text box above.",
                style = "color: #007bc2; cursor: help; margin-left: 5px; display: inline-block;"
              )
            ),
            value = current_autofill # Use preserved state
          )
        )
      } else {
        tagList(
          shinyjs::disabled(checkboxInput(
            inputId = "map_orcid2scopusid", 
            label = tagList(
              "Map ORCiD -> SCOPUS ID ",
              tags$span(
                icon("circle-question"),
                "data-toggle" = "tooltip",
                title = "Map ORCiD(s) to SCOPUS ID(s) one-way for faster retrieval. Auto-refresh does NOT apply to ORCiD -> SCOPUS ID Mapping. Run CollabNET.",
                style = "color: #007bc2; cursor: help; margin-left: 5px; display: inline-block;"
              )
            ),
            value = FALSE # Remains strictly false when disabled
          )),
          shinyjs::disabled(checkboxInput(
            inputId = "autofill_scopusid_input", 
            label = tagList(
              "Autofill SCOPUS ID Input ",
              tags$span(
                icon("circle-question"),
                "data-toggle" = "tooltip",
                title = "Automatically append the discovered SCOPUS IDs into the text box above.",
                style = "color: #007bc2; cursor: help; margin-left: 5px; display: inline-block;"
              )
            ),
            value = FALSE # Remains strictly false when disabled
          ))
        )
      },
      
      # Use the preserved variable here
      checkboxInput("ext_match", "Extended Keyword Matching", value = current_ext_match),
      shinyjs::disabled(checkboxInput("ignore_case", "Ignore Case", value = TRUE)),
      
      tags$hr(style = "border-top: 1px solid #ffb74d; margin-top: 10px; margin-bottom: 10px;"),
      tags$div(
        style = "max-height: 250px; overflow-y: auto; overflow-x: hidden; padding-right: 5px;",
        control_rows
      )
    )
  })
  
  # output$lookup_controls_panel <- renderUI({
  #   # Abort rendering if table isn't ready
  #   req(rv$glens_full_table)
  #   df <- rv$glens_full_table
  #   
  #   cols_to_show <- if (ncol(df) > 0) names(df) else collabnet_required_cols
  #   
  #   control_rows <- lapply(cols_to_show, function(col) {
  #     hex_str <- paste(as.character(charToRaw(col)), collapse = "")
  #     chk_id <- paste0("lookup_chk_", hex_str)
  #     delim_id <- paste0("lookup_delim_", hex_str)
  #     
  #     existing_chk <- isolate(input[[chk_id]])
  #     existing_delim <- isolate(input[[delim_id]])
  #     
  #     is_checked <- FALSE
  #     delim_val <- ""
  #     
  #     if (!is.null(existing_chk)) {
  #       is_checked <- existing_chk
  #       delim_val <- if(!is.null(existing_delim)) existing_delim else ""
  #     } else if (!is.null(isolate(rv$detected_mv_cols)) && (col %in% names(isolate(rv$detected_mv_cols)))) {
  #       is_checked <- TRUE
  #       delim_val <- isolate(rv$detected_mv_cols)[[col]]
  #     } else {
  #       if (grepl("^(Authors|DOI|ORCID|Author\\(s\\) ID)$", col, ignore.case = TRUE)) {
  #         is_checked <- TRUE
  #         delim_val <- ","
  #       }
  #     }
  #     
  #     fluidRow(
  #       style = "margin-bottom: 5px; align-items: center; display: flex;",
  #       column(6, checkboxInput(chk_id, col, value = is_checked)),
  #       column(6, textInput(delim_id, label = NULL, value = delim_val, placeholder = "Delimiters (e.g., ; , |)", width = "100%"))
  #     )
  #   })
  #   
  #   tags$div(
  #     style = "background-color: #fff3e0; border: 2px solid #ff9800; border-radius: 8px; padding: 15px; margin-top: 15px;",
  #     
  #     tags$div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
  #              tags$h4(icon("cogs"), " Lookup Controls", style = "color: #e65100; margin-top: 0; margin-bottom: 0;"),
  #              actionButton("reset_ext_controls", "Reset Defaults", icon = icon("undo"), class = "btn-danger", style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 2px 8px; font-size: 0.8em;")
  #     ),
  #     
  #     tags$p(style = "font-size: 0.9em; color: #555;", "Select columns for look-up and their delimiters (if any)."),
  #     checkboxInput("auto_refresh_lookup", "Auto-Refresh Lookup", value = TRUE),
  #     
  #     if (isTRUE(rv$has_scopus_key) || !is.null(glens_env$scopus_key)) {
  #       tagList(
  #         checkboxInput(
  #           inputId = "map_orcid2scopusid", 
  #           label = tagList(
  #             "Map ORCiD -> SCOPUS ID ",
  #             tags$span(
  #               icon("circle-question"),
  #               "data-toggle" = "tooltip",
  #               title = "Map ORCiD(s) to SCOPUS ID(s) one-way for faster retrieval. Auto-refresh does NOT apply to ORCiD -> SCOPUS ID Mapping. Run CollabNET.",
  #               style = "color: #007bc2; cursor: help; margin-left: 5px; display: inline-block;"
  #             )
  #           ),
  #           value = TRUE
  #         ),
  #         checkboxInput(
  #           inputId = "autofill_scopusid_input", 
  #           label = tagList(
  #             "Autofill SCOPUS ID Input ",
  #             tags$span(
  #               icon("circle-question"),
  #               "data-toggle" = "tooltip",
  #               title = "Automatically append the discovered SCOPUS IDs into the SCOPUS ID input text box above.",
  #               style = "color: #007bc2; cursor: help; margin-left: 5px; display: inline-block;"
  #             )
  #           ),
  #           value = FALSE
  #         )
  #       )
  #     } else {
  #       tagList(
  #         shinyjs::disabled(checkboxInput(
  #           inputId = "map_orcid2scopusid", 
  #           label = tagList(
  #             "Map ORCiD -> SCOPUS ID ",
  #             tags$span(
  #               icon("circle-question"),
  #               "data-toggle" = "tooltip",
  #               title = "Map ORCiD(s) to SCOPUS ID(s) one-way for faster retrieval. Auto-refresh does NOT apply to ORCiD -> SCOPUS ID Mapping. Run CollabNET.",
  #               style = "color: #007bc2; cursor: help; margin-left: 5px; display: inline-block;"
  #             )
  #           ),
  #           value = FALSE
  #         )),
  #         shinyjs::disabled(checkboxInput(
  #           inputId = "autofill_scopusid_input", 
  #           label = tagList(
  #             "Autofill SCOPUS ID Input ",
  #             tags$span(
  #               icon("circle-question"),
  #               "data-toggle" = "tooltip",
  #               title = "Automatically append the discovered SCOPUS IDs into the text box above.",
  #               style = "color: #007bc2; cursor: help; margin-left: 5px; display: inline-block;"
  #             )
  #           ),
  #           value = FALSE
  #         ))
  #       )
  #     },
  #     checkboxInput("ext_match", "Extended Keyword Matching", value = TRUE),
  #     shinyjs::disabled(checkboxInput("ignore_case", "Ignore Case", value = TRUE)),
  #     
  #     tags$hr(style = "border-top: 1px solid #ffb74d; margin-top: 10px; margin-bottom: 10px;"),
  #     tags$div(
  #       style = "max-height: 250px; overflow-y: auto; overflow-x: hidden; padding-right: 5px;",
  #       control_rows
  #     )
  #   )
  # })
  
  observeEvent(input$clear_log, {
    rv$log_text <- ""
  })
  
  progress_state <- reactiveValues(orcid_done = 0, doi_done = 0, scopus_done = 0, doi_found = 0)
  
  # =========================================================================
  # --- ASYNC LISTENER: Catch Citation Data from JavaScript ---
  # =========================================================================
  observeEvent(input$api_counts_ready, {
    res <- input$api_counts_ready
    req_id <- res$requestId
    
    # STRICT GATEKEEPER: Only accept if it matches current Run ID
    if (!is.null(rv$current_run_id) && grepl(rv$current_run_id, req_id)) {
      meta_df <- rv$temp_meta[[req_id]]
      
      if (!is.null(meta_df)) {
        valid_citations <- na.omit(unlist(res$counts))
        meta_df$Citations <- if (length(valid_citations) == 0) 0 else as.numeric(max(valid_citations))
        rv$final_results[[req_id]] <- meta_df
        
        rv$doi_processed <- rv$doi_processed + 1
        rv$temp_meta[[req_id]] <- NULL # Nullify to prevent double counting
        
        pct <- round((rv$doi_processed / max(1, rv$doi_total)) * 100)
        
        shinyWidgets::updateProgressBar(
          session, id = "prog_doi", 
          value = rv$doi_processed, 
          total = max(1, rv$doi_total),
          title = sprintf("DOI: %d%% (%d/%d)", pct, rv$doi_processed, rv$doi_total),
          status = if(pct >= 100) "success" else "warning"
        )
        
        # TRIGGER THE PIPELINE!
        if (rv$doi_processed >= rv$doi_total) {
          rv$dois_finished <- TRUE
          rv$trigger_pipeline <- if(is.null(rv$trigger_pipeline)) 1 else rv$trigger_pipeline + 1
        }
      }
    }
  })
  
  # =========================================================================
  # --- THE FINAL PIPELINE TRIGGER ---
  # =========================================================================
  observeEvent(rv$trigger_pipeline, {
    # Ensure BOTH streams are 100% finished before continuing
    if (!isTRUE(rv$scopus_finished) || !isTRUE(rv$dois_finished)) return(NULL)
    
    # Ensure a valid run lock exists
    if(!fs::file_exists(file.path("run.lock"))) return(NULL)
    
    tryCatch({
      message("Both Streams Finished! Merging Data...")
      
      # --- 1. MERGE THE SCRAPED DATA ---
      valid_dois <- Filter(is.data.frame, rv$final_results)
      accumulated_df <- dplyr::bind_rows(valid_dois)
      if (nrow(accumulated_df) > 0) {
        accumulated_df <- accumulated_df %>% dplyr::distinct() %>% dplyr::mutate(Source = "DOI/ORCID")
      }
      
      # Prevent Year mismatch crash before binding
      if(nrow(accumulated_df) > 0 && "Year" %in% names(accumulated_df)) accumulated_df$Year <- as.character(accumulated_df$Year)
      if(nrow(rv$scopus_df) > 0 && "Year" %in% names(rv$scopus_df)) rv$scopus_df$Year <- as.character(rv$scopus_df$Year)
      
      # The unified raw table!
      raw_df <- dplyr::bind_rows(accumulated_df, rv$scopus_df)
      print(paste("raw_df:", nrow(raw_df)))
      
      # --- 2. PARSE THE TARGET AUTHORS ---
      target_variants <- stringi::stri_omit_empty(stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n"))))
      target_variants <- target_variants[target_variants != ""]

      if(length(target_variants) > 0) {
        rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
          vn <- normalize_name(v)
          list(norm = vn, parts = extract_parts(vn))
        })
        rv$author_match_regex <- build_name_regex_for_variants(target_variants)
      } else {
        rv$target_variants_norm <- NULL
        rv$author_match_regex <- NULL
      }

      print("submit_btn:extend_input_table():")
      # --- 3. THE HEAVY LIFTING ---
      extended_df <- extend_input_table(rv, raw_df, rv$author_match_regex, rv$target_variants_norm)
      matched_df <- match_journals(rv, extended_df)

      if(nrow(matched_df) > 0){
        rv$glens_full_table <- matched_df
      }

      # --- 4. UI SETUP ---
      render_skeleton_plots(rv, matched_df, output)

      years <- as.numeric(na.omit(matched_df$Year))
      if (length(years) > 0) {
        min_yr <- min(years)
        max_yr <- max(years)
        updateSliderInput(session, "year_slider", min = min_yr, max = max_yr, value = c(min_yr, max_yr))
      }

      if (!isTRUE(input$auto_refresh_lookup)) {
        later::later(function() {
          isolate({ rv$manual_submit <- if(is.null(rv$manual_submit)) 1 else rv$manual_submit + 1 })
        }, delay = 0.8)
      }

      shinyjs::show("year_slider")
      shinyjs::show("sh_index")
      shinyjs::show("summary_table")
      shinyjs::show("lookup_controls_panel")

      # --- 5. CLEANUP ---
      rv$log_text <- paste(rv$log_text, "<span style='color: green;'>✓ Run complete.</span>", sep="<br>")
      try({ if (fs::file_exists("run.lock")) fs::file_delete("run.lock") }, silent = TRUE)

      rv$is_cancelled <- FALSE
      rv$is_glens_exec <- FALSE
      shinyjs::hide("progress_overlay")
      shinyjs::enable("submit_button")
      
    }, error = function(e) {
      print(e)
      shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 100, status = "danger", title = "Process Failed!")
      rv$log_text <- paste(rv$log_text, sprintf("\n(Master) Failed in Final Processing: %s", conditionMessage(e)), sep="<br>")
      shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
      shinyjs::enable("submit_button")
    })
  })
  
  #Submit Button Event
  observeEvent(input$submit_button, {   # same as bindEvent(input$submit_button)
    # 1. Re-determine the exact list of columns the UI generated
      cols_to_check <- if (!is.null(rv$glens_full_table) && ncol(rv$glens_full_table) > 0) {
        names(rv$glens_full_table)
      } else {
        collabnet_required_cols
      }
      
      # 2. Initialize an empty list to store our results
      columns_to_split <- list()
      
      # 3. Loop through the columns and fetch the inputs using the safe Hex IDs
      for(col in cols_to_check) {
        hex_str <- paste(as.character(charToRaw(col)), collapse = "")
        
        # Fetch the current value of the checkbox and textbox
        is_checked <- input[[paste0("lookup_chk_", hex_str)]]
        delims     <- input[[paste0("lookup_delim_", hex_str)]]
        
        # If the checkbox exists in the UI and is actively checked:
        if (!is.null(is_checked) && isTRUE(is_checked)) {
          # Fallback to comma if they checked it but left delimiter empty
          if (is.null(delims) || trimws(delims) == "") delims <- ","
          
          # Store the delimiter string in our list, using the original column name as the key
          columns_to_split[[col]] <- delims
        }
      }
      
      # --- NEW LOGIC: Fallback for ORCID/DOI ---
      has_orcid <- !is.null(input$orcid_text) && trimws(input$orcid_text) != ""
      has_doi   <- !is.null(input$doi_text) && trimws(input$doi_text) != ""
      has_scopus   <- !is.null(input$scopusid_text) && trimws(input$scopusid_text) != ""
      
      if ((has_scopus || has_orcid || has_doi) && !("Authors" %in% names(columns_to_split))) {
        # 1. Add it to the backend list
        columns_to_split[["Authors"]] <- ","
        
        # 2. Update the UI visually so the user sees it happened
        authors_hex <- paste(as.character(charToRaw("Authors")), collapse = "")
        updateCheckboxInput(session, paste0("lookup_chk_", authors_hex), value = TRUE)
        updateTextInput(session, paste0("lookup_delim_", authors_hex), value = ",")
      }
      
      # Save the final mapped list to rv so the rest of the app can use it!
      rv$detected_mv_cols <- columns_to_split
      
      print(rv$detected_mv_cols)
      
      # basic input guard
      rv$is_cancelled <- FALSE
      rv$is_glens_exec <- T
      # rv$is_submitted <- T
      rv$log_text <- ""
      # rv$glens_etable_final <- NULL
      rv$glens_year_filtered <- NULL
      # rv$scopus_df <- NULL
      # rv$wos_df <- NULL
      # rv$semantic_df <- NULL
      
      fs::file_create(file.path("run.lock"))
      
      # message(str(input$cancel_button))
      
      # Reset the UI Progress Bars to 0%
      shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 0, 
                                      title = "DOI / ORCID Resolver: 0%", status = "info")
      shinyWidgets::updateProgressBar(session, id = "prog_scopus", value = 0, 
                                      title = "Scopus API: 0%", status = "info")
      
      # check_orcid_input <- F
      # input_is_orcid <- F
      if (is.null(input$author_list) || stringi::stri_isempty(input$author_list)) {
        rv$log_text <- paste(rv$log_text, "Keyword list empty.", sep="<br>")
        # shinyjs::enable(id = "submit_button")
        # shinyjs::hide("progress_overlay")
        # rv$is_glens_exec <- F
        # req(input$author_list)
        # return()
      }
      
      shinyjs::disable(id = "submit_button")
      shinyjs::hide(id="year_slider")
      shinyjs::show("progress_overlay")
        
      if (is.null(input$doi_text) || stringi::stri_isempty(input$doi_text)) {
        rv$log_text <- paste(rv$log_text, "No DOIs provided in Input.", sep="<br>")
      }
      orcid_list <- stringr::str_trim(stringi::stri_omit_empty(str_split(input$orcid_text, "\n")[[1]]))
      scopus_list <- stringr::str_trim(stringi::stri_omit_empty(str_split(input$scopusid_text, "\n")[[1]]))
      print(orcid_list)
      # print(length(orcid_list))
      # if(check_orcid_input){
      if (is.null(input$orcid_text) || stringi::stri_isempty(input$orcid_text) || length(orcid_list) == 0) {
        rv$log_text <- paste(rv$log_text, "Empty ORC-ID input.", sep="<br>")
      } 
      
      if(length(orcid_list) > 0 && !all(grepl("^[0-9-]+$", orcid_list, perl=TRUE))){
        showNotification(paste("Error:", "ORCiD can have only numbers and '-'."), type = "error", duration = 5)
        rv$log_text <- paste(rv$log_text, "<span style='color: red;'>Invalid characters in ORCiD</span>", sep="<br>")
        shinyjs::enable(id = "submit_button")
        shinyjs::show(id="year_slider")
        shinyjs::hide("progress_overlay")
        return()
      }
      
      if(length(scopus_list) > 0 && !all(grepl("^[0-9]+$", scopus_list, perl=TRUE))){
        showNotification(paste("Error:", "SCOPUS IDs can have only numbers."), type = "error", duration = 5)
        rv$log_text <- paste(rv$log_text, "<span style='color: red;'>Invalid characters in SCOPUS IDs</span>", sep="<br>")
        shinyjs::enable(id = "submit_button")
        shinyjs::show(id="year_slider")
        shinyjs::hide("progress_overlay")
        return()
      }
      
      doi_lines <- c()
      # print(orcid_list)
      if(length(orcid_list) == 1 && stringi::stri_isempty(orcid_list)){
        orcid_list<- list()
      }
      
      # Ensure lists are clean and empty strings are removed
      orcid_list <- orcid_list[trimws(orcid_list) != ""]
      doi_lines <- doi_lines[trimws(doi_lines) != ""]
      
      # We use a reactiveValues object to safely track progress across all async streams on the main thread
      progress_state$orcid_done = 0
      progress_statedoi_done = 0
      progress_state$scopus_done = 0
      progress_state$doi_found = 0
      
      # --- 1. SCOPUS ---
      if (!is.null(glens_env$scopus_key) && glens_env$scopus_key != "") {
        rv$log_text <- paste(rv$log_text, "Found Scopus API key!", sep="<br>")
        rv$has_key_scopus <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No Scopus key found. Skipping Scopus.", sep="<br>")
        rv$has_key_scopus <- F
      }
      # --- 2. Web of Science ---
      if (!is.null(glens_env$wos_key) && glens_env$wos_key != "") {
        rv$log_text <- paste(rv$log_text, "Found Web of Science API key!", sep="<br>")
        rv$has_key_wos <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No WoS key found. Skipping WoS.", sep="<br>")
        rv$has_key_wos <- F
      }
      # --- 3. Semantic Scholar ---
      if (!is.null(glens_env$semantic_key) && glens_env$semantic_key != "") {
        rv$log_text <- paste(rv$log_text, "Found Semantic Scholar API key!", sep="<br>")
        rv$has_key_semantic <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No Semantic Scholar key found. Skipping Semantic Scholar.", sep="<br>")
        rv$has_key_semantic <- F
      }
      # --- 4. Crossref ---
      if (!is.null(glens_env$crossref_key) && glens_env$crossref_key != "") {
        rv$log_text <- paste(rv$log_text, "Found Crossref API key!", sep="<br>")
        rv$have_key_crossref <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No Crossref key found.", sep="<br>")
        rv$have_key_crossref <- F
      }
      # --- 5. OpenCitations ---
      if (!is.null(glens_env$opencites_key) && glens_env$opencites_key != "") {
        rv$log_text <- paste(rv$log_text, "Found OpenCitations API key!", sep="<br>")
        rv$have_key_opencites <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No OpenCitations key found.", sep="<br>")
        rv$have_key_opencites <- F
      }
      
      # ==============================================================================
      # PHASE 1: ORCID -> SCOPUS Mapping
      # ==============================================================================
      
      # 1. Strictly clean the lists first so setdiff() matches perfectly!
      orcid_list <- trimws(orcid_list)
      orcid_list <- orcid_list[orcid_list != ""]
      
      scopus_list <- trimws(scopus_list)
      scopus_list <- scopus_list[scopus_list != ""]
      
      scopus_count <- length(scopus_list)
      orcid_count <- length(orcid_list)
      
      should_map_orcid <- if (is.null(input$map_orcid2scopusid)) FALSE else isTRUE(input$map_orcid2scopusid)
      
      # 2. Check the CORRECT API key boolean (has_key_scopus)
      if(should_map_orcid && rv$has_key_scopus && orcid_count > 0){
        rv$log_text <- paste(rv$log_text, "Matching ORCiD(s) to SCOPUS ID(s)...", sep="<br>")
        
        # Map using the actual string values
        oid_mappings <- lapply(orcid_list, function(actual_orcid){
          return(get_author_mapping(actual_orcid, id_type = "ORCID", glens_env$scopus_key))
        })
        
        clean_oid_mappings <- Filter(Negate(is.null), oid_mappings)
        
        if (length(clean_oid_mappings) > 0) {
          mapping_df <- dplyr::bind_rows(clean_oid_mappings)
          
          # 3. ONLY extract rows where Scopus actually returned a valid AU-ID
          successful_mappings <- mapping_df[!is.na(mapping_df$Mapped_AUID) & trimws(mapping_df$Mapped_AUID) != "", ]
          
          if (nrow(successful_mappings) > 0) {
            mapped_orcids <- successful_mappings$Searched_ID
            mapped_auids <- successful_mappings$Mapped_AUID
            
            # Remove ONLY the successfully mapped ORCIDs from the orcid_list
            orcid_list <- setdiff(orcid_list, mapped_orcids)
            
            # Append the newly discovered AU-IDs to the scopus_list & deduplicate
            scopus_list <- unique(c(scopus_list, mapped_auids))
            
            # Log the optimization
            rv$log_text <- paste(rv$log_text, 
                                 sprintf("<span style='color: green;'>Optimization: Re-routed %d ORCID(s) to the Scopus Author pipeline!</span>", length(mapped_auids)), 
                                 sep="<br>")
          } else {
            rv$log_text <- paste(rv$log_text, "No underlying Scopus Author IDs found for these ORCIDs.", sep="<br>")
          }
        }
      }
      
      # Refresh counts post-mapping
      scopus_count <- length(scopus_list) 
      orcid_count <- length(orcid_list)
      
      if(scopus_count > 0){
        # Create the communication queue
        progress_queue <- ipc::shinyQueue()
        
        # Define the handler. Notice it directly accepts 'current' and 'total'
        progress_queue$consumer$addHandler(function(signal, msg, env) {
          
          # Extract our values from the msg object
          current <- msg$current
          total <- msg$total
          pct <- round((current / total) * 100)
          
          shinyWidgets::updateProgressBar(
            session, id = "prog_scopus", 
            value = current, total = total,
            title = sprintf("Scopus ID: %d%% (%d/%d)", pct, current, total),
            status = if(pct == 100) "success" else "info"
          )
        }, signal = "update_progress")
        
        # Start the queue
        progress_queue$consumer$start(100)
        
        # progress_state$scopus_done <- 0
        # scopusid_promise <- future({
        #   tryCatch({ 
        #     # if (rv$is_cancelled) return(NULL)
        #     if(!fs::file_exists(file.path("run.lock"))) return(NULL)
        #     
        #     # Handle missing key gracefully & update progress bar
        #     if (!rv$has_key_scopus) {
        #       shinyWidgets::updateProgressBar(
        #         session, id = "prog_scopus", value = scopus_count , total = max(1, scopus_count),
        #         title = sprintf("Scopus ID Skipped (No Key): %d%%", 100), status = "warning"
        #       )
        #       return(promise_resolve(NULL))
        #     }
        #     
        #     # INCREMENT PROGRESS BAR
        #     prog_scopus_reactive <- reactive({ progress_state$scopus_done + 1 })
        #     # progress_state$scopus_done <- progress_state$scopus_done + 1
        #     pct <- round(( isolate(prog_scopus_reactive()) / max(1, scopus_count) ) * 100)
        #     shinyWidgets::updateProgressBar(
        #       session, id = "prog_scopus", value = scopus_count, total = max(1, scopus_count),
        #       title = sprintf("Scopus ID: %d%% (%d/%d)", pct, isolate(prog_scopus_reactive()) , scopus_count),
        #       status = if(pct == 100) "success" else "info"
        #     ) #value = isolate(prog_scopus_reactive())
        #     progress_state$scopus_done <- isolate(prog_scopus_reactive())
        #     return(get_scopus_data_id(scopus_list, rv, glens_env$scopus_key))
        #     
        #   }, error = function(e){
        #     # message("ERROR (get_scopus_data_id()):",str(e),e)
        #     rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>SCOPUS: Error fetching ID(s):",e,"</span>"), sep="<br>")
        #     })
        # }, 
        # globals = c("get_scopus_data_id", "scopus_list","scopus_count", "has_key_scopus", "glens_env", "glens_env$scopus_key", "rv", "session", "progress_state", "print_log"),
        # packages = c("shinyWidgets","dplyr", "httr2", "jsonlite", "tidyr", "purrr", "shiny"), seed = TRUE 
        # ) %...>% (function(res) {
        #   # if (rv$is_cancelled) return(NULL)
        #   if(!fs::file_exists(file.path("run.lock"))) return(NULL)
        #   return(res) 
        # }) 
        
        # Start the background process
        scopusid_promise <- future({
          tryCatch({ 
            if(!fs::file_exists(file.path("run.lock"))) return(NULL)
            
            return(get_scopus_data_id(scopus_list, api_key = glens_env$scopus_key, queue = progress_queue))
            
          }, error = function(e){
            return(list(error = conditionMessage(e)))
          })
        }, 
        globals = c("get_scopus_data_id", "scopus_list", "scopus_count", "glens_env", "progress_queue"),
        packages = c("dplyr", "httr2", "jsonlite", "tidyr", "purrr", "ipc"), seed = TRUE 
        ) %...>% (function(res) {
          if(!fs::file_exists(file.path("run.lock"))) return(NULL)
          print("SCOPUS:1:")
          print(str(res))
          # Handle the error passed back from the tryCatch
          if (is.list(res) && "error" %in% names(res)) {
            rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>SCOPUS Error:", res$error, "</span>"), sep="<br>")
            return(NULL)
          }
          
          return(res) 
        }) %...!% (function(res) {
          # if (rv$is_cancelled) return(NULL)
          if(!fs::file_exists(file.path("run.lock"))) return(NULL)
          progress_state$scopus_done <- progress_state$scopus_done + 1 
          shinyWidgets::updateProgressBar(session, id = "prog_scopus", value =  progress_state$scopus_done , status = "danger", title = "Process Failed!")
          # output$log <- renderText(sprintf("Failed in SCOPUS Processing: %s", conditionMessage(err)))
          warning("Failed SCOPUS ID Processing:", err)
          shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
          shinyjs::enable("submit_button")
          if (is.list(res) && "error" %in% names(res)) {
            rv$log_text <- paste(rv$log_text, paste("Scopus ID Error:", res$error), sep="<br>")
            print(res)
            # output$log <- renderText({rv$log_text})
            return(NULL)
          }
        })
        
        # Wrap all Scopus promises into one master promise
        master_scopusid_promise <- promise_all(scopusid_promise)
      }else{
        master_scopusid_promise <- promise_resolve(list())
      }
      
      # ==============================================================================
      # PHASE 1: ORCID -> DOI EXTRACTION (ASYNC)
      # ==============================================================================
      if (orcid_count > 0) {
        rv$log_text <- paste(rv$log_text, sprintf("\nProcessing %d ORC-ID(s)...\n", length(orcid_list)), sep="<br>")
        
        # output$log <- renderText({rv$log_text})
        print("ORCID:1:")
        # Stream 1: Fetch all ORCIDs in parallel
        orcid_promises <- lapply(orcid_list, function(orcid_str) {
          clean_orcid <- trimws(orcid_str)
          
          if (length(strsplit(clean_orcid, "-")[[1]]) == 4) {
            rv$log_text <- paste(rv$log_text, "Working on ORCID:", clean_orcid, sep="<br>")
            print("ORCID:2:")
          }
          
          future({
            # --- INSIDE FUTURE: Pure R only ---
            if (length(strsplit(clean_orcid, "-")[[1]]) != 4) return(list(error = "Malformed ORCID"))
            
            target_url <- paste0("https://pub.orcid.org/v3.0/", clean_orcid, "/works")
            local_is_WASM <- grepl(pattern="wasm", x=Sys.info()["machine"])
            
            res_text <- ""
            
            # 1. SMART FETCHING
            tryCatch({
              if (local_is_WASM) {
                con <- url(target_url, headers = c(Accept = "application/xml"))
                res_text <- paste(readLines(con, warn = FALSE), collapse = "\n")
                close(con)
              } else {
                # Require httr locally because ORCID blocks base R user-agents!
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
            
            if (trimws(res_text) == "") return(list(df = data.frame(), error = "Empty ORCID response"))
            
            # 2. BULLETPROOF XML PARSING (Ignore Namespaces via local-name())
            xml_vec <- xml2::read_xml(res_text)
            
            # Find all works directly, bypassing <group> or namespace prefixes completely
            xml_summaries <- xml2::xml_find_all(xml_vec, "//*[local-name()='work-summary']")
            
            if (length(xml_summaries) == 0) return(list(df = data.frame(), error = NULL))
            
            xtext_safe <- function(node, xpath) {
              val <- xml2::xml_text(xml2::xml_find_first(node, xpath))
              if (is.na(val) || trimws(val) == "") return(NA_character_) else return(trimws(val))
            }
            
            # 3. EXTRACT DOI DETAILS
            orcid_df <- purrr::map_dfr(xml_summaries, function(w) {
              
              # Get all external IDs attached to this specific paper
              # We use .//* to ensure we only search INSIDE the current work summary
              ext_id_nodes <- xml2::xml_find_all(w, ".//*[local-name()='external-id']")
              
              ext_id_val <- NA_character_
              ext_id_url <- NA_character_
              
              if (length(ext_id_nodes) > 0) {
                # Extract type, value, and URL for all IDs
                id_types <- sapply(ext_id_nodes, function(x) xtext_safe(x, ".//*[local-name()='external-id-type']"))
                id_vals <- sapply(ext_id_nodes, function(x) xtext_safe(x, ".//*[local-name()='external-id-value']"))
                id_urls <- sapply(ext_id_nodes, function(x) xtext_safe(x, ".//*[local-name()='external-id-url']"))
                
                # Hunt for the DOI natively in R
                doi_idx <- which(tolower(id_types) == "doi")
                if (length(doi_idx) > 0) {
                  ext_id_val <- id_vals[doi_idx[1]]
                  ext_id_url <- id_urls[doi_idx[1]]
                } else {
                  # Fallback to the first ID if no DOI exists
                  ext_id_val <- id_vals[1]
                  ext_id_url <- id_urls[1]
                }
              }
              
              tibble::tibble(
                source_name = xtext_safe(w, ".//*[local-name()='source-name']"),
                title = xtext_safe(w, ".//*[local-name()='title']"),
                external_id_value = ext_id_val,
                external_id_url = ext_id_url,
                journal_title = xtext_safe(w, ".//*[local-name()='journal-title']"),
                work_type = xtext_safe(w, ".//*[local-name()='type']"),
                orcid = paste0("https://orcid.org/", clean_orcid)
              )
            })
            
            return(list(df = orcid_df, error = NULL))
            
          }, globals = c("clean_orcid"), seed = TRUE) %...>% (function(res) {
            
            # --- BACK ON MAIN THREAD: Safe to touch Shiny UI and reactives again ---
            if(!fs::file_exists(file.path("run.lock"))) return(NULL)
            print("ORCID:9:")
            orcid_count <- length(orcid_list)
            progress_state$orcid_done <- progress_state$orcid_done + 1 
            pct <- round(( progress_state$orcid_done / max(1, orcid_count) ) * 100)
            
            # FIXED: Replaced scopus_count with orcid_count, and updated the title
            shinyWidgets::updateProgressBar(
              session, 
              id = "prog_doi", 
              value = progress_state$orcid_done, 
              total = max(1, orcid_count),
              title = sprintf("Processing ORCID: %d%%", pct), 
              status = "info"
            )
            
            if (!is.null(res$error)) {
              rv$log_text <- paste(rv$log_text, paste("ORCID Error:", res$error), sep="<br>")
              return(NULL)
            } 
            print("ORCID:10:")
            print(paste("nrow(res$df):",nrow(res$df)))
            return(res$df)
            
          }) %...!% (function(err) {
            # If the future itself crashes, log it safely on the main thread
            warning(paste("ORCID Error:", err))
            rv$log_text <- paste(rv$log_text, paste("System Error during ORCID fetch:", err), sep="<br>")
          })
        })
        
        master_orcid_promise <- promise_all(.list = orcid_promises)
      } else {
        # Fallback: if no ORCIDs were provided, resolve immediately to an empty list
        master_orcid_promise <- promise_resolve(list())
      }
      print("ORCID:11:")
      print("PHASE-2:")
      # ==============================================================================
      # PHASE 2: LAUNCH SCOPUS IMMEDIATELY (Doesn't wait for ORCID extraction)
      # ==============================================================================
      
      # rv$log_text <- paste(rv$log_text, "Launching Scopus fetching in parallel...\n")
      # output$log <- renderText({rv$log_text})
      # if(orcid_count > 0){
        
        # scopus_promises <- lapply(seq_along(orcid_list), function(i) {
        #   
        #     orcid_target <- orcid_list[i]
        #     
        #     # 1. Handle missing key gracefully & update progress bar
        #     if (!has_key_scopus) {
        #       progress_state$scopus_done <- progress_state$scopus_done + 1 
        #       pct <- round(( progress_state$scopus_done / max(1, orcid_count) ) * 100)
        #       shinyWidgets::updateProgressBar(
        #         session, id = "prog_scopus", value = progress_state$scopus_done , total = max(1, orcid_count),
        #         title = sprintf("Scopus Skipped (No Key): %d%%", pct), status = "warning"
        #       )
        #       return(promise_resolve(NULL))
        #     }
        #     
        #     # 2. Launch the Future Worker
        #     future({
        #       tryCatch({ 
        #         # if (rv$is_cancelled) return(NULL)
        #         if(!fs::file_exists(file.path("run.lock"))) return(NULL)
        #         # INCREMENT PROGRESS BAR
        #         prog_scopus_reactive <- reactive({ progress_state$scopus_done + 1 })
        #         # progress_state$scopus_done <- progress_state$scopus_done + 1
        #         pct <- round(( isolate(prog_scopus_reactive()) / max(1, orcid_count) ) * 100)
        #         shinyWidgets::updateProgressBar(
        #           session, id = "prog_scopus", value = isolate(prog_scopus_reactive()), total = max(1, orcid_count),
        #           title = sprintf("Scopus: %d%% (%d/%d)", pct, isolate(prog_scopus_reactive()) , orcid_count),
        #           status = if(pct == 100) "success" else "info"
        #         )
        #         progress_state$scopus_done <- isolate(prog_scopus_reactive())
        #         return(get_scopus_data_orcid(orcid_target, scopusid_target, rv, glens_env$scopus_key))
        #         
        #       }, error = function(e) message("ERROR (get_scopus_data_orcid()):",str(e),e))
        #     }, 
        #     globals = c("get_scopus_data_orcid", "orcid_count", "glens_env", "orcid_target", "scopusid_target", "rv", "session", "progress_state", "print_log"),
        #     packages = c("shinyWidgets","dplyr", "httr2", "jsonlite", "tidyr", "purrr", "shiny"), seed = TRUE 
        #     ) %...>% (function(res) {
        #       # if (rv$is_cancelled) return(NULL)
        #       if(!fs::file_exists(file.path("run.lock"))) return(NULL)
        #       return(res) 
        #     }) %...!% (function(res) {
        #       # if (rv$is_cancelled) return(NULL)
        #       if(!fs::file_exists(file.path("run.lock"))) return(NULL)
        #       progress_state$scopus_done <- progress_state$scopus_done + 1 
        #       shinyWidgets::updateProgressBar(session, id = "prog_scopus", value =  progress_state$scopus_done , status = "danger", title = "Process Failed!")
        #       # output$log <- renderText(sprintf("Failed in SCOPUS Processing: %s", conditionMessage(err)))
        #       warning("Failed SCOPUS Processing:", err)
        #       shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
        #       shinyjs::enable("submit_button")
        #       if (is.list(res) && !is.null(res$error)) {
        #         rv$log_text <- paste(rv$log_text, "\nScopus Error for", orcid_target, ":", res$error)
        #         
        #         # output$log <- renderText({rv$log_text})
        #         return(NULL)
        #       }
        #     })
        #   
        # })
        
        # #wait till SCOPUS ID fetch is complete before querying SCOPUS with ORCiD
        # master_scopusdf_promise <- master_scopusid_promise %...>% (function(scopus_results){
        #   # if (rv$is_cancelled) return(NULL)
        #   if(!fs::file_exists(file.path("run.lock"))) return(NULL)
        #   # print(paste("orcid_results: ", colnames(orcid_results),collapse=","))
        #   scopus_df_tmp <- data.frame()
        #   # 1. Combine DOIs extracted from ORCIDs with manually typed DOIs
        #   extracted_scopus_dfs <- purrr::compact(scopus_results) 
        #   scopus_combo <- data.frame()
        #   if (length(extracted_scopus_dfs) > 0) {
        #     orcid_combo <- dplyr::bind_rows(extracted_scopus_dfs)
        #     missing_url <- is.na(scopus_combo$external_id_url)
        #     scopus_combo[missing_url, "external_id_url"] <- scopus_combo[missing_url, "external_id_value"]
        #     # doi_lines <- unique(c(doi_lines, orcid_combo$external_id_url))
        #     scopus_df_tmp <- scopus_combo %>% dplyr::select(external_id_url, external_id_value, orcid) %>% dplyr::rename(doi_url=external_id_url) %>% dplyr::rename(doi=external_id_value)
        #   }
        #   print(paste("scopus_combo: ",paste(colnames(scopus_combo),collapse=",")))
        #   print(str(scopus_combo))
        #   print(str(scopus_df_tmp))
        #   if(nrow(scopus_df_tmp) > 0){
        #     scopus_df_tmp$doi <- scopus_df_tmp$doi[!is.na(scopus_df_tmp$doi) & trimws(scopus_df_tmp$doi) != ""]
        #     
        #     # Safely parse text box line-by-line
        #     scopus_lines <- unlist(strsplit(input$scopusid_text, "\n"))
        #     scopus_lines <- scopus_lines[trimws(scopus_lines) != ""]
        #     
        #     if (length(scopus_lines) > 0) {
        #       # bind_rows is safer than full_join here because the columns (DOI vs SCOPUS_ID) don't match
        #       scopus_df_tmp <- dplyr::bind_rows(scopus_df_tmp, data.frame(SCOPUS_ID = scopus_lines))
        #     }
        #   }else{
        #     # Safely parse text box line-by-line
        #     scopus_lines <- unlist(strsplit(input$scopusid_text, "\n"))
        #     scopus_lines <- scopus_lines[trimws(scopus_lines) != ""]  
        #     
        #     # Using rep() prevents the "0, 1" row error!
        #     scopus_df_tmp <- data.frame(
        #       doi = scopus_lines, 
        #       orcid = rep(NA_character_, length(scopus_lines)), 
        #       doi_url = scopus_lines
        #     )
        #   }
        #   scopus_df_tmp <- scopus_df_tmp %>% dplyr::distinct()
        #   return(scopus_df_tmp)
        # })
        
        progress_state$scopus_done <- 0
        # --- STREAM B: PARALLEL SCOPUS PROCESSING ---
        scopus_promise <- future({
            tryCatch({ 
              # if (rv$is_cancelled) return(NULL)
              if(!fs::file_exists(file.path("run.lock"))) return(NULL)
            
              # Handle missing key gracefully & update progress bar
              if (!rv$has_key_scopus) {
                shinyWidgets::updateProgressBar(
                  session, id = "prog_scopus", value = orcid_count , total = max(1, orcid_count),
                  title = sprintf("Scopus Skipped (No Key): %d%%", 100), status = "warning"
                )
                return(promise_resolve(NULL))
              }
              print("SCOPUS:2:")    
              # INCREMENT PROGRESS BAR
              prog_scopus_reactive <- reactive({ progress_state$scopus_done + 1 })
              # progress_state$scopus_done <- progress_state$scopus_done + 1
              pct <- round(( isolate(prog_scopus_reactive()) / max(1, orcid_count) ) * 100)
              shinyWidgets::updateProgressBar(
                session, id = "prog_scopus", value = orcid_count, total = max(1, orcid_count),
                title = sprintf("Scopus: %d%% (%d/%d)", pct, isolate(prog_scopus_reactive()) , orcid_count),
                status = if(pct == 100) "success" else "info"
              ) #value = isolate(prog_scopus_reactive())
              progress_state$scopus_done <- isolate(prog_scopus_reactive())
              return(get_scopus_data_orcid(orcid_list, rv, glens_env$scopus_key))
              
            }, error = function(e){
              # message("ERROR (get_scopus_data_orcid()):",str(e),e)
              rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>SCOPUS: Error fetching ORCiD(s):",e,"</span>"), sep="<br>")
              })
          }, 
          globals = c("get_scopus_data_orcid", "orcid_list","orcid_count", "has_key_scopus", "glens_env", "glens_env$scopus_key", "rv", "session", "progress_state", "print_log"),
          packages = c("shinyWidgets","dplyr", "httr2", "jsonlite", "tidyr", "purrr", "shiny"), seed = TRUE 
          ) %...>% (function(res) {
            # if (rv$is_cancelled) return(NULL)
            if(!fs::file_exists(file.path("run.lock"))) return(NULL)
            return(res) 
          }) %...!% (function(err) {
            # if (rv$is_cancelled) return(NULL)
            if(!fs::file_exists(file.path("run.lock"))) return(NULL)
            progress_state$scopus_done <- progress_state$scopus_done + 1 
            shinyWidgets::updateProgressBar(session, id = "prog_scopus", value =  progress_state$scopus_done , status = "danger", title = "Process Failed!")
            # output$log <- renderText(sprintf("Failed in SCOPUS Processing: %s", conditionMessage(err)))
            warning("Failed SCOPUS Processing:", err)
            shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
            shinyjs::enable("submit_button")
            rv$log_text <- paste(rv$log_text, paste("Scopus Error:", err), sep="<br>")
            # output$log <- renderText({rv$log_text})
            print(err)
            return(NULL)
          })
        
        print("SCOPUS:3:")   
        # Wrap all Scopus promises into one master promise
        master_scopus_promise <- promise_all(scopus_promise)
        print("SCOPUS:4:")   
        # ==============================================================================
        # PHASE 3: WAIT FOR ORCIDS -> THEN LAUNCH DOI
        # ==============================================================================
      #   # Notice we assign this to `master_doi_promise`
      #   master_doi_promise <- master_orcid_promise %...>% (function(orcid_results) {
      #     # if (rv$is_cancelled) return(NULL)
      #     if(!fs::file_exists(file.path("run.lock"))) return(NULL)
      #     # print(paste("orcid_results: ", colnames(orcid_results),collapse=","))
      #     doi_df <- data.frame()
      #     # 1. Combine DOIs extracted from ORCIDs with manually typed DOIs
      #     extracted_orcid_dfs <- purrr::compact(orcid_results) 
      #     orcid_combo <- data.frame()
      #     if (length(extracted_orcid_dfs) > 0) {
      #       orcid_combo <- dplyr::bind_rows(extracted_orcid_dfs)
      #       missing_url <- is.na(orcid_combo$external_id_url)
      #       orcid_combo[missing_url, "external_id_url"] <- orcid_combo[missing_url, "external_id_value"]
      #       # doi_lines <- unique(c(doi_lines, orcid_combo$external_id_url))
      #       doi_df <- orcid_combo %>% dplyr::select(external_id_url, external_id_value, orcid) %>% dplyr::rename(doi_url=external_id_url) %>% dplyr::rename(doi=external_id_value)
      #     }
      #     print(paste("orcid_combo: ",paste(colnames(orcid_combo),collapse=",")))
      #     print(str(orcid_combo))
      #     print(str(doi_df))
      #     if(nrow(doi_df) > 0){
      #       doi_df$doi <- doi_df$doi[!is.na(doi_df$doi) & trimws(doi_df$doi) != ""]  
      #       doi_df <- dplyr::full_join(doi_df, data.frame(doi=input$doi_text))
      #     }else{
      #       doi_lines <- input$doi_text[!is.na(input$doi_text) & trimws(input$doi_text) != ""]  
      #       doi_df <- data.frame(doi=doi_lines, orcid=NA, doi_url=doi_lines)
      #     }
      #     doi_df <- doi_df %>% dplyr::distinct()
      #     # doi_count <- length(doi_lines)
      #     rv$doi_count <- nrow(doi_df)
      #     message(paste("DOI COUNT:", rv$doi_count))
      #     rv$log_text <- paste(rv$log_text, sprintf("\nExtracted %d total DOIs. Launching DOIs...\n", rv$doi_count), sep="<br>")
      #     
      #     rv$temp_meta <- list()
      #     rv$final_results <- list()
      #     rv$doi_count <- nrow(doi_df)
      #     progress_state$doi_done <- 0
      #     
      #     # If doi/orcid was given as input and we were able to extract DOIs
      #     if(nrow(doi_df) > 0){
      #       # --- STREAM A: PARALLEL DOI PROCESSING ---
      #       # doi_promises <- lapply(seq(nrow(doi_df)), function(i) {
      #       #   # doi_target <- doi_lines[i]
      #       #   doi_target <- doi_df[i,]
      #       #   future({
      #       #     tryCatch({ 
      #       #       # if (rv$is_cancelled) return(NULL)
      #       #       if(!fs::file_exists(file.path("run.lock"))) return(NULL)
      #       #       ret_df <- doi2gscholarlens(doi_target[["doi"]], doi_target[["orcid"]], rv) 
      #       #       prog_doi_reactive <- reactive({ progress_state$doi_done + 1 })
      #       #       # progress_state$scopus_done <- progress_state$scopus_done + 1
      #       #       pct <- round(( isolate(prog_doi_reactive()) / max(1, rv$doi_count) ) * 100 )
      #       #       shinyWidgets::updateProgressBar(
      #       #         session, id = "prog_doi", value = isolate(prog_doi_reactive()), total = max(1, rv$doi_count),
      #       #         title = sprintf("DOI: %d%% (%d/%d)", pct, isolate(prog_doi_reactive()), rv$doi_count),
      #       #         status = if(pct == 100) "success" else "warning"
      #       #       )
      #       #       progress_state$doi_done <- isolate(prog_doi_reactive())
      #       #       return(ret_df)
      #       #     }, error = function(e){ 
      #       #       message(paste("ERROR (doi2gscholarlens()):", e))
      #       #       warning(traceback()) })
      #       #   }, globals = c("glens_env", "doi_target", "doi2gscholarlens", "rv", "session", "progress_state"), packages = c("shinyWidgets", "stringi", "dplyr", "shiny"), seed = TRUE) %...>% (function(res_df) {
      #       #     # if (rv$is_cancelled) return(NULL)
      #       #     if(!fs::file_exists(file.path("run.lock"))) return(NULL)
      #       #     return(res_df)
      #       #   }) %...!% (function(err) {
      #       #     # if (rv$is_cancelled) return(NULL)
      #       #     if(!fs::file_exists(file.path("run.lock"))) return(NULL)
      #       #     progress_state$doi_done <- progress_state$doi_done + 1 
      #       #     shinyWidgets::updateProgressBar(session, id = "prog_doi", value = progress_state$doi_done , status = "danger", title = "Process Failed!")
      #       #     # output$log <- renderText(sprintf("Failed in DOI Processing: %s", conditionMessage(err)))
      #       #     rv$log_text <- paste(rv$log_text, sprintf("\nFailed in DOI Processing: %s", conditionMessage(err)),sep="<br>")
      #       #     warning(paste("Failed in DOI Processing:", err))
      #       #     message(traceback())
      #       #     shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
      #       #     shinyjs::enable("submit_button")
      #       #   })
      #       #   
      #       # })
      #       # 
      #       # # RETURN the resolved DOI promises to `master_doi_promise`
      #       # return(promise_all(.list = doi_promises))
      #       
      #       # The Dispatcher Loop
      #       for (i in seq_len(nrow(doi_df))) {
      #         doi_target <- doi_df[i, ]
      #         
      #         # Create a unique ID for this row so R knows which citations belong to which row when JS returns them
      #         req_id <- paste0("doi_req_", i) 
      #         
      #         tryCatch({
      #           if(!fs::file_exists(file.path("run.lock"))) stop("Run locked")
      #           
      #           # 1. Prepare RIS metadata synchronously
      #           meta_df <- prepare_doi_metadata(doi_target[["doi"]], doi_target[["orcid"]], rv)
      #           
      #           if (is.null(meta_df)) {
      #             # If RIS fails, immediately increment progress and skip to next
      #             progress_state$doi_done <- progress_state$doi_done + 1
      #             shinyWidgets::updateProgressBar(session, id = "prog_doi", value = progress_state$doi_done, total = rv$doi_count)
      #             next 
      #           }
      #           
      #           # 2. Store the metadata temporarily in R
      #           rv$temp_meta[[req_id]] <- meta_df
      #           
      #           # 3. Fire the JS async fetcher (from previous step)
      #           get_citation_counts_async(
      #             doi_or_url = doi_target[["doi"]],
      #             semanticscholar_key = glens_env$semantic_key, # Or pass your key logic here
      #             input_id = "api_counts_ready",
      #             request_id = req_id
      #           )
      #           
      #         }, error = function(e) {
      #           message(paste("ERROR (Dispatcher):", e))
      #           rv$log_text <- paste(rv$log_text, sprintf("\nFailed in DOI Dispatch: %s", conditionMessage(e)), sep="<br>")
      #           progress_state$doi_done <- progress_state$doi_done + 1
      #         })
      #       }
      #     }else{
      #       return(doi_df)
      #     }
      #   })
      # }else{
      #     # Fallback: if no ORCIDs were provided, resolve immediately to an empty list
      #     master_scopus_promise <- promise_resolve(list())
      #     master_scopusdf_promise <- master_scopusid_promise
      #     # master_doi_promise <- promise_resolve(list())
      # }
      # 
      # promise_all(
      #   dois = master_doi_promise,
      #   scopus_orcid = master_scopus_promise,
      #   scopus_id = master_scopusdf_promise
      # ) %...>% (function(results) {
      #   # if (rv$is_cancelled) return(NULL)
      #   if(!fs::file_exists(file.path("run.lock"))) return(NULL)
      #   
      #   failed_doi_count <- length(Filter(is.null, results$dois))
      #   if(abs(rv$doi_count - failed_doi_count) > 0){
      #     rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>Failed RIS Extraction Count:", abs(rv$doi_count - failed_doi_count), "</span>"), sep="<br>")
      #   }
      #   
      #   # --- 1. MERGE THE SCRAPED DATA ---
      #   # Extract and bind the data from the promises safely
      #   clean_dois <- Filter(Negate(is.null), results$dois)
      #   valid_dois <- Filter(is.data.frame, clean_dois)
      #   accumulated_df <- dplyr::bind_rows(valid_dois)
      #   if (nrow(accumulated_df) > 0) accumulated_df <- accumulated_df %>% dplyr::distinct() %>% dplyr::mutate(Source = "DOI/ORCID")
      #   
      #   clean_scopus_orcid <- Filter(Negate(is.null), results$scopus_orcid)
      #   clean_scopus_id <- Filter(Negate(is.null), results$scopus_id)
      #   rv$scopus_df <- dplyr::bind_rows(purrr::compact(Filter(is.data.frame, clean_scopus_orcid)), purrr::compact(Filter(is.data.frame, clean_scopus_id))) %>% dplyr::distinct()
      #   if (nrow(rv$scopus_df) > 0) rv$scopus_df <- rv$scopus_df %>% dplyr::mutate(Source = "SCOPUS")
      #   
      #   # Prevent Year mismatch crash before binding
      #   if(nrow(accumulated_df) > 0 && "Year" %in% names(accumulated_df)) accumulated_df$Year <- as.character(accumulated_df$Year)
      #   if(nrow(rv$scopus_df) > 0 && "Year" %in% names(rv$scopus_df)) rv$scopus_df$Year <- as.character(rv$scopus_df$Year)
      #   
      #   # The unified raw table!
      #   raw_df <- dplyr::bind_rows(accumulated_df, rv$scopus_df)
      #   print(paste("raw_df:",nrow(raw_df)))
      #   
      #   # --- 2. PARSE THE TARGET AUTHORS ---
      #   # We must do this here so the extend function knows exactly who to search for
      #   target_variants <- stringi::stri_omit_empty(stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n"))))
      #   target_variants <- target_variants[target_variants != ""]
      #   
      #   if(length(target_variants) > 0) {
      #     rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
      #       vn <- normalize_name(v)
      #       list(norm = vn, parts = extract_parts(vn))
      #     })
      #     rv$author_match_regex <- build_name_regex_for_variants(target_variants)
      #   } else {
      #     rv$target_variants_norm <- NULL
      #     rv$author_match_regex <- NULL
      #   }
      #   
      #   print("submit_btn:extend_input_table():")
      #   # --- 3. THE HEAVY LIFTING (Hybrid Paradigm) ---
      #   # Pass raw_df directly into the extension and matching pipeline
      #   extended_df <- extend_input_table(rv, raw_df, rv$author_match_regex, rv$target_variants_norm)
      #   matched_df <- match_journals(rv, extended_df)
      #   
      #   if(nrow(matched_df) > 0){
      #     # Store the final static table. This triggers the rest of the UI!
      #     rv$glens_full_table <- matched_df
      #   }
      #   # print(str(matched_df))
      #   # --- 4. UI SETUP ---
      #   # Initialize empty Skeletons so they are ready for the Proxy
      #   render_skeleton_plots(rv, matched_df, output)
      # 
      #   # Configure Slider safely
      #   years <- as.numeric(na.omit(matched_df$Year))
      #   print(levels(factor(years)))
      #   print(str(years))
      #   if (length(years) > 0) {
      #     min_yr <- min(years)
      #     max_yr <- max(years)
      #     updateSliderInput(session, "year_slider", min = min_yr, max = max_yr, value = c(min_yr, max_yr))
      #   }
      #   
      #   print("HERE0")
      #   # Trigger a manual update if auto-refresh is OFF
      #   if (!isTRUE(input$auto_refresh_lookup)) {
      #     # Increment a counter to signal the reactive graph
      #     # Delay the manual trigger so the browser has time to render the skeletons
      #     later::later(function() {
      #       isolate({
      #         rv$manual_submit <- if(is.null(rv$manual_submit)) 1 else rv$manual_submit + 1
      #       })
      #     }, delay = 0.8) # 800ms delay to safely match your debounce timing
      #   }
      #   
      #   print("HERE1")
      #   print("HERE2")
      #   # Reveal UI Elements
      #   shinyjs::show("year_slider")
      #   shinyjs::show("sh_index")
      #   shinyjs::show("summary_table")
      #   shinyjs::show("lookup_controls_panel")
      #   # shinyjs::show("network_full")
      #   print("HERE3")
      #   # --- 5. CLEANUP ---
      #   rv$log_text <- paste(rv$log_text, "<span style='color: green;'>✓ Run complete.</span>", sep="<br>")
      #   
      #   try({ if (fs::file_exists("run.lock")) fs::file_delete("run.lock") }, silent = TRUE)
      #   
      #   rv$is_cancelled <- FALSE
      #   rv$is_glens_exec <- FALSE   
      #   # shinyjs::delay(1500, shinyjs::hide("progress_overlay"))
      #   shinyjs::hide("progress_overlay")
      #   shinyjs::enable("submit_button")
      #   
      # }) %...!% (function(err) {
      #   print(err)
      #   shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 100, status = "danger", title = "Process Failed!")
      #   rv$log_text <- paste(rv$log_text, sprintf("\n(Master) Failed in DOI/Scopus Processing: %s", conditionMessage(err)),sep="<br>")
      #   shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
      #   shinyjs::enable("submit_button")
      #   if(!fs::file_exists(file.path("run.lock"))) return(NULL)
      # })
        
        # # =========================================================================
        # # --- FINAL PIPELINE GATEKEEPER ---
        # # This function fires ONLY when both DOIs (JS) and Scopus (R Promises) are done
        # # =========================================================================
        # run_final_pipeline <- function() {
        #   if (!isTRUE(rv$scopus_finished) || !isTRUE(rv$dois_finished)) {
        #     return(NULL) # One of them is still working, so wait!
        #   }
        #   
        #   tryCatch({
        #     if(!fs::file_exists(file.path("run.lock"))) return(NULL)
        #     
        #     # --- 1. MERGE THE SCRAPED DATA ---
        #     # Fetch DOIs from the background JS process
        #     valid_dois <- Filter(is.data.frame, rv$final_results)
        #     accumulated_df <- dplyr::bind_rows(valid_dois)
        #     if (nrow(accumulated_df) > 0) {
        #       accumulated_df <- accumulated_df %>% dplyr::distinct() %>% dplyr::mutate(Source = "DOI/ORCID")
        #     }
        #     
        #     # Prevent Year mismatch crash before binding
        #     if(nrow(accumulated_df) > 0 && "Year" %in% names(accumulated_df)) accumulated_df$Year <- as.character(accumulated_df$Year)
        #     if(nrow(rv$scopus_df) > 0 && "Year" %in% names(rv$scopus_df)) rv$scopus_df$Year <- as.character(rv$scopus_df$Year)
        #     
        #     # The unified raw table!
        #     raw_df <- dplyr::bind_rows(accumulated_df, rv$scopus_df)
        #     print(paste("raw_df:", nrow(raw_df)))
        #     
        #     # --- 2. PARSE THE TARGET AUTHORS ---
        #     target_variants <- stringi::stri_omit_empty(stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n"))))
        #     target_variants <- target_variants[target_variants != ""]
        #     
        #     if(length(target_variants) > 0) {
        #       rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
        #         vn <- normalize_name(v)
        #         list(norm = vn, parts = extract_parts(vn))
        #       })
        #       rv$author_match_regex <- build_name_regex_for_variants(target_variants)
        #     } else {
        #       rv$target_variants_norm <- NULL
        #       rv$author_match_regex <- NULL
        #     }
        #     
        #     print("submit_btn:extend_input_table():")
        #     # --- 3. THE HEAVY LIFTING ---
        #     extended_df <- extend_input_table(rv, raw_df, rv$author_match_regex, rv$target_variants_norm)
        #     matched_df <- match_journals(rv, extended_df)
        #     
        #     if(nrow(matched_df) > 0){
        #       rv$glens_full_table <- matched_df
        #     }
        #     
        #     # --- 4. UI SETUP ---
        #     render_skeleton_plots(rv, matched_df, output)
        #     
        #     years <- as.numeric(na.omit(matched_df$Year))
        #     if (length(years) > 0) {
        #       min_yr <- min(years)
        #       max_yr <- max(years)
        #       updateSliderInput(session, "year_slider", min = min_yr, max = max_yr, value = c(min_yr, max_yr))
        #     }
        #     
        #     if (!isTRUE(input$auto_refresh_lookup)) {
        #       later::later(function() {
        #         isolate({ rv$manual_submit <- if(is.null(rv$manual_submit)) 1 else rv$manual_submit + 1 })
        #       }, delay = 0.8) 
        #     }
        #     
        #     shinyjs::show("year_slider")
        #     shinyjs::show("sh_index")
        #     shinyjs::show("summary_table")
        #     shinyjs::show("lookup_controls_panel")
        #     
        #     # --- 5. CLEANUP ---
        #     rv$log_text <- paste(rv$log_text, "<span style='color: green;'>✓ Run complete.</span>", sep="<br>")
        #     try({ if (fs::file_exists("run.lock")) fs::file_delete("run.lock") }, silent = TRUE)
        #     
        #     rv$is_cancelled <- FALSE
        #     rv$is_glens_exec <- FALSE   
        #     shinyjs::hide("progress_overlay")
        #     shinyjs::enable("submit_button")
        #     
        #   }, error = function(e) {
        #     print(e)
        #     shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 100, status = "danger", title = "Process Failed!")
        #     rv$log_text <- paste(rv$log_text, sprintf("\n(Master) Failed in Final Processing: %s", conditionMessage(e)), sep="<br>")
        #     shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
        #     shinyjs::enable("submit_button")
        #   })
        # }
        
        # =========================================================================
        # --- EXECUTE STREAMS ---
        # =========================================================================
        rv$scopus_finished <- FALSE
        rv$dois_finished <- FALSE
        
        print("SCOPUS:5:")   
        
        # --- STREAM A: DOI DISPATCHER ---
        master_orcid_promise %...>% (function(orcid_results) {
          print("SCOPUS:6:")   
          if(!fs::file_exists(file.path("run.lock"))) return(NULL)
          
          doi_df <- data.frame(doi=character(), orcid=character(), doi_url=character(), stringsAsFactors=FALSE)
          extracted_orcid_dfs <- purrr::compact(orcid_results) 
          
          if (length(extracted_orcid_dfs) > 0) {
            orcid_combo <- dplyr::bind_rows(extracted_orcid_dfs)
            
            # Ensure columns exist before trying to modify them!
            if (!"external_id_url" %in% names(orcid_combo)) orcid_combo$external_id_url <- NA_character_
            if (!"external_id_value" %in% names(orcid_combo)) orcid_combo$external_id_value <- NA_character_
            if (!"orcid" %in% names(orcid_combo)) orcid_combo$orcid <- NA_character_
            
            missing_url <- is.na(orcid_combo$external_id_url)
            orcid_combo[missing_url, "external_id_url"] <- orcid_combo[missing_url, "external_id_value"]
            
            doi_df <- orcid_combo %>% 
              dplyr::select(external_id_url, external_id_value, orcid) %>% 
              dplyr::rename(doi_url=external_id_url, doi=external_id_value)
          }
          
          # Safely clean manual DOIs from the UI
          doi_ui_lines <- if (!is.null(input$doi_text)) input$doi_text else character(0)
          doi_ui_lines <- doi_ui_lines[!is.na(doi_ui_lines) & trimws(doi_ui_lines) != ""]
          
          if(nrow(doi_df) > 0) {
            # Using dplyr::filter instead of base subsetting to avoid row-length crashes
            doi_df <- doi_df %>% dplyr::filter(!is.na(doi) & trimws(doi) != "")
            
            # Only join manual DOIs if the user actually typed some
            if (length(doi_ui_lines) > 0) {
              manual_df <- data.frame(doi = doi_ui_lines, stringsAsFactors = FALSE)
              doi_df <- dplyr::full_join(doi_df, manual_df, by = "doi")
            }
          } else {
            if (length(doi_ui_lines) > 0) {
              doi_df <- data.frame(doi=doi_ui_lines, orcid=NA_character_, doi_url=doi_ui_lines, stringsAsFactors=FALSE)
            } else {
              # Truly empty dataframe safe fallback
              doi_df <- data.frame(doi=character(0), orcid=character(0), doi_url=character(0), stringsAsFactors=FALSE)
            }
          }
          
          # doi_df <- doi_df %>% dplyr::distinct()
        #   rv$doi_count <- nrow(doi_df)
        #   rv$log_text <- paste(rv$log_text, sprintf("\nExtracted %d total DOIs. Launching DOIs...\n", rv$doi_count), sep="<br>")
        #   
        #   rv$temp_meta <- list()
        #   rv$final_results <- list()
        #   progress_state$doi_done <- 0
        #   
        #   if(rv$doi_count > 0) {
        #     # Dispatch DOIs to JS concurrently
        #     for (i in seq_len(nrow(doi_df))) {
        #       doi_target <- doi_df[i, ]
        #       req_id <- paste0("doi_req_", i) 
        #       
        #       tryCatch({
        #         meta_df <- prepare_doi_metadata(doi_target[["doi"]], doi_target[["orcid"]], rv)
        #         if (is.null(meta_df)) {
        #           progress_state$doi_done <- progress_state$doi_done + 1
        #           shinyWidgets::updateProgressBar(session, id = "prog_doi", display_pct=T, value = progress_state$doi_done, total = rv$doi_count)
        #           next 
        #         }
        #         rv$temp_meta[[req_id]] <- meta_df
        #         
        #         get_citation_counts_async(doi_target[["doi"]], glens_env$semantic_key, input_id = "api_counts_ready", request_id = req_id)
        #       }, error = function(e) {
        #         progress_state$doi_done <- progress_state$doi_done + 1
        #       })
        #     }
        #   } else {
        #     # No DOIs to process, immediately tell Gatekeeper we are done
        #     rv$dois_finished <- TRUE
        #     run_final_pipeline()
        #   }
        # }) %...!% (function(err) {
        #   # CRITICAL FIX 4: Catch ORCID errors so they don't break the environment silently
        #   print(err)
        #   shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 100, status = "danger", title = "Process Failed!")
        #   rv$log_text <- paste(rv$log_text, sprintf("\n(Master) Failed in ORCID/DOI Extraction: %s", conditionMessage(err)), sep="<br>")
        #   shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
        #   shinyjs::enable("submit_button")
        # })
          
          doi_df <- doi_df %>% dplyr::distinct()
          
          # 1. UPFRONT FILTERING
          # Apply our detector to every raw identifier in the dataframe
          detection_results <- lapply(doi_df$doi, detect_identifier_type)
          
          # Unpack the list of lists into dataframe columns
          doi_df$clean_id <- sapply(detection_results, function(x) x$clean_id)
          doi_df$id_type  <- sapply(detection_results, function(x) x$type)
          doi_df$is_valid <- sapply(detection_results, function(x) x$is_doi)
          
          # Split the traffic!
          valid_dois <- doi_df %>% dplyr::filter(is_valid == TRUE)
          invalid_dois <- doi_df %>% dplyr::filter(is_valid == FALSE)
          
          # 2. BULLETPROOF TRACKING
          rv$current_run_id <- as.character(as.numeric(Sys.time())) # Unique ID prevents ghosts
          rv$temp_meta <- list()
          rv$final_results <- list()
          rv$doi_total <- nrow(doi_df)
          rv$doi_processed <- 0
          rv$js_expected <- nrow(valid_dois)
          
          rv$log_text <- paste(rv$log_text, sprintf("\nExtracted %d total DOIs. Launching...\n", rv$doi_total), sep="<br>")
          print("SCOPUS:7:")   
          # 3. INSTANTLY PROCESS INVALID DOIs (No JS needed)
          if (nrow(invalid_dois) > 0) {
            print("SCOPUS:7.1:")   
            for (i in seq_len(nrow(invalid_dois))) {
              meta_df <- prepare_doi_metadata(invalid_dois$clean_id[i], invalid_dois$orcid[i], rv, id_type = invalid_dois$id_type[i])
              if (!is.null(meta_df)) {
                meta_df$Citations <- 0
                rv$final_results[[paste0("inv_", i)]] <- meta_df
              }
              rv$doi_processed <- rv$doi_processed + 1
            }
          }
          
          print("SCOPUS:8:")   
          # 4. DISPATCH VALID DOIs TO BROWSER
          if (rv$js_expected > 0) {
            print("SCOPUS:8.1:")   
            nrow_valid_dois <- nrow(valid_dois)
            for (i in seq_len(nrow_valid_dois)) {
              doi_target <- valid_dois[i, ]
              req_id <- paste0(rv$current_run_id, "_req_", i) 
              
              meta_df <- prepare_doi_metadata(doi_target[["doi"]], doi_target[["orcid"]], rv)
              pct <- round((i / max(1, nrow_valid_dois)) * 100)
              shinyWidgets::updateProgressBar(session, id = "prog_doi", title= sprintf("Submitting DOI: %d%% (%d/%d)", pct, i, nrow_valid_dois), value = i, total = max(1, nrow_valid_dois))
              if (!is.null(meta_df)) {
                rv$temp_meta[[req_id]] <- meta_df
                get_citation_counts_async(doi_target[["doi"]], glens_env$semantic_key, input_id = "api_counts_ready", request_id = req_id)
              } else {
                rv$js_expected <- rv$js_expected - 1
                rv$doi_processed <- rv$doi_processed + 1
              }
            }
          }
          
          pct <- round((rv$doi_processed / max(1, rv$doi_total)) * 100)
          # Update progress bar for the instantly processed invalid ones
          shinyWidgets::updateProgressBar(session, id = "prog_doi", title = sprintf("DOI: %d%% (%d/%d)", pct, rv$doi_processed, rv$doi_total),, value = rv$doi_processed, total = max(1, rv$doi_total))
          
          # Check if we are miraculously done instantly
          if (rv$js_expected == 0) {
            print("SCOPUS:9:")   
            rv$dois_finished <- TRUE
            rv$trigger_pipeline <- if(is.null(rv$trigger_pipeline)) 1 else rv$trigger_pipeline + 1
          }
        }) %...!% (function(err) {
          print(err)
          shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 100, status = "danger", title = "Process Failed!")
          rv$log_text <- paste(rv$log_text, sprintf("\n(Master) Failed in ORCID/DOI Extraction: %s", conditionMessage(err)), sep="<br>")
          shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
          shinyjs::enable("submit_button")
        })
        
        print("SCOPUS:10:")   
        # --- STREAM B: SCOPUS PROMISE ---
        promise_all(
          scopus_orcid = master_scopus_promise,
          scopus_id = master_scopusid_promise
        ) %...>% (function(results) {
          if(!fs::file_exists(file.path("run.lock"))) return(NULL)
          
          clean_scopus_orcid <- Filter(Negate(is.null), results$scopus_orcid)
          clean_scopus_id <- Filter(Negate(is.null), results$scopus_id)
          
          rv$scopus_df <- dplyr::bind_rows(
            purrr::compact(Filter(is.data.frame, clean_scopus_orcid)), 
            purrr::compact(Filter(is.data.frame, clean_scopus_id))
          ) %>% dplyr::distinct()
          
          if (nrow(rv$scopus_df) > 0) rv$scopus_df <- rv$scopus_df %>% dplyr::mutate(Source = "SCOPUS")
          
          # Tell the Gatekeeper Scopus is done
          rv$scopus_finished <- TRUE
          # run_final_pipeline()
          rv$trigger_pipeline <- if(is.null(rv$trigger_pipeline)) 1 else rv$trigger_pipeline + 1
          
        }) %...!% (function(err) {
          print(err)
          shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 100, status = "danger", title = "Process Failed!")
          rv$log_text <- paste(rv$log_text, sprintf("\n(Master) Failed in Scopus Processing: %s", conditionMessage(err)), sep="<br>")
          shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
          shinyjs::enable("submit_button")
        })

  }) #observeEVENT(submit_button)
  
  # Tell Shiny to render this UI in the background even while the parent div is hidden.
  outputOptions(output, "lookup_controls_panel", suspendWhenHidden = FALSE)
  outputOptions(output, "extended_table", suspendWhenHidden = FALSE)
  
  outputOptions(output, "sh_index", suspendWhenHidden = FALSE)
  outputOptions(output, "summary_table", suspendWhenHidden = FALSE)
  outputOptions(output, "network_filtered", suspendWhenHidden = FALSE)
  
} #server end
