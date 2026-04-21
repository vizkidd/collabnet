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
  )
  
  shinyjs::hide(id =  "year_slider")
  shinyjs::hide("sh_index")
  shinyjs::hide("summary_table")
  shinyjs::hide("acounts_plot")
  shinyjs::hide("ccounts_plot")
  shinyjs::hide("cdist_plot")
  shinyjs::hide("aperc_plot")
  shinyjs::hide("cperc_plot")
  shinyjs::hide("network_filtered")
  shinyjs::hide("network_full")
  shinyjs::hide("extended_table")
  shinyjs::hide("progress_overlay") 
  
  # Try VFS first. If it fails or doesn't exist, fall back to what's already in glens_env
  get_resolved_key <- function(filename, env_fallback) {
    path <- file.path("keys", filename)
    message(paste("Looking for key:", path))
    if (fs::file_exists(path)) {
      tryCatch({
        dec <- sodium::data_decrypt(readRDS(path), key=openssl::sha256(glens_env$privkey_dec))
        return(trimws(rawToChar(dec)))
      }, error = function(e) return(env_fallback)) # Fallback on decrypt error
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

  output$log <- renderText({
    rv$log_text
  })
  
  
  # Render Filtered Subset Network
  output$network_filtered <- renderVisNetwork({
    req(rv$glens_year_filtered, nrow(rv$glens_year_filtered) > 0) # Assuming this is your filtered reactive variable
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
  
  observeEvent(input$browser_stored_keys, {
    keys <- input$browser_stored_keys
    
    if (!is.null(keys$scopus_key) && keys$scopus_key != "") glens_env$scopus_key <- keys$scopus_key
    if (!is.null(keys$wos_key) && keys$wos_key != "") glens_env$wos_key <- keys$wos_key
    if (!is.null(keys$semantic_key) && keys$semantic_key != "") glens_env$semantic_key <- keys$semantic_key
    if (!is.null(keys$crossref_key) && keys$crossref_key != "") glens_env$crossref_key <- keys$crossref_key
    if (!is.null(keys$opencites_key) && keys$opencites_key != "") glens_env$opencites_key <- keys$opencites_key
  })
  
  # output$log <- renderText({
  #   files <- list.files(getwd(), all.files = TRUE, recursive = TRUE)
  #   paste(
  #     "Current working dir:", getwd(),
  #     "\n\nFiles in VFS:\n", 
  #     paste(files, collapse = "\n")
  #   )
  # })
  
  output$extended_table <- DT::renderDataTable({
    req(rv$glens_year_filtered, nrow(rv$glens_year_filtered) > 0)
    # print(paste("Rows:", nrow(rv$glens_year_filtered)))
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
  
  output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
  
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
  
  output$dynamic_source_ui <- renderUI({
    req(rv$glens_etable_final, "Source" %in% colnames(rv$glens_etable_final))
    available_sources <- levels(factor(rv$glens_etable_final$Source))
    if (length(available_sources) == 0) return(p("Import: No sources identified yet.", style = "color: #888;"))
    radioButtons("selected_source", label = NULL, choices = available_sources, selected = available_sources[1], inline = FALSE)
  })
  
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

  output$column_mapping_ui <- renderUI({
    
    # Ensure we have data and an import type selected
    req(rv$imported_data_list, input$col_import_type)
    
    # 1. Get all unique columns across all uploaded files
    all_cols <- unique(unlist(lapply(rv$imported_data_list, names)))
    
    # Find columns common among the uploaded files themselves (in case they uploaded multiple CSVs at once)
    uploaded_common <- Reduce(intersect, lapply(rv$imported_data_list, names))
    
    # Get the target App columns (Existing data, or the required template as a fallback)
    app_cols <- if (!is.null(rv$glens_etable_final) && ncol(rv$glens_etable_final) > 0) {
      names(rv$glens_etable_final)
    } else {
      collabnet_required_cols
    }
    
    # THE FIX: Intersect the uploaded columns with the actual App columns
    common_cols <- intersect(uploaded_common, app_cols)
    
    # 2. Determine current import mode
    import_type <- input$col_import_type
    is_common_mode <- import_type == "Common Columns"
    is_map_mode <- import_type == "Map"
    has_no_common <- is_common_mode && length(common_cols) == 0
    
    # 3. Determine which columns to render
    cols_to_render <- if (is_common_mode && !has_no_common) {
      common_cols
    } else {
      all_cols
    }
    
    # --- 4. Dynamic Info Boxes ---
    info_box <- if (import_type == "All Columns") {
      tags$div(
        style = "background-color: #e3f2fd; border: 1px solid #90caf9; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
        tags$strong(icon("info-circle", style = "color: #1976d2;"), " Required CollabNET Columns:", style = "color: #1565c0;"),
        tags$p(style = "margin-top: 5px; margin-bottom: 5px; font-family: monospace; font-size: 0.9em;", 
               paste(collabnet_required_cols, collapse = ", ")),
        tags$small(style = "color: #555;", "Rename your columns below to match these requirements where applicable.")
      )
    } else if (is_common_mode) {
      if (has_no_common) {
        tags$div(
          style = "background-color: #ffebee; border: 1px solid #ef9a9a; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
          tags$strong(icon("exclamation-triangle", style = "color: #d32f2f;"), " No Common Columns Found", style = "color: #c62828;"),
          tags$p(style = "margin-top: 5px; margin-bottom: 0; font-size: 0.9em; color: #555;", 
                 "None of the uploaded columns match the existing app dataset. Import is disabled.")
        )
      } else {
        tags$div(
          style = "background-color: #e8f5e9; border: 1px solid #a5d6a7; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
          tags$strong(icon("check-circle", style = "color: #388e3c;"), " Common Columns Found", style = "color: #2e7d32;"),
          tags$p(style = "margin-top: 5px; margin-bottom: 0; font-size: 0.9em; color: #555;", 
                 paste("Found", length(common_cols), "columns that match the existing app dataset."))
        )
      }
    } else if (is_map_mode) {
      tags$div(
        style = "background-color: #fff3e0; border: 1px solid #ffcc80; border-radius: 5px; padding: 10px; margin-bottom: 15px;",
        tags$strong(icon("exchange-alt", style = "color: #ef6c00;"), " Map & Align Columns:", style = "color: #e65100;"),
        tags$p(style = "margin-top: 5px; margin-bottom: 5px; font-family: monospace; font-size: 0.9em;", 
               paste(collabnet_required_cols, collapse = ", ")),
        tags$small(style = "color: #555;", "Review auto-mapped columns on the right. Rename, combine, or split your uploaded columns on the left to match the dataset structure.")
      )
    } else {
      NULL
    }
    
    # --- 5. CSS for inline checkboxes ---
    checkbox_css <- tags$style(HTML("
    .mapping-row-checkbox .form-group { margin-bottom: 0 !important; }
    .mapping-row-checkbox .checkbox { margin-top: 0 !important; margin-bottom: 0 !important; }
    .mapping-row-checkbox label { margin-bottom: 0 !important; padding-top: 2px; }
  "))
    
    # --- 6. Generate the UI rows for each column ---
    column_rows <- lapply(cols_to_render, function(col) {
      
      safe_id <- make.names(col)
      
      # Disable UI if "Common Columns" is selected but none exist
      is_disabled <- has_no_common
      chk_val <- !has_no_common 
      
      # Auto-Mapping Logic: Match uploaded col to required col (case-insensitive)
      mapped_val <- col 
      if (exists("collabnet_required_cols")) {
        match_idx <- match(tolower(col), tolower(collabnet_required_cols))
        if (!is.na(match_idx)) {
          mapped_val <- collabnet_required_cols[match_idx] # Apply exact case formatting of required col
        }
      }
      
      # 1. Checkbox
      col_chk <- checkboxInput(paste0("map_chk_", safe_id), label = NULL, value = chk_val)
      if (is_disabled) col_chk <- shinyjs::disabled(col_chk)
      
      # 2. Original Uploaded Column (Read-only Display for Map Mode)
      orig_col_display <- tags$div(
        style = "font-size: 0.9em; color: #495057; background-color: #e9ecef; border: 1px solid #ced4da; border-radius: 4px; padding: 6px 12px; height: 34px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
        title = paste("Uploaded Column:", col),
        col
      )
      
      # 3. Column Name Textbox (Pre-filled with auto-mapped value)
      col_name_tb <- textInput(paste0("map_name_", safe_id), label = NULL, value = mapped_val, width = "100%")
      if (is_disabled) col_name_tb <- shinyjs::disabled(col_name_tb)
      
      # 4. Delimiter Textbox 
      col_delim_tb <- textInput(paste0("map_delim_", safe_id), label = NULL, placeholder = "Delim (e.g. ; |)", width = "100%")
      if (is_disabled) col_delim_tb <- shinyjs::disabled(col_delim_tb)
      
      # 5. Action Buttons
      btn_split <- actionButton(paste0("btn_split_", safe_id), "Split", icon = icon("cut"), class = "btn-sm btn-outline-primary", style = "padding: 4px 8px; font-size: 0.85em;")
      btn_combine <- actionButton(paste0("btn_combine_", safe_id), "Combine", icon = icon("compress-arrows-alt"), class = "btn-sm btn-outline-success", style = "padding: 4px 8px; font-size: 0.85em;")
      btn_drop <- actionButton(paste0("btn_drop_", safe_id), "Drop", icon = icon("trash"), class = "btn-sm btn-outline-danger", style = "padding: 4px 8px; font-size: 0.85em;")
      
      if (is_disabled) {
        btn_split <- shinyjs::disabled(btn_split)
        btn_combine <- shinyjs::disabled(btn_combine)
        btn_drop <- shinyjs::disabled(btn_drop)
      }
      
      # --- 7. Flexbox Layout Construction ---
      # Apply a different structural layout if we are in Map Mode vs standard modes
      
      if (is_map_mode) {
        # Map Mode: [Checkbox] [Original] -> [Mapped] [Delim] [Buttons]
        tags$div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px; padding: 5px; border-bottom: 1px solid #eee; width: 100%;",
          
          tags$div(class = "mapping-row-checkbox", style = "width: 5%; display: flex; justify-content: center; flex-shrink: 0;", col_chk),
          tags$div(style = "width: 20%; padding-right: 5px; flex-shrink: 0;", orig_col_display),
          tags$div(style = "width: 5%; display: flex; justify-content: center; color: #aaa; flex-shrink: 0;", icon("arrow-right")),
          tags$div(style = "width: 20%; padding-right: 5px; flex-shrink: 0;", tags$div(style = "margin-bottom: 0;", col_name_tb)),
          tags$div(style = "width: 15%; padding-right: 10px; flex-shrink: 0;", tags$div(style = "margin-bottom: 0;", col_delim_tb)),
          tags$div(style = "width: 35%; display: flex; gap: 4px; justify-content: flex-end;", btn_split, btn_combine, btn_drop)
        )
      } else {
        # Standard Mode: [Checkbox] [Column Name] [Delim] [Buttons]
        tags$div(
          style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 8px; padding: 5px; border-bottom: 1px solid #eee; width: 100%;",
          
          tags$div(class = "mapping-row-checkbox", style = "width: 5%; display: flex; justify-content: center; flex-shrink: 0;", col_chk),
          tags$div(style = "width: 30%; padding-right: 5px; flex-shrink: 0;", tags$div(style = "margin-bottom: 0;", col_name_tb)),
          tags$div(style = "width: 20%; padding-right: 10px; flex-shrink: 0;", tags$div(style = "margin-bottom: 0;", col_delim_tb)),
          tags$div(style = "width: 45%; display: flex; gap: 4px; justify-content: flex-end;", btn_split, btn_combine, btn_drop)
        )
      }
    })
    
    # --- 8. Return the complete UI ---
    tagList(
      checkbox_css,
      info_box,
      tags$div(
        style = "max-height: 350px; overflow-y: auto; overflow-x: hidden; padding-right: 5px;",
        column_rows
      ),
      # Wait 300ms for UI to paint, then strip the disabled attribute and restore pointer events
      tags$script(HTML("
        setTimeout(function(){ 
          var btn = $('#next_row_merge');
          btn.prop('disabled', false);
          btn.css('pointer-events', 'auto');
          btn.css('opacity', '1');
        }, 300);
      "))
    )
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
    
    if (is.null(rv$glens_etable_final) || ncol(rv$glens_etable_final) == 0) {
      return(tagList(
        tags$div(class = "alert alert-warning", "No existing data to merge with. Selecting 'New'."),
        unlock_script # Make sure they can click confirm!
      ))
    }
    req(input$row_import_type == "Merge")
    
    if (!is.null(rv$glens_etable_final) && !is.null(rv$intermediate_merged_df)) {
      # Only show keys that exist in BOTH datasets to prevent join errors
      common_keys <- intersect(names(rv$glens_etable_final), names(rv$intermediate_merged_df))
      
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
  
  output$lookup_controls_panel <- renderUI({
    # Determine which columns to show
    cols_to_show <- if (!is.null(rv$glens_etable_final) && ncol(rv$glens_etable_final) > 0) {
      names(rv$glens_etable_final)
    } else {
      collabnet_required_cols
    }
    
    current_selections <- isolate(rv$detected_mv_cols)
    
    # Build list of rows using Hex Encoded IDs
    control_rows <- lapply(cols_to_show, function(col) {
      
      hex_str <- paste(as.character(charToRaw(col)), collapse = "")
      chk_id <- paste0("lookup_chk_", hex_str)
      delim_id <- paste0("lookup_delim_", hex_str)
      
      is_checked <- FALSE
      delim_val <- ""
      
      # Restore existing selections safely
      if (!is.null(current_selections) && length(current_selections) > 0) {
        if (col %in% names(current_selections)) {
          is_checked <- TRUE
          delim_val <- current_selections[[col]]
        }
      } else {
        # EXTENDED FALLBACK: Automatically check Authors, DOI, and ORCID/Author IDs on first load
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
    
    # Wrap it all in the panel
    tags$div(
      style = "background-color: #fff3e0; border: 2px solid #ff9800; border-radius: 8px; padding: 15px; margin-top: 15px;",
      
      # Header with inline Reset Button
      tags$div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
               tags$h4(icon("cogs"), " Lookup Controls", style = "color: #e65100; margin-top: 0; margin-bottom: 0;"),
               actionButton("reset_ext_controls", "Reset Defaults", icon = icon("undo"), class = "btn-danger", style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; padding: 2px 8px; font-size: 0.8em;")
      ),
      
      tags$p(style = "font-size: 0.9em; color: #555;", "Select columns for look-up and their delimiters (if any)."),
      
      checkboxInput("auto_refresh_lookup", "Auto-Refresh Lookup", value = TRUE),
      if (!is.null(glens_env$scopus_key)) {
        tagList(
          checkboxInput(
            inputId = "map_orcid2scopusid", 
            label = tagList(
              "Map ORCiD -> SCOPUS ID ",
              icon(
                "circle-question",
                "data-toggle" = "tooltip",
                style = "color: #007bc2; cursor: help; margin-left: 5px;",
                title = "Map ORCiD(s) to SCOPUS ID(s) one-way for faster retrieval. Auto-refresh does NOT apply to ORCiD -> SCOPUS ID Mapping. Run CollabNET."
              )
            ),
            value = TRUE
          ),
          checkboxInput(
            inputId = "autofill_scopusid_input", 
            label = tagList(
              "Autofill SCOPUS ID Input ",
              icon(
                "circle-question",
                "data-toggle" = "tooltip",
                style = "color: #007bc2; cursor: help; margin-left: 5px;",
                title = "Automatically append the discovered SCOPUS IDs into the SCOPUS ID input text box above."
              )
            ),
            value = TRUE
          )
        )
      } else {
        tagList(
          shinyjs::disabled(checkboxInput(
            inputId = "map_orcid2scopusid", 
            label = tagList(
              "Map ORCiD -> SCOPUS ID ",
              icon(
                "circle-question",
                "data-toggle" = "tooltip",
                style = "color: #007bc2; cursor: help; margin-left: 5px;",
                title = "Map ORCiD(s) to SCOPUS ID(s) one-way for faster retrieval. Auto-refresh does NOT apply to ORCiD -> SCOPUS ID Mapping. Run CollabNET."
              )
            ),
            value = FALSE
          )),
          shinyjs::disabled(checkboxInput(
            inputId = "autofill_scopusid_input", 
            label = tagList(
              "Autofill SCOPUS ID Input ",
              icon(
                "circle-question",
                "data-toggle" = "tooltip",
                style = "color: #007bc2; cursor: help; margin-left: 5px;",
                title = "Automatically append the discovered SCOPUS IDs into the text box above."
              )
            ),
            value = FALSE
          ))
        )
      },
      checkboxInput("ext_match", "Extended Keyword Matching", value = TRUE),
      checkboxInput("ignore_case", "Ignore Case", value = TRUE),
      
      tags$hr(style = "border-top: 1px solid #ffb74d; margin-top: 10px; margin-bottom: 10px;"),
      tags$div(
        style = "max-height: 250px; overflow-y: auto; overflow-x: hidden; padding-right: 5px;",
        control_rows
      )
    )
  })
  
  # 1. Gather all lookup controls into a single reactive list
  raw_lookup_inputs <- reactive({
    
    # Base requirements
    req(rv$glens_etable_final)
    
    # If you have dynamic inputs (like checkboxes and delimiters generated per column),
    # you need to read them based on the columns that actually exist.
    cols <- names(rv$glens_etable_final)
    
    # Fetch the dynamic checkbox and delimiter values
    # (Change "lookup_chk_" and "lookup_delim_" to whatever prefix your UI uses)
    dynamic_chks <- lapply(cols, function(c) input[[paste0("lookup_chk_", make.names(c))]])
    dynamic_delims <- lapply(cols, function(c) input[[paste0("lookup_delim_", make.names(c))]])
    dynamic_keywords <- lapply(cols, function(c) input[[paste0("lookup_text_", make.names(c))]])
    
    target_variants <- ""
    if(length(input$author_list) > 0){
      target_variants <- stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n")))
      target_variants <- target_variants[target_variants != ""]
      if(length(target_variants) > 0){
        rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
          vn <- normalize_name(v)
          list(norm = vn, parts = extract_parts(vn))
        })
        rv$author_match_regex <- build_name_regex_for_variants(target_variants)
      }
    }
    # Return a list of EVERYTHING that should trigger a data update
    list(
      source = input$selected_source,
      year = input$year_slider,
      authors = target_variants,
      logic_gate = input$author_logic_gate,
      # orcids = input$orcid_text,
      # dois = input$doi_text,
      
      # The dynamic controls
      chks = dynamic_chks,
      delims = dynamic_delims,
      keywords = dynamic_keywords,
      
      # Add a data trigger so it re-runs automatically if a new file is uploaded
      dataset_trigger = nrow(rv$glens_etable_final) 
    )
  })
  
  # 2. Add a delay (debounce). 
  # It will wait until 800ms has passed since the LAST change to any input in the basket above.
  debounced_inputs <- raw_lookup_inputs %>% debounce(800)
  
  observeEvent(input$reset_ext_controls, {
    req(rv$glens_etable_final)
    current_cols <- names(rv$glens_etable_final)
    
    # Loop through all columns currently displayed in the UI
    for (col_name in current_cols) {
      
      # Recreate the exact Hex IDs so we can target the inputs
      hex_str <- paste(as.character(charToRaw(col_name)), collapse = "")
      chk_id <- paste0("lookup_chk_", hex_str)
      delim_id <- paste0("lookup_delim_", hex_str)
      
      # If the column is Authors, DOI, or ORCID/Author IDs, check it and set delimiter to ','
      if (grepl("^(Authors\\(s\\) ID)$", col_name, ignore.case = TRUE)) {
        updateCheckboxInput(session, chk_id, value = TRUE)
        updateTextInput(session, delim_id, value = ",")
      } else {
        # Otherwise, uncheck it and clear the delimiter
        updateCheckboxInput(session, chk_id, value = FALSE)
        updateTextInput(session, delim_id, value = "")
      }
    }
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
    req(rv$imported_data_list)
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
  
  #observe for extended panel columns with delimiters
  observe({
    # Re-determine the columns that are currently rendered
    current_cols <- if (!is.null(rv$glens_etable_final) && ncol(rv$glens_etable_final) > 0) {
      names(rv$glens_etable_final)
    } else {
      collabnet_required_cols 
    }
    
    # CRITICAL FIX: Ensure at least one column exists AND the UI has finished generating
    req(length(current_cols) > 0)
    req(!is.null(input[["lookup_chk_1"]])) # If input 1 is NULL, UI is still building. Abort to prevent wiping data!
    
    new_mv_cols <- list()
    
    # Loop through by numeric index
    for (i in seq_along(current_cols)) {
      col_name <- current_cols[i]
      
      # Read the Shiny inputs using the numeric ID
      is_checked <- input[[paste0("lookup_chk_", i)]]
      
      if (!is.null(is_checked) && is_checked) {
        delim <- input[[paste0("lookup_delim_", i)]]
        
        # Fallback to comma if they checked it but left the delimiter blank
        if (is.null(delim) || trimws(delim) == "") delim <- "," 
        
        # Assign exactly using the original column name with spaces/symbols intact
        new_mv_cols[[col_name]] <- delim
      }
    }
    
    # Save the mapped list to your reactive values
    rv$detected_mv_cols <- new_mv_cols
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
  
  observeEvent(input$upload_btn, {
    
    tryCatch({
      # --- SUCCESS STATE ---
      # Instantly change to a green checkmark when the file hits the server
      shinyjs::runjs("
        document.getElementById('upload_text').innerText = ' Upload Success!';
        document.getElementById('upload_icon').className = 'fa fa-check';
        document.getElementById('upload_icon').style.color = '#28a745'; // Bootstrap success green
      ")
      
      message("FILE UPLOADED!")
      
      # Note: input$upload_btn is a dataframe. 
      # You can access the actual uploaded file path using: input$upload_btn$datapath
      
      # 2. Iterate through each uploaded file using lapply
      imported_data_list <- lapply(seq_len(nrow(input$upload_btn)), function(i) {
        
        # Shiny stores the original name in 'name', and the temp file in 'datapath'
        file_name <- input$upload_btn$name[i]
        file_path <- input$upload_btn$datapath[i]
        
        # Extract the extension and convert to lowercase for safe matching
        ext <- tolower(tools::file_ext(file_name))
        
        # 3. Invoke the appropriate reader based on the extension
        df <- switch(ext,
                     "csv"  = read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE),
                     "tsv"  = read.delim(file_path, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE),
                     "xlsx" = readxl::read_excel(file_path),
                     "xls"  = readxl::read_excel(file_path),
                     {
                       # Fallback error message if someone uploads a weird file format
                       warning(paste("Unsupported file extension:", ext))
                       NULL 
                     }
        )
        
        return(df)
      })
      
      # Remove any NULLs (in case an unsupported file was skipped)
      rv$imported_data_list <- Filter(Negate(is.null), imported_data_list)
      
      # showModal(modalDialog(
      #   title = tags$span(icon("upload", lib = "font-awesome"), " File Upload Wizard"),
      #   size = "m",
      #   radioButtons("col_import_type", label = "Choose Column import type:", inline = TRUE, choices = c("Common Columns", "All Columns","Merge")),
      #   uiOutput("column_mapping_ui"),
      #   radioButtons("row_import_type", label = "Choose Row import type:", inline = TRUE, choices = c("New", "Append", "Merge")),
      #   uiOutput("row_merge_ui"),
      #   footer = tagList(
      #     actionButton("confirm_import", "Confirm Import", class = "btn-success"),
      #     modalButton("Cancel Import")
      #   ),
      #   easyClose = TRUE
      # ))
      # --- TRANSITION DELAY ---
      # Wait 1.5 seconds (1500ms), then reset the button and open the modal
      shinyjs::delay(1500, {
        
        # Reset the button visually and clear the HTML input value so the same file can be uploaded twice if needed
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
            # Native HTML disabled + CSS pointer-events blocker
            actionButton("next_row_merge", "Next: Configure Rows", 
                         class = "btn-primary", 
                         disabled = "disabled", 
                         style = "pointer-events: none; opacity: 0.5;")
          ),
          easyClose = FALSE # Force them to use the buttons
        ))
        
        if (is.null(rv$glens_etable_final) || ncol(rv$glens_etable_final) == 0) {
          # 1. Safely tell Shiny to change the selection
          updateRadioButtons(session, "col_import_type", selected = "All Columns")
          
          # 2. Wait 100 milliseconds for the modal to render, THEN disable the buttons
          shinyjs::delay(100, {
            shinyjs::runjs("$('input[name=\"col_import_type\"][value=\"Common Columns\"]').prop('disabled', true);")
            # shinyjs::runjs("$('input[name=\"col_import_type\"][value=\"Map\"]').prop('disabled', true);")
          })
        }
      })
    }, error = function(e) {
      
      # --- FAILURE DETECTED ---
      # Change button to a red X
      shinyjs::runjs("
        document.getElementById('upload_text').innerText = ' Upload Failed';
        document.getElementById('upload_icon').className = 'fa fa-times';
        document.getElementById('upload_icon').style.color = '#dc3545'; // Bootstrap danger red
      ")
      
      # Tell the user what went wrong
      showNotification(paste("Failed to process file:", e$message), type = "error", duration = 5)
      rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>Failed to process file:", e$message, "</span>"), sep="<br>")
      
      # Reset the button after 3 seconds so they can try again
      shinyjs::delay(3000, {
        shinyjs::runjs("
          document.getElementById('upload_text').innerText = '';
          document.getElementById('upload_icon').className = 'fa fa-upload';
          document.getElementById('upload_icon').style.color = '';
          document.getElementById('upload_btn').value = '';
        ")
      })
    })
    
  }) #upload_btn
  
  observeEvent(input$next_after_delim, {
    req(rv$intermediate_merged_df, rv$detected_mv_cols)
    merged_df <- rv$intermediate_merged_df
    
    # Apply splits sequentially based on user input
    for (col in names(rv$detected_mv_cols)) {
      action <- input[[paste0("delim_action_", make.names(col))]]
      delim_val <- input[[paste0("delim_val_", make.names(col))]]
      
      if (!is.null(action) && action == "rows" && !is.null(delim_val) && trimws(delim_val) != "") {
        # Construct a regex that handles extra spaces (e.g., splitting "123; 456" properly)
        sep_regex <- paste0("\\s*", escape_regex_inline(delim_val), "\\s*")
        
        # Lengthen the dataframe
        merged_df <- merged_df %>% 
          tidyr::separate_rows(dplyr::all_of(col), sep = sep_regex) %>%
          # Clean up any residual empty spaces
          dplyr::mutate(!!col := trimws(.data[[col]]))
      }
    }
    
    # Filter out empty rows that might have been generated by trailing semicolons (e.g. "ID1; ID2;")
    # (Optional, but good practice for Scopus data)
    
    rv$intermediate_merged_df <- merged_df
    removeModal()
    
    # Advance to Step 2
    show_row_merge_modal(rv, session)
  })
  
  observeEvent(input$next_row_merge, {
    req(rv$imported_data_list)
    imported_data_list <- rv$imported_data_list
    
    print("imported_data_list:")
    print(length(imported_data_list))
    
    # Save the column choice so we don't lose it when Modal 1 closes
    rv$saved_col_import_type <- input$col_import_type 
    
    # --- 1. Extract Inputs from New Dynamic UI ---
    # Get all possible columns across the uploaded datasets
    all_uploaded_cols <- unique(unlist(lapply(imported_data_list, names)))
    
    cols_to_keep <- c()
    rename_map <- list()
    detected_mv_cols <- list() # Store delimiters here now
    
    for (col in all_uploaded_cols) {
      safe_id <- make.names(col)
      
      # Check if the user selected this column for import via checkbox
      is_checked <- input[[paste0("map_chk_", safe_id)]]
      
      # Safety fallback in case UI hasn't finished rendering yet
      if (is.null(is_checked)) is_checked <- TRUE 
      
      if (isTRUE(is_checked)) {
        cols_to_keep <- c(cols_to_keep, col)
        
        # Read the mapped name from the textbox
        mapped_name <- input[[paste0("map_name_", safe_id)]]
        final_name <- col # Default to original name
        
        # If the user changed the text box, save it to the rename map
        if (!is.null(mapped_name) && trimws(mapped_name) != "" && trimws(mapped_name) != col) {
          rename_map[[col]] <- trimws(mapped_name)
          final_name <- trimws(mapped_name) # Track the new name for the delimiter
        }
        
        # Read the delimiter from the new UI textbox
        delim_val <- input[[paste0("map_delim_", safe_id)]]
        if (!is.null(delim_val) && trimws(delim_val) != "") {
          detected_mv_cols[[final_name]] <- trimws(delim_val)
        }
      }
    }
    
    print("(COL) RENAME MAP:")
    print(length(rename_map))
    print(str(rename_map))
    
    # --- 2. Apply Column Subsetting and Mapping ---
    imported_data_list <- lapply(imported_data_list, function(df) {
      
      # 1. Drop columns the user explicitly unchecked
      valid_cols <- intersect(names(df), cols_to_keep)
      df <- df[, valid_cols, drop = FALSE]
      
      # 2. Perform the actual renaming based on the map
      current_names <- names(df)
      for (i in seq_along(current_names)) {
        check_name <- current_names[i]
        if (check_name %in% names(rename_map)) {
          current_names[i] <- rename_map[[check_name]]
        }
      }
      
      # Handle potential duplicate column names if the user mapped two sources to the same target name
      names(df) <- make.unique(current_names, sep = "_")
      
      return(df)
    })
    
    # Combine the uploaded files into one intermediate dataframe
    merged_df <- dplyr::bind_rows(imported_data_list) %>% dplyr::distinct()
    
    # Save outputs to reactive values
    rv$intermediate_merged_df <- merged_df
    rv$detected_mv_cols <- detected_mv_cols
    
    removeModal()
    
    # --- 3. Route to Step 1.5 or Step 2 ---
    # Since delimiters are now entered directly in Step 1, we only trigger the Step 1.5 
    # modal if they actually typed a delimiter, acting as a final confirmation.
    
    if (length(detected_mv_cols) > 0) {
      showModal(modalDialog(
        title = tags$span(icon("cut", lib = "font-awesome"), " Step 1.5: Confirm Splits"),
        size = "l",
        tags$p("You defined delimiters for the columns below. Review them before proceeding."),
        uiOutput("delimiter_ui"), # Assuming delimiter_ui is built to read rv$detected_mv_cols
        footer = tagList(
          modalButton("Cancel"),
          actionButton("next_after_delim", "Apply Splits & Continue to Step 2", class = "btn-warning")
        ),
        easyClose = FALSE
      ))
    } else {
      # If no delimiters were entered, skip completely to Step 2
      show_row_merge_modal(rv, session)
    }
  })

  observeEvent(input$confirm_import, {
    req(rv$intermediate_merged_df)
    merged_df <- rv$intermediate_merged_df
    rv$glens_etable_final_tmp <- rv$glens_etable_final
    
    # Force all columns in both datasets to be character text.
    merged_df <- merged_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    
    # 5. Handle Row Logic
    if (input$row_import_type == "New" || is.null(rv$glens_etable_final)) {
      rv$glens_etable_final <- merged_df
      
    } else if (input$row_import_type == "Append") {
      
      if (rv$saved_col_import_type == "Common Columns") {
        final_common_cols <- intersect(names(rv$glens_etable_final), names(merged_df))
        rv$glens_etable_final <- dplyr::bind_rows(
          rv$glens_etable_final[, final_common_cols, drop = FALSE],
          merged_df[, final_common_cols, drop = FALSE]
        )
      } else {
        print("MERGING:")
        rv$glens_etable_final <- dplyr::bind_rows(rv$glens_etable_final %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)), merged_df)
      }
      
    } else if (input$row_import_type == "Merge") {
      
      join_keys <- input$row_merge_keys
      join_type <- input$join_type # Grab the selected join type
      
      print("join_keys:")
      print(join_keys)
      print("join_type:")
      print(join_type)
      
      if (!is.null(join_keys) && length(join_keys) > 0) {
        
        overlap_cols <- setdiff(intersect(names(rv$glens_etable_final), names(merged_df)), join_keys)
        
        # 1. Dynamically select the join function based on the dropdown
        join_func <- switch(join_type,
                            "inner" = dplyr::inner_join,
                            "left"  = dplyr::left_join,
                            "right" = dplyr::right_join,
                            "full"  = dplyr::full_join)
        
        # 2. Execute the join
        joined_df <- join_func(
          rv$glens_etable_final, 
          merged_df, 
          by = join_keys,  
          suffix = c(".old", ".new"),
          relationship = "many-to-many" 
        )
        
        # 3. Coalesce overlapping columns (prioritizing old data, filling gaps with new data)
        for (col in overlap_cols) {
          old_col <- paste0(col, ".old")
          new_col <- paste0(col, ".new")
          
          # Force both to character to prevent integer/character mismatch crashes
          old_vals <- as.character(joined_df[[old_col]])
          new_vals <- as.character(joined_df[[new_col]])
          
          joined_df[[col]] <- dplyr::coalesce(old_vals, new_vals)
          
          joined_df[[old_col]] <- NULL
          joined_df[[new_col]] <- NULL
        }
        
        # 4. Spread metadata up and down grouped by ALL selected keys
        joined_df <- joined_df %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
          tidyr::fill(dplyr::everything(), .direction = "downup") %>%
          dplyr::ungroup()
        
        rv$glens_etable_final <- joined_df
        
      } else {
        warning("No join keys selected. Falling back to Append.")
        # rv$glens_etable_final <- dplyr::bind_rows(rv$glens_etable_final, merged_df)
        rv$glens_etable_final <- dplyr::bind_rows(rv$glens_etable_final %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)), merged_df)
      }
    }
    
    # --- Apply NA Removal ---
    # if (input$drop_na_rows) {
    #   # Drops any row that has an NA in ANY column
    #   rv$glens_etable_final <- tidyr::drop_na(rv$glens_etable_final)
    # }
    # 
    # if (input$drop_na_cols) {
    #   # Drops any column that has an NA in ANY row
    #   rv$glens_etable_final <- rv$glens_etable_final %>%
    #     dplyr::select(dplyr::where(~ !any(is.na(.))))
    # }
    # Keep rows if ANY column has a non-NA value (drops rows where ALL are NA)
    rv$glens_etable_final <- rv$glens_etable_final %>%
      dplyr::filter(dplyr::if_any(dplyr::everything(), ~ !is.na(.)))
  
    # Keep columns if they don't have ALL NA values (drops columns where ALL are NA)
    rv$glens_etable_final <- rv$glens_etable_final %>%
      dplyr::select(dplyr::where(~ !all(is.na(.))))
    
    # ------------------------------
    # --- 1. Safely Consolidate & Rename Known Columns ---
    # Define all the variations of names that might come from different files
    target_mappings <- list(
      "orcid" = c("orcid", "ORCiD", "Orcid", "ORCID"),
      "SCOPUS_ID" = c("SCOPUS_ID", "SCOPUS ID", "Scopus ID", "Author(s) ID"),
      "Citations" = c("Citations", "Cited by"),
      "User_Journal" = c("User_Journal", "Source title"),
      "doi" = c("doi", "DOI")
    )
    
    for (targ in names(target_mappings)) {
      aliases <- target_mappings[[targ]]
      # Find which of the aliases actually exist in the current dataframe
      found_cols <- intersect(aliases, colnames(rv$glens_etable_final))
      
      if (length(found_cols) > 0) {
        master_vec <- rep(NA_character_, nrow(rv$glens_etable_final))
        
        # Coalesce all found columns into one master vector (forcing character to avoid type crashes)
        for (fc in found_cols) {
          master_vec <- dplyr::coalesce(master_vec, as.character(rv$glens_etable_final[[fc]]))
        }
        
        # Assign the master merged column
        rv$glens_etable_final[[targ]] <- master_vec
        
        # Drop the old alias columns so the dataset stays clean
        drop_cols <- setdiff(found_cols, targ)
        if (length(drop_cols) > 0) {
          rv$glens_etable_final <- rv$glens_etable_final %>% dplyr::select(-dplyr::all_of(drop_cols))
        }
      }
    }
    
    if(nrow(rv$glens_etable_final) <= 0){
      showNotification("Data import/merge returned empty rows. Try different options", type = "error", duration = 10)
      rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>Data import/merge returned empty rows. Try different options </span>"),sep="<br>")
      rv$imported_data_list <- NULL
      rv$intermediate_merged_df <- NULL
      rv$saved_col_import_type <- NULL
      rv$glens_etable_final <- rv$glens_etable_final_tmp
      # rv$glens_input_table <- NULL
      # rv$glens_year_filtered <- NULL
      removeModal()
      return()
    }
    
    # #Checks to make sure CollabNET columns exist
    # # Auto-fill any missing columns with NA. 
    # # allows multi-file upload without it getting rejected for missing columns.
    # for (col in collabnet_required_cols) {
    #   if (!(col %in% colnames(rv$glens_etable_final))) {
    #     rv$glens_etable_final[[col]] <- NA_character_
    #   }
    # }
    
    print(colnames(rv$glens_etable_final))
    print(nrow(rv$glens_etable_final))
    print(str(rv$glens_etable_final))
    print("MERGED_DF:")
    print(colnames(merged_df))
    print(nrow(merged_df))
    print(str(merged_df))
    missing_cols <- setdiff(collabnet_required_cols, colnames(rv$glens_etable_final))
    if(length(missing_cols) > 0) {
      
      # Format the missing columns into a clean string
      missing_str <- paste(missing_cols, collapse=", ")
      # Update the log
      rv$log_text <- paste(rv$log_text, 
                           paste0("<span style='color: red;'>Missing required columns: ", missing_str, "</span>"), 
                           sep="<br>")
      # Show the smaller, targeted notification
      showNotification(paste("Missing columns:", missing_str), type = "error", duration = 10)
      # rv$imported_data_list <- NULL
      # rv$intermediate_merged_df <- NULL
      # rv$saved_col_import_type <- NULL
      # rv$glens_etable_final <- rv$glens_etable_final_tmp
      # # rv$glens_input_table <- NULL
      # # rv$glens_year_filtered <- NULL
      # removeModal()
      # return()
    }
    
    # Cleanup
    rv$glens_etable_final <- dplyr::distinct(rv$glens_etable_final)
    rv$imported_data_list <- NULL
    rv$intermediate_merged_df <- NULL
    rv$saved_col_import_type <- NULL
    rv$log_text <- paste(rv$log_text, paste("Post-Import Total:",nrow(rv$glens_etable_final),"lines..."),sep="<br>")
    # rv$glens_input_table <- rv$glens_etable_final
    print("HERE2.1")
    
    # target_variants <- stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n")))
    # target_variants <- target_variants[target_variants != ""]
    # 
    # # Apply normalization based on Extended Matching checkbox
    # if (isTRUE(input$ext_match)) {
    #   rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
    #     vn <- normalize_name(v)
    #     list(norm = vn, parts = extract_parts(vn))
    #   })
    # } else {
    #   # If Extended Matching is off, skip strict normalization but respect case preference
    #   rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
    #     vn <- if(isTRUE(input$ignore_case)) tolower(v) else v
    #     list(norm = vn, parts = list(vn))
    #   })
    # }
    # 
    # # Pass the ignore_case UI value into the regex builder
    # rv$author_match_regex <- build_name_regex_for_variants(target_variants, ignore_case = input$ignore_case)
    # 
    # extend_input_table(rv)
    rv$glens_year_filtered <- rv$glens_etable_final
    
    # target_variants <- stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n")))
    # target_variants <- target_variants[target_variants != ""]
    # rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
    #   vn <- normalize_name(v)
    #   list(norm = vn, parts = extract_parts(vn))
    # })
    # rv$author_match_regex <- build_name_regex_for_variants(target_variants)
    # print("HERE2.2")
    # extend_input_table(rv)
    # print("HERE2.3")
    # rv$glens_year_filtered <- rv$glens_etable_final
    
    if (nrow(rv$glens_year_filtered) <= 0) {
      rv$log_text <- paste(rv$log_text, "Import: No keywords were matched.",sep="<br>")
      # shinyjs::enable("submit_button")
      # removeModal()
      # return()
    }else{
      print("HERE2.4")
      # compute_indices(rv)
      # output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
      # output$sh_index <- renderUI(HTML(paste("<b>Sh-Index:</b>", "NA")))
      # output$extended_table <- DT::renderDataTable({
      #   DT::datatable(rv$glens_year_filtered, options = list(scrollY = "600px", scrollX = TRUE, paging = TRUE))
      # })
      # match_journals(rv)
      rv$glens_year_filtered <- rv$glens_etable_final
      shinyjs::show("extended_table")
    }
    print("HERE2.5")
    if(length(na.omit(levels(factor(rv$glens_year_filtered$Year)))) > 1){
      min_year <- min(as.numeric(rv$glens_year_filtered$Year), na.rm = TRUE)
      max_year <- max(as.numeric(rv$glens_year_filtered$Year), na.rm = TRUE)
      if (is.finite(min_year) && is.finite(max_year)) {
        updateSliderInput(session, "year_slider", value = c(min_year, max_year), min = min_year, max = max_year)
        shinyjs::show("year_slider")
      }else{
        shinyjs::hide("year_slider")
      }
    }else{
      shinyjs::hide("year_slider")
    }
    # rv$extended_controls <- TRUE
    saveRDS(rv$glens_etable_final, "glens_etable_final.rds")
    
    removeModal()
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
    has_key_scopus    <- !is.null(val_scopus) && val_scopus != ""
    has_key_wos       <- !is.null(val_wos) && val_wos != ""
    has_key_semantic  <- !is.null(val_semantic) && val_semantic != ""
    has_key_crossref  <- !is.null(val_crossref) && val_crossref != ""
    has_key_opencites <- !is.null(val_opencites) && val_opencites != ""
    
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
               
               ', if(has_key_scopus) {
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
                        ', if(has_key_wos) {
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
                        ', if(has_key_semantic) {
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
               
               ', if(has_key_crossref) {
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
               
               ', if(has_key_opencites) {
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
    if(has_key_scopus) updateTextInput(session, "scopus_key", value = val_scopus)
    if(has_key_wos) updateTextInput(session, "wos_key", value = val_wos)
    if(has_key_semantic) updateTextInput(session, "semantic_key", value = val_semantic)
    if(has_key_crossref) updateTextInput(session, "crossref_key", value = val_crossref)
    if(has_key_opencites) updateTextInput(session, "opencites_key", value = val_opencites)
  })
  
  # Save handlers
  observeEvent(input$save_scopus, {
    # req(input$scopus_key)
    if(is.null(input$scopus_key) || stringi::stri_isempty(input$scopus_key)){
      if(fs::file_exists(file.path("keys","scopus.key")))
        fs::file_delete(file.path("keys","scopus.key"))
      # removeModal()
      return()
    }
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
    showNotification("Scopus Key Encrypted and Saved.", type = "message")
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
      shinyjs::hide("summary_table")
      shinyjs::hide("acounts_plot")
      shinyjs::hide("ccounts_plot")
      shinyjs::hide("cdist_plot")
      shinyjs::hide("aperc_plot")
      shinyjs::hide("cperc_plot")
      shinyjs::hide("network_filtered")
      shinyjs::hide("network_full")
      # shinyjs::hide("extended_table")
      shinyjs::hide(id="year_slider")
      shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
      shinyjs::enable(id = "submit_button")
      # Update logs
      rv$log_text <- paste(rv$log_text, paste0("Process cancelled by user.\n"),sep="<br>")
      
      removeModal()
  })
  
  observeEvent(rv$glens_year_filtered, {
    filters <- debounced_inputs()
    # print(paste("length(filters$authors):",length(filters$authors)))
    
    if("SCOPUS_ID" %in% colnames(rv$glens_year_filtered)){
      # req("SCOPUS_ID" %in% colnames(rv$glens_year_filtered))
      # 1. Safely extract the column (handles NULL if the table isn't ready)
      scopus_col <- rv$glens_year_filtered$SCOPUS_ID
      
      if (is.null(scopus_col) || length(scopus_col) == 0) {
        # Safe fallback if data isn't loaded yet
        available_scoupusids <- character(0) 
        
      } else {
        # 2. Split by comma, semicolon, or literal double-quote
        raw_splits <- unlist(strsplit(as.character(scopus_col), split = "[,;\"]", perl = TRUE))
        
        # 3. Trim whitespace
        trimmed_splits <- trimws(raw_splits)
        
        # 4. Remove empty strings and get unique values directly
        available_scoupusids <- unique(trimmed_splits[trimmed_splits != ""])
      }
      # print(available_scoupusids)
      req(length(na.omit(available_scoupusids)) > 0)
      if (isTRUE(input$autofill_scopusid_input)) {
        updateTextAreaInput(session, "scopusid_text", value=paste(available_scoupusids, collapse="\n"))
      } else {
        updateTextAreaInput(session, "scopusid_text", value=NULL)
      }
    }
    print(paste("length(filters$authors):",length(filters$authors)))
    print(paste("colnames(rv$glens_year_filtered):",paste(colnames(rv$glens_year_filtered),collapse=",")))
    req(length(filters$authors) > 0)
    print("HERE3.0")
    req(all(c("First_Author","Second_Author","Co_Author","Corresponding_Author", "Adjusted_Citations", "Qscore") %in% colnames(rv$glens_year_filtered)))
    print("HERE3.1")
    rv$glens_year_filtered <- extend_input_table(rv, rv$glens_year_filtered)
    compute_indices(rv, rv$glens_year_filtered)
    plot_glens_table(rv, session)
  })
  
  # #source selection, slider, author_list ,Slider Events
  # observeEvent(c(rv$glens_etable_final, input$selected_source, input$year_slider, input$author_list, input$author_logic_gate), {
  # 3. Execute the logic when the debounced inputs finally settle
  observeEvent(debounced_inputs(), {
  # observe({
      filters <- debounced_inputs()
      req(filters$source,filters$year)
      req(rv$glens_etable_final, input$selected_source, input$year_slider) #input$author_list
      # message(paste("(post)nrow(rv$glens_etable_final):",nrow(rv$glens_etable_final)))
      # message(paste("(post)colnames(rv$glens_etable_final):",colnames(rv$glens_etable_final)))
      # req("Source" %in% names(rv$glens_etable_final))
      # req("Qscore" %in% names(rv$glens_etable_final))
    
      if(isTRUE(is.null(filters$logic_gate))){
        author_logic_gate <- "OR"
      }else{
        author_logic_gate <- filters$logic_gate
      }
      if(nrow(rv$glens_etable_final)<=0){
        return()
      }
      if(is.na(filters$year[1]) || is.na(filters$year[2])){
        return()
      }
      if(rv$is_glens_exec){
        warning("ColabNET is Executing...")
        return()
      }
      # shinyjs::disable(id="year_slider")
    
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
      
      yeardata_tmp <- data.frame()
      if("Year" %in% colnames(rv$glens_etable_final) && "Source" %in% colnames(rv$glens_etable_final)){
          year_levels <- levels(factor(rv$glens_etable_final[["Year"]]))
          if(length(year_levels) > 1){
            yeardata_tmp <- rv$glens_etable_final %>%
              filter(
                Year >= filters$year[1],
                Year <= filters$year[2],
                Source == filters$source # The new source-based filter logic
              )
            
            # print("rv$glens_etable_final===>")
            # print(rv$glens_etable_final %>%
            #         filter(Year >= input$year_slider[1],
            #                Year <= input$year_slider[2]))
            if(nrow(yeardata_tmp) <= 0){
              # output$log <- renderText({paste("input$year_slider - Warning: No data found for this year range.")})
              rv$log_text <- paste(rv$log_text, paste("input$year_slider - Warning: No data found for this year range."), sep="<br>")
              warning("input$year_slider - Warning: No data found for this year range.")
              # shinyjs::hide("sh_index")
              shinyjs::hide("summary_table")
              shinyjs::hide("acounts_plot")
              shinyjs::hide("ccounts_plot")
              shinyjs::hide("cdist_plot")
              shinyjs::hide("aperc_plot")
              shinyjs::hide("cperc_plot")
              shinyjs::hide("network_filtered")
              # shinyjs::hide("extended_table")
              shinyjs::enable(id="year_slider")
              return()
            }
            
            rv$log_text <- paste(rv$log_text,paste("(Slider:", input$year_slider[1], "-", input$year_slider[2],")","Filtered years to range...", min(yeardata_tmp$Year), "and",max(yeardata_tmp$Year)
            ),paste("Source:", input$selected_source), sep="<br>")
          }else if(length(year_levels) == 1){
            yeardata_tmp <- rv$glens_etable_final %>%
              filter(
                Year >= year_levels,
                Year <= year_levels,
                Source == filters$source 
              )
          }else{
            yeardata_tmp <- rv$glens_etable_final  
          }
      }else{
        rv$log_text <- paste(rv$log_text,"'Year' & 'Source' columns are missing. Skipping filters", sep="<br>")  
        yeardata_tmp <- rv$glens_etable_final  
        }
      # output$log <- renderText({ rv$log_text })
      # print(paste("(Slider:", input$year_slider[1], "-", input$year_slider[2],")","Filtered years to range...", min(rv$glens_year_filtered$Year), "and",max(rv$glens_year_filtered$Year)))
      # print(str(rv$glens_year_filtered$Year))
      #Fetch author info only when auto_refresh_lookup is enabled
      yeardata_tmp[["matched_token"]] <- NULL  
    req(input$auto_refresh_lookup)
      # if (isTRUE(input$auto_refresh_lookup)) {   
      # raw_text <- filters$authors
      print(paste("HERE2:RT:", filters$authors))
      # Only apply the logic gate if the user has actually typed something
      if (!is.null(filters$authors) && length(filters$authors) > 0){ #&& trimws(raw_text) != "") {
        author_list <- filters$authors #unlist(strsplit(filters$authors, "[\n,]"))
        author_list <- stringr::str_squish(author_list)
        author_list <- stringr::str_to_title(author_list)
        author_list <- author_list[author_list != ""]
        
        rv$author_list <- unique(author_list)
        # Apply the logic gate function we built earlier
        if (length(author_list) > 0) {
          
          # 1. Grab current toggle states (fallback to TRUE if NULL)
          ext_match_flag <- if (!is.null(input$ext_match)) input$ext_match else TRUE
          ignore_case_flag <- if (!is.null(input$ignore_case)) input$ignore_case else TRUE
          
          # 2. Identify which columns the user selected in the Lookup Controls
          search_cols <- names(rv$detected_mv_cols)
          if (is.null(search_cols) || length(search_cols) == 0) search_cols <- "Authors"
          valid_search_cols <- intersect(search_cols, colnames(yeardata_tmp))
          if (length(valid_search_cols) == 0) valid_search_cols <- "Authors"
          
          # 3. Apply the logic filter
          filtered_df <- apply_author_logic(
            pubs_df          = yeardata_tmp,
            selected_authors = author_list, 
            gate             = author_logic_gate,
            ext_match        = ext_match_flag,
            ignore_case      = ignore_case_flag,
            search_cols      = valid_search_cols
          )
          print(paste("colnames(filtered_df):", paste(colnames(filtered_df), collapse=",")))
          # 4. Save the newly filtered data to your reactive variable
          yeardata_tmp <- filtered_df
          
          shinyjs::show("summary_table")
          shinyjs::show("acounts_plot")
          shinyjs::show("ccounts_plot")
          shinyjs::show("cdist_plot")
          shinyjs::show("aperc_plot")
          shinyjs::show("cperc_plot")
          shinyjs::show("network_filtered")
        }
      } else{
        # hide plots because no keywords were given
        # shinyjs::hide("sh_index")
        # shinyjs::hide("summary_table")
        print(paste("HERE2.1!!!!:", length(filters$authors)))
        shinyjs::hide("acounts_plot")
        shinyjs::hide("ccounts_plot")
        shinyjs::hide("cdist_plot")
        shinyjs::hide("aperc_plot")
        shinyjs::hide("cperc_plot")
        shinyjs::hide("network_filtered")
        shinyjs::enable(id="year_slider")
        return()
      }
      # }
      
      # output$extended_table <- renderTable(rv$glens_year_filtered, striped = TRUE)
      # output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
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
      
      # output$extended_table <- DT::renderDataTable({
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
      
      req(rv$author_match_regex)
      print("observeEvent(): rv$author_match_regex:")
      print(str(rv$author_match_regex))
      rv$glens_year_filtered <- extend_input_table(rv, yeardata_tmp)
      # rv$glens_year_filtered <- rv$glens_etable_final
      if (nrow(rv$glens_year_filtered) <= 0) {
        rv$log_text <- paste(rv$log_text, "No keywords were matched.",sep="<br>")
        # shinyjs::enable("submit_button")
        # shinyjs::hide("progress_overlay")
        # # req(nrow(rv$glens_year_filtered) > 0)
      } else {
        compute_indices(rv, yeardata_tmp)
        
        rv$glens_year_filtered <- yeardata_tmp
        # shinyjs::show("extended_table")
      }
      
      # plot_glens_table(rv, session)
      # plot_glens_table(rv, output, session)
      # plot_glens_table()
      
      shinyjs::enable(id="year_slider")
  
  })
  
  # observeEvent(input$cancel_button,{
  #   rv$is_cancelled <- TRUE
  #   message("HERE1")
  #   
  #   # Immediately hide the overlay and re-enable the UI
  #   shinyjs::hide("sh_index")
  #   shinyjs::hide("summary_table")
  #   shinyjs::hide("acounts_plot")
  #   shinyjs::hide("ccounts_plot")
  #   shinyjs::hide("cdist_plot")
  #   shinyjs::hide("aperc_plot")
  #   shinyjs::hide("cperc_plot")
  #   shinyjs::hide("network_filtered")
  #   shinyjs::hide("network_full")
  #   shinyjs::hide("extended_table")
  #   shinyjs::hide(id="year_slider")
  #   shinyjs::enable(id = "submit_button")
  #   
  #   # Update logs
  #   rv$log_text <- paste(rv$log_text, paste0("Process cancelled by user.\n"),sep="<br>")
  #   removeModal()
  # })
  
  #Submit Button Event
  observeEvent(input$submit_button, {   # same as bindEvent(input$submit_button)
    req(input$map_orcid2scopusid)  
    I
    # 1. Re-determine the exact list of columns the UI generated
      cols_to_check <- if (!is.null(rv$glens_etable_final) && ncol(rv$glens_etable_final) > 0) {
        names(rv$glens_etable_final)
      } else {
        collabnet_required_cols
      }
      
      # 2. Initialize an empty list to store our results
      columns_to_split <- list()
      
      # 3. Loop through the columns and fetch the inputs using the safe Hex IDs
      for (col in cols_to_check) {
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
      orcid_list <- str_split(input$orcid_text, "\n")[[1]]
      scopus_list <- str_split(input$scopusid_text, "\n")[[1]]
      print(orcid_list)
      # print(length(orcid_list))
      # if(check_orcid_input){
      if (is.null(input$orcid_text) || stringi::stri_isempty(input$orcid_text) || length(orcid_list) == 0) {
        rv$log_text <- paste(rv$log_text, "Empty ORC-ID input.\n")
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
      progress_state <- reactiveValues(orcid_done = 0, doi_done = 0, scopus_done = 0, doi_found = 0)
      
      # --- 1. SCOPUS ---
      if (!is.null(glens_env$scopus_key) && glens_env$scopus_key != "") {
        rv$log_text <- paste(rv$log_text, "Found Scopus API key!", sep="\n")
        has_key_scopus <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No Scopus key found. Skipping Scopus.", sep="\n")
        has_key_scopus <- F
      }
      # --- 2. Web of Science ---
      if (!is.null(glens_env$wos_key) && glens_env$wos_key != "") {
        rv$log_text <- paste(rv$log_text, "Found Web of Science API key!", sep="\n")
        has_key_wos <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No WoS key found. Skipping WoS.", sep="\n")
        has_key_wos <- F
      }
      # --- 3. Semantic Scholar ---
      if (!is.null(glens_env$semantic_key) && glens_env$semantic_key != "") {
        rv$log_text <- paste(rv$log_text, "Found Semantic Scholar API key!", sep="\n")
        has_key_semantic <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No Semantic Scholar key found. Skipping Semantic Scholar.", sep="\n")
        has_key_semantic <- F
      }
      # --- 4. Crossref ---
      if (!is.null(glens_env$crossref_key) && glens_env$crossref_key != "") {
        rv$log_text <- paste(rv$log_text, "Found Crossref API key!", sep="\n")
        have_crossref <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No Crossref key found.", sep="\n")
        have_crossref <- F
      }
      # --- 5. OpenCitations ---
      if (!is.null(glens_env$opencites_key) && glens_env$opencites_key != "") {
        rv$log_text <- paste(rv$log_text, "Found OpenCitations API key!", sep="\n")
        have_opencites <- T
      } else {
        rv$log_text <- paste(rv$log_text, "No OpenCitations key found.", sep="\n")
        have_opencites <- F
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
      
      # 2. Check the CORRECT API key boolean (has_key_scopus)
      if(input$map_orcid2scopusid && has_key_scopus && orcid_count > 0){
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
        progress_state$scopus_done <- 0
        scopusid_promise <- future({
          tryCatch({ 
            # if (rv$is_cancelled) return(NULL)
            if(!fs::file_exists(file.path("run.lock"))) return(NULL)
            
            # Handle missing key gracefully & update progress bar
            if (!has_key_scopus) {
              shinyWidgets::updateProgressBar(
                session, id = "prog_scopus", value = scopus_count , total = max(1, scopus_count),
                title = sprintf("Scopus ID Skipped (No Key): %d%%", 100), status = "warning"
              )
              return(promise_resolve(NULL))
            }
            
            # INCREMENT PROGRESS BAR
            prog_scopus_reactive <- reactive({ progress_state$scopus_done + 1 })
            # progress_state$scopus_done <- progress_state$scopus_done + 1
            pct <- round(( isolate(prog_scopus_reactive()) / max(1, scopus_count) ) * 100)
            shinyWidgets::updateProgressBar(
              session, id = "prog_scopus", value = scopus_count, total = max(1, scopus_count),
              title = sprintf("Scopus ID: %d%% (%d/%d)", pct, isolate(prog_scopus_reactive()) , scopus_count),
              status = if(pct == 100) "success" else "info"
            ) #value = isolate(prog_scopus_reactive())
            progress_state$scopus_done <- isolate(prog_scopus_reactive())
            return(get_scopus_data_id(scopus_list, rv, glens_env$scopus_key))
            
          }, error = function(e){
            # message("ERROR (get_scopus_data_id()):",str(e),e)
            rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>SCOPUS: Error fetching ID(s):",e,"</span>"), sep="<br>")
            })
        }, 
        globals = c("get_scopus_data_id", "scopus_list","scopus_count", "has_key_scopus", "glens_env", "glens_env$scopus_key", "rv", "session", "progress_state", "print_log"),
        packages = c("shinyWidgets","dplyr", "httr2", "jsonlite", "tidyr", "purrr", "shiny"), seed = TRUE 
        ) %...>% (function(res) {
          # if (rv$is_cancelled) return(NULL)
          if(!fs::file_exists(file.path("run.lock"))) return(NULL)
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
          if (is.list(res) && !is.null(res$error)) {
            rv$log_text <- paste(rv$log_text, paste("Scopus ID Error:", res$error), sep="<br>")
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
        rv$log_text <- paste(rv$log_text, sprintf("\nProcessing %d ORC-ID(s)...\n", length(orcid_list)))
        
        # output$log <- renderText({rv$log_text})
        
        # Stream 1: Fetch all ORCIDs in parallel
        orcid_promises <- lapply(orcid_list, function(orcid_str) {
          clean_orcid <- trimws(orcid_str)
          
          # 1. MAIN THREAD: Safe to update Shiny reactives here, BEFORE the future starts
          if (length(strsplit(clean_orcid, "-")[[1]]) == 4) {
            rv$log_text <- paste(rv$log_text, "Working on ORCID:", clean_orcid, sep="<br>")
          }
          
          future({
            # --- INSIDE FUTURE: Pure R only. NO `rv`, NO `session`, NO `input`! ---
            if (length(strsplit(clean_orcid, "-")[[1]]) != 4) return(list(error = "Malformed ORCID"))
            
            target_url <- paste0("https://pub.orcid.org/v3.0/", clean_orcid, "/works")
            
            # Use base R connection
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
            
            # Parse XML 
            xml_vec <- xml2::read_xml(res)
            xml_vec_ns <- xml2::xml_ns(xml_vec)
            xml_groups <- xml2::xml_find_all(xml_vec, ".//activities:group", xml_vec_ns)
            
            # Extract DOI details
            orcid_df <- purrr::map_dfr(xml_groups, function(g) {
              tibble::tibble(
                source_name = xtext(g, ".//common:source-name", xml_vec_ns),
                title = xtext(g, ".//common:title", xml_vec_ns),
                external_id_value = xtext(g, ".//common:external-id-value", xml_vec_ns),
                external_id_url = xtext(g, ".//common:external-id-url", xml_vec_ns),
                last_modified_date = xtext(g, ".//common:last-modified-date", xml_vec_ns),
                journal_title = xtext(g, ".//work:journal-title", xml_vec_ns),
                work_type = xtext(g, ".//work:type", xml_vec_ns),
                orcid = paste0("https://orcid.org/",clean_orcid)
              )
            })
            
            return(list(df = orcid_df, error = NULL))
            
            # Note: I removed 'rv', 'progress_state', and 'print_log' from globals because they shouldn't be here
          }, globals = c("xtext", "clean_orcid"), seed = TRUE) %...>% (function(res) {
            
            # --- BACK ON MAIN THREAD: Safe to touch Shiny UI and reactives again ---
            if(!fs::file_exists(file.path("run.lock"))) return(NULL)
            
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
              rv$log_text <- paste(rv$log_text, "ORCID Error:", res$error, "\n")
              return(NULL)
            } 
            
            return(res$df)
            
          }) %...!% (function(err) {
            # If the future itself crashes, log it safely on the main thread
            warning(paste("ORCID Error:", err))
            rv$log_text <- paste(rv$log_text, "System Error during ORCID fetch:", err, "\n")
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
      
      # rv$log_text <- paste(rv$log_text, "Launching Scopus fetching in parallel...\n")
      # output$log <- renderText({rv$log_text})
      if(orcid_count > 0){
        
        
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
        
        #wait till SCOPUS ID fetch is complete before querying SCOPUS with ORCiD
        master_scopusdf_promise <- master_scopusid_promise %...>% (function(scopus_results){
          # if (rv$is_cancelled) return(NULL)
          if(!fs::file_exists(file.path("run.lock"))) return(NULL)
          # print(paste("orcid_results: ", colnames(orcid_results),collapse=","))
          scopus_df_tmp <- data.frame()
          # 1. Combine DOIs extracted from ORCIDs with manually typed DOIs
          extracted_scopus_dfs <- purrr::compact(scopus_results) 
          scopus_combo <- data.frame()
          if (length(extracted_scopus_dfs) > 0) {
            orcid_combo <- dplyr::bind_rows(extracted_scopus_dfs)
            missing_url <- is.na(scopus_combo$external_id_url)
            scopus_combo[missing_url, "external_id_url"] <- scopus_combo[missing_url, "external_id_value"]
            # doi_lines <- unique(c(doi_lines, orcid_combo$external_id_url))
            scopus_df_tmp <- scopus_combo %>% dplyr::select(external_id_url, external_id_value, orcid) %>% dplyr::rename(doi_url=external_id_url) %>% dplyr::rename(doi=external_id_value)
          }
          print(paste("scopus_combo: ",paste(colnames(scopus_combo),collapse=",")))
          print(str(scopus_combo))
          print(str(scopus_df_tmp))
          if(nrow(scopus_df_tmp) > 0){
            scopus_df_tmp$doi <- scopus_df_tmp$doi[!is.na(scopus_df_tmp$doi) & trimws(scopus_df_tmp$doi) != ""]
            
            # Safely parse text box line-by-line
            scopus_lines <- unlist(strsplit(input$scopusid_text, "\n"))
            scopus_lines <- scopus_lines[trimws(scopus_lines) != ""]
            
            if (length(scopus_lines) > 0) {
              # bind_rows is safer than full_join here because the columns (DOI vs SCOPUS_ID) don't match
              scopus_df_tmp <- dplyr::bind_rows(scopus_df_tmp, data.frame(SCOPUS_ID = scopus_lines))
            }
          }else{
            # Safely parse text box line-by-line
            scopus_lines <- unlist(strsplit(input$scopusid_text, "\n"))
            scopus_lines <- scopus_lines[trimws(scopus_lines) != ""]  
            
            # Using rep() prevents the "0, 1" row error!
            scopus_df_tmp <- data.frame(
              doi = scopus_lines, 
              orcid = rep(NA_character_, length(scopus_lines)), 
              doi_url = scopus_lines
            )
          }
          scopus_df_tmp <- scopus_df_tmp %>% dplyr::distinct()
          return(scopus_df_tmp)
        })
        
        progress_state$scopus_done <- 0
        # --- STREAM B: PARALLEL SCOPUS PROCESSING ---
        scopus_promise <- future({
            tryCatch({ 
              # if (rv$is_cancelled) return(NULL)
              if(!fs::file_exists(file.path("run.lock"))) return(NULL)
            
              # Handle missing key gracefully & update progress bar
              if (!has_key_scopus) {
                shinyWidgets::updateProgressBar(
                  session, id = "prog_scopus", value = orcid_count , total = max(1, orcid_count),
                  title = sprintf("Scopus Skipped (No Key): %d%%", 100), status = "warning"
                )
                return(promise_resolve(NULL))
              }
          
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
          }) %...!% (function(res) {
            # if (rv$is_cancelled) return(NULL)
            if(!fs::file_exists(file.path("run.lock"))) return(NULL)
            progress_state$scopus_done <- progress_state$scopus_done + 1 
            shinyWidgets::updateProgressBar(session, id = "prog_scopus", value =  progress_state$scopus_done , status = "danger", title = "Process Failed!")
            # output$log <- renderText(sprintf("Failed in SCOPUS Processing: %s", conditionMessage(err)))
            warning("Failed SCOPUS Processing:", err)
            shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
            shinyjs::enable("submit_button")
            if (is.list(res) && !is.null(res$error)) {
              rv$log_text <- paste(rv$log_text, paste("Scopus Error:", res$error), sep="<br>")
              # output$log <- renderText({rv$log_text})
              return(NULL)
            }
          })
        
        # Wrap all Scopus promises into one master promise
        master_scopus_promise <- promise_all(scopus_promise)
        
        # ==============================================================================
        # PHASE 3: WAIT FOR ORCIDS -> THEN LAUNCH DOI
        # ==============================================================================
        # Notice we assign this to `master_doi_promise`
        master_doi_promise <- master_orcid_promise %...>% (function(orcid_results) {
          # if (rv$is_cancelled) return(NULL)
          if(!fs::file_exists(file.path("run.lock"))) return(NULL)
          # print(paste("orcid_results: ", colnames(orcid_results),collapse=","))
          doi_df <- data.frame()
          # 1. Combine DOIs extracted from ORCIDs with manually typed DOIs
          extracted_orcid_dfs <- purrr::compact(orcid_results) 
          orcid_combo <- data.frame()
          if (length(extracted_orcid_dfs) > 0) {
            orcid_combo <- dplyr::bind_rows(extracted_orcid_dfs)
            missing_url <- is.na(orcid_combo$external_id_url)
            orcid_combo[missing_url, "external_id_url"] <- orcid_combo[missing_url, "external_id_value"]
            # doi_lines <- unique(c(doi_lines, orcid_combo$external_id_url))
            doi_df <- orcid_combo %>% dplyr::select(external_id_url, external_id_value, orcid) %>% dplyr::rename(doi_url=external_id_url) %>% dplyr::rename(doi=external_id_value)
          }
          print(paste("orcid_combo: ",paste(colnames(orcid_combo),collapse=",")))
          print(str(orcid_combo))
          print(str(doi_df))
          if(nrow(doi_df) > 0){
            doi_df$doi <- doi_df$doi[!is.na(doi_df$doi) & trimws(doi_df$doi) != ""]  
            doi_df <- dplyr::full_join(doi_df, data.frame(doi=input$doi_text))
          }else{
            doi_lines <- input$doi_text[!is.na(input$doi_text) & trimws(input$doi_text) != ""]  
            doi_df <- data.frame(doi=doi_lines, orcid=NA, doi_url=doi_lines)
          }
          doi_df <- doi_df %>% dplyr::distinct()
          # doi_count <- length(doi_lines)
          rv$doi_count <- nrow(doi_df)
          message(paste("DOI COUNT:", rv$doi_count))
          rv$log_text <- paste(rv$log_text, sprintf("\nExtracted %d total DOIs. Launching DOIs...\n", rv$doi_count))
          
          # If doi/orcid was given as input and we were able to extract DOIs
          if(nrow(doi_df) > 0){
            # --- STREAM A: PARALLEL DOI PROCESSING ---
            doi_promises <- lapply(seq(nrow(doi_df)), function(i) {
              # doi_target <- doi_lines[i]
              doi_target <- doi_df[i,]
              future({
                tryCatch({ 
                  # if (rv$is_cancelled) return(NULL)
                  if(!fs::file_exists(file.path("run.lock"))) return(NULL)
                  ret_df <- doi2gscholarlens(doi_target[["doi"]], doi_target[["orcid"]], rv) 
                  prog_doi_reactive <- reactive({ progress_state$doi_done + 1 })
                  # progress_state$scopus_done <- progress_state$scopus_done + 1
                  pct <- round(( isolate(prog_doi_reactive()) / max(1, rv$doi_count) ) * 100 )
                  shinyWidgets::updateProgressBar(
                    session, id = "prog_doi", value = isolate(prog_doi_reactive()), total = max(1, rv$doi_count),
                    title = sprintf("DOI: %d%% (%d/%d)", pct, isolate(prog_doi_reactive()), rv$doi_count),
                    status = if(pct == 100) "success" else "warning"
                  )
                  progress_state$doi_done <- isolate(prog_doi_reactive())
                  return(ret_df)
                }, error = function(e){ 
                  message(paste("ERROR (doi2gscholarlens()):", e))
                  warning(traceback()) })
              }, globals = c("glens_env", "doi_target", "doi2gscholarlens", "rv", "session", "progress_state"), packages = c("shinyWidgets", "stringi", "dplyr", "shiny"), seed = TRUE) %...>% (function(res_df) {
                # if (rv$is_cancelled) return(NULL)
                if(!fs::file_exists(file.path("run.lock"))) return(NULL)
                return(res_df)
              }) %...!% (function(err) {
                # if (rv$is_cancelled) return(NULL)
                if(!fs::file_exists(file.path("run.lock"))) return(NULL)
                progress_state$doi_done <- progress_state$doi_done + 1 
                shinyWidgets::updateProgressBar(session, id = "prog_doi", value = progress_state$doi_done , status = "danger", title = "Process Failed!")
                # output$log <- renderText(sprintf("Failed in DOI Processing: %s", conditionMessage(err)))
                rv$log_text <- paste(rv$log_text, sprintf("\nFailed in DOI Processing: %s", conditionMessage(err)),sep="<br>")
                warning(paste("Failed in DOI Processing:", err))
                message(traceback())
                shinyjs::delay(3000, shinyjs::hide("progress_overlay"))
                shinyjs::enable("submit_button")
              })
              
            })
            
            # RETURN the resolved DOI promises to `master_doi_promise`
            return(promise_all(.list = doi_promises))
          }else{
            return(doi_df)
          }
        })
      }else{
          # Fallback: if no ORCIDs were provided, resolve immediately to an empty list
          master_scopus_promise <- promise_resolve(list())
          master_scopusdf_promise <- master_scopusid_promise
          master_doi_promise <- promise_resolve(list())
      }
      
      promise_all(
        dois = master_doi_promise,
        scopus_orcid = master_scopus_promise,
        scopus_id = master_scopusdf_promise
      ) %...>% (function(results) {
        # if (rv$is_cancelled) return(NULL)
        if(!fs::file_exists(file.path("run.lock"))) return(NULL)
        
        failed_doi_count <- length(Filter(is.null, results$dois))
        rv$log_text <- paste(rv$log_text, paste("<span style='color: red;'>Failed RIS Extraction Count:", abs(rv$doi_count - failed_doi_count), "</span>"), sep="<br>")
        
        # Merge DOIs safely
        clean_dois <- Filter(Negate(is.null), results$dois)
        valid_dois <- Filter(is.data.frame, clean_dois)
        accumulated_df <- dplyr::bind_rows(valid_dois)
        if (nrow(accumulated_df) > 0) accumulated_df <- accumulated_df %>% dplyr::distinct() %>% dplyr::mutate(Source = "DOI/ORCID")
        
        clean_scopus_orcid <- Filter(Negate(is.null), results$scopus_orcid)
        clean_scopus_id <- Filter(Negate(is.null), results$scopus_id)
        rv$scopus_df <- dplyr::bind_rows(purrr::compact(Filter(is.data.frame, clean_scopus_orcid)), purrr::compact(Filter(is.data.frame, clean_scopus_id))) %>% dplyr::distinct()
        if(has_key_scopus){
          rv$log_text <- paste(rv$log_text, paste("SCOPUS (ORCiD) Rows:", length(clean_scopus_orcid)), sep="<br>")
          rv$log_text <- paste(rv$log_text, paste("SCOPUS (ID) Rows:", length(clean_scopus_id)), sep="<br>")
          rv$log_text <- paste(rv$log_text, paste("Total SCOPUS Rows:", nrow(rv$scopus_df)), sep="<br>")
        }
        # Merge Scopus
        # rv$scopus_df <- dplyr::bind_rows(purrr::compact(Filter(is.data.frame, clean_scopus)))
        if (nrow(rv$scopus_df) > 0) rv$scopus_df <- rv$scopus_df %>% dplyr::distinct() %>% dplyr::mutate(Source = "SCOPUS")
        intrm_tmp <- dplyr::bind_rows(accumulated_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)), rv$scopus_df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)))
        # Final Table
        
        if(nrow(rv$glens_full_table) > 0){
          rv$glens_full_table <- dplyr::bind_rows(rv$glens_full_table %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character)), intrm_tmp)
        }else{
          rv$glens_full_table <- intrm_tmp
        }
        
        rv$log_text <- paste(rv$log_text, sprintf("\nDone. Found %d total records.\n", nrow(intrm_tmp)))
        
        # output$dynamic_source_ui <- renderUI({
        #   req(rv$glens_input_table)
        #   req(nrow(rv$glens_input_table) > 0)
        #   available_sources <- levels(factor(rv$glens_input_table$Source))
        #   if (length(available_sources) == 0){ 
        #       return(p("No sources identified yet.", style = "color: #888;")) 
        #     }else{
        #       radioButtons("selected_source", label = NULL, choices = available_sources, selected = available_sources[1], inline = FALSE)
        #     }
        # })
        
        # --- SCRIPTS 2 & 3: STATS & PLOTTING ---
        target_variants <- stringr::str_trim(unlist(stringr::str_split(input$author_list, "\n")))
        target_variants <- target_variants[target_variants != ""]
        
        rv$target_variants_norm <- lapply(setNames(target_variants, target_variants), function(v) {
          vn <- normalize_name(v)
          list(norm = vn, parts = extract_parts(vn))
        })
        rv$author_match_regex <- build_name_regex_for_variants(target_variants)
        print(" rv$author_match_regex:")
        print(str(rv$author_match_regex))
        rv$glens_full_table <- extend_input_table(rv, rv$glens_full_table)
        # rv$glens_year_filtered <- rv$glens_etable_final
        if (nrow(rv$glens_full_table) <= 0) {
          rv$log_text <- paste(rv$log_text, "No keywords were matched.",sep="<br>")
          # shinyjs::enable("submit_button")
          # shinyjs::hide("progress_overlay")
          # # req(nrow(rv$glens_year_filtered) > 0)
        } else {
          print("HERE1.1")
          compute_indices(rv, rv$glens_full_table)
          
          # output$summary_table <- renderTable(rv$summary_table, striped = TRUE)
          # output$sh_index <- renderUI(HTML(paste("<b>Sh-Index:</b>", rv$sh_index)))
          # output$extended_table <- DT::renderDataTable({
          #   DT::datatable(rv$glens_year_filtered, options = list(scrollY = "600px", scrollX = TRUE, paging = TRUE))
          # })
          
          print(str(jcr_names_norm))
          # 1. Match Journals and create Qscore FIRST
          # rv$glens_year_filtered <- match_journals(rv, rv$glens_full_table)
          rv$glens_full_table <- match_journals(rv, rv$glens_full_table)
          rv$glens_etable_final <- rv$glens_full_table
          rv$glens_year_filtered <- rv$glens_etable_final
          shinyjs::show("extended_table")
        }
        
        # 3. Render the initial plots
        render_skeleton_plots(rv, output)
        
        plot_glens_table(rv, session)
        
        # 2. Update the Slider SECOND (now it's safe to trigger observers)
        if(length(na.omit(levels(factor(rv$glens_year_filtered$Year)))) > 1){
          min_year <- min(as.numeric(rv$glens_year_filtered$Year), na.rm = TRUE)
          max_year <- max(as.numeric(rv$glens_year_filtered$Year), na.rm = TRUE)
          if (is.finite(min_year) && is.finite(max_year)) {
            updateSliderInput(session, "year_slider", value = c(min_year, max_year), min = min_year, max = max_year)
            shinyjs::show("year_slider")
          }else{
            shinyjs::hide("year_slider")
          }
        }else{
          shinyjs::hide("year_slider")
        }
        # Render Full Data Network
        output$network_full <- renderVisNetwork({
          req(rv$glens_etable_final) # Ensure data exists
          req(nrow(rv$glens_etable_final) > 0)
          
          net_data <- build_collaboration_network(rv$glens_etable_final, rv$author_list)
          
          visNetwork(net_data$nodes, net_data$edges, width = "100%", height = "500px") %>%
            visNodes(font = list(size = 14)) %>%
            visEdges(color = list(color = "#cccccc", highlight = "#2c3e50"), smooth = TRUE) %>%
            visIgraphLayout(layout = "layout_with_fr") %>%
            visOptions(highlightNearest = list(enabled = TRUE, degree = 1), nodesIdSelection = TRUE) %>%
            addFontAwesome() 
        })
        shinyjs::show("network_full")
        
        fs::file_delete(file.path("run.lock"))
        rv$is_cancelled <- FALSE
        rv$is_glens_exec <- FALSE   
        shinyjs::delay(1500, shinyjs::hide("progress_overlay"))
        shinyjs::enable("submit_button")
        
      }) %...!% (function(err) {
        if(!fs::file_exists(file.path("run.lock"))) return(NULL)
        shinyWidgets::updateProgressBar(session, id = "prog_doi", value = 100, status = "danger", title = "Process Failed!")
        rv$log_text <- paste(rv$log_text, sprintf("\n(Master) Failed in DOI/Scopus Processing: %s", conditionMessage(err)),sep="<br>")
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
