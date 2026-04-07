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
suppressPackageStartupMessages(require(uuid))
suppressPackageStartupMessages(require(openssl))
suppressPackageStartupMessages(require(xfun))

is_WASM <- grepl(pattern="wasm",x=Sys.info()["machine"])
# # use a multisession plan so futures run in background R sessions
# if(!is_WASM){
#   future::plan(future::multisession)
future::plan(future::multicore)
# }else{
#   future::plan(future::sequential)
# }

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML("
      @font-face {
        font-family: 'schibsted-grotesk';
        src: url('fonts/SchibstedGrotesk.ttf') format('truetype');
      }
      *:not(.fa):not(.fas):not(.far) { 
          font-family: 'schibsted-grotesk', sans-serif !important; 
      }

      /* --- Theme Variables --- */
      :root {
        --bg-color: #f8f9fa;
        --card-bg: #ffffff;
        --text-color: #2c3e50;
      }
      .dark-mode {
        --bg-color: #121212;
        --card-bg: #1e1e1e;
        --text-color: #ffffff;
      }

      body { background-color: var(--bg-color) !important; color: var(--text-color) !important; transition: all 0.3s ease; }
      
      .custom-card { 
        background-color: var(--card-bg) !important; 
        color: var(--text-color) !important; 
        transition: background-color 0.3s ease;
      }

      /* --- TABLE PROTECTION --- */
      .dark-mode table, .dark-mode .table, .dark-mode td, .dark-mode th, .dark-mode .dataTables_wrapper {
        background-color: white !important; 
        color: #333333 !important;
      }
      
      /* --- HEADER ALIGNMENT --- */
      .header-container {
        display: flex;
        justify-content: space-between;
        align-items: center;
        padding: 15px 25px;
        background-color: #4B8BBE;
        color: white;
        margin-bottom: 20px;
        border-radius: 0 0 10px 10px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.2);
      }

      /* Button Grouping on the right */
      .header-buttons {
        display: flex;
        gap: 12px;
        align-items: center;
      }
      
      .btn-outline-white {
        background: rgba(255,255,255,0.15);
        border: 1px solid rgba(255,255,255,0.6);
        color: white;
        font-weight: 600;
        transition: all 0.2s;
      }
      
      .btn-outline-white:hover {
        background: rgba(255,255,255,0.3);
        border-color: white;
        color: white;
      }
      
 .status-badge {
  display: inline-block;
  padding: 2px 8px;
  border-radius: 4px;
  font-size: 11px;
  font-weight: bold;
  margin-left: 10px;
  vertical-align: middle;
}
.badge-missing { background-color: #e0e0e0; color: #757575; }
.badge-found { background-color: #d4edda; color: #155724; border: 1px solid #c3e6cb; }

/* Ensure the label and badge sit on the same line */
.label-container {
  display: flex;
  justify-content: space-between;
  align-items: center;
  width: 100%;
  margin-bottom: 8px;
}
      
      /* Vertical spacing for the whole row */
.api-row {
  margin-bottom: 25px;
  padding-bottom: 15px;
  border-bottom: 1px solid #eee;
}

/* Label styling to stay on top */
.api-row label {
  font-weight: bold;
  margin-bottom: 8px;
  display: block;
}

/* The magic grouping container */
.input-button-group {
  display: flex;
  flex-direction: row;
  align-items: center; /* Centers items vertically relative to each other */
  gap: 10px;
}

/* Remove Shiny's default bottom margin from the input within the group */
.input-button-group .form-group {
  margin-bottom: 0 !important;
  flex-grow: 1;
}

/* Fixed width for buttons to ensure text fits and alignment is consistent */
.api-save-wrap {
  flex: 0 0 240px; 
}

.save-btn-custom {
  width: 100%;
  height: 38px; /* Standard Bootstrap input height */
  font-weight: 600;
  white-space: nowrap;
  padding: 6px 12px;
}

/* Styling for the new Source radio buttons */
#dynamic_source_ui .shiny-options-group {
  margin-top: 10px;
}

#dynamic_source_ui label {
  font-weight: normal;
  cursor: pointer;
  padding: 5px 0;
  display: block;
}

/* Optional: Make the radio button labels change color on hover */
#dynamic_source_ui label:hover {
  color: #3498db;
}

/* The container for both the handle and the log */
/* --- 1. LEFT SIDEBAR (Now stretches naturally with page) --- */
.left-sidebar-col {
  height: auto !important;   /* Changed from 100vh */
  min-height: 100vh;         /* Ensures it at least fills the screen */
  display: flex;
  flex-direction: column;
  padding-bottom: 150px;     /* Extra space at bottom so log doesn't cover last inputs */
  overflow: visible !important; /* Removes the inner scrollbar */
}

/* --- 2. LOG WRAPPER (Stays fixed to viewport, not sidebar) --- */
#log_wrapper {
  position: fixed !important; 
  bottom: 0 !important; 
  /* Z-index high enough to beat the progress overlay (usually 1050 in Bootstrap) */
  z-index: 10000 !important; 
  background-color: #ffffff !important;
  border: 1px solid #e3e3e3 !important;
  border-radius: 8px 8px 0 0 !important;
  box-shadow: 0 -4px 15px rgba(0,0,0,0.1) !important;
  display: flex;
  flex-direction: column;
  overflow: hidden !important; 
}

/* --- 3. PANEL HEADER (Drag Handle) --- */
#log_header {
  padding: 10px 15px;
  background-color: #f5f5f5;
  border-bottom: 1px solid #e3e3e3;
  display: flex;
  justify-content: space-between;
  align-items: center;
  cursor: ns-resize;
  user-select: none;
  margin: 0 !important;
}
.drag-grip {
  width: 25px;
  height: 2px;
  background-color: #aaa;
  box-shadow: 0 5px 0 #aaa, 0 -5px 0 #aaa;
}

/* --- 4. TEXT OUTPUT (Scrollable, size bounded) --- */
#log {
  width: 100% !important;
  max-height: 85vh !important;
  min-height: 30px !important;
  overflow-y: auto !important;  
  white-space: pre-wrap !important;
  word-wrap: break-word !important;
  font-family: monospace;
  font-size: 12px;
  border: none !important;
  margin: 0 !important;
}
#log.recalculating { opacity: 1 !important; }

/* --- FIX 2: Radio Button Alignment --- */
#dynamic_source_ui .shiny-options-group {
  display: flex;
  flex-direction: column;
  gap: 10px; /* Clean spacing between the options */
  margin-top: 5px;
}

#dynamic_source_ui .radio {
  margin: 0; /* Remove Shiny's default block margins */
}

/* Flexbox alignment locks the circle and text on the same horizontal axis */
#dynamic_source_ui .radio label {
  display: flex !important;
  align-items: center; 
  gap: 8px; /* Space between the radio circle and the text */
  margin: 0;
  padding: 0;
  font-weight: normal;
  cursor: pointer;
}

/* Reset the actual input circle so it sits normally */
#dynamic_source_ui .radio input[type='radio'] {
  margin: 0 !important;
  position: static !important; /* Overrides Shiny's default absolute positioning */
}


/* --- Minimal Table Styling --- */
.minimal-table table {
  width: 100%;
  border-collapse: collapse;
  margin-bottom: 20px;
  font-family: inherit;
}

.minimal-table th {
  background-color: #f8f9fa;  /* Very light grey header */
  color: #4a5568;             /* Soft dark grey text */
  font-weight: 600;
  font-size: 13px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  padding: 12px 15px;
  border-bottom: 2px solid #e2e8f0;
  text-align: left;
}

.minimal-table td {
  padding: 12px 15px;
  border-bottom: 1px solid #edf2f7; /* Very subtle row dividers */
  color: #2d3748;
  font-size: 14px;
}

/* Subtle hover effect for table rows */
.minimal-table tbody tr:hover {
  background-color: #f7fafc;
}

/* --- SH-Index Badge Styling --- */
.sh-index-container {
  display: inline-flex;
  align-items: baseline;
  background-color: #f0f7ff; /* Soft blue background to match your theme */
  padding: 12px 20px;
  border-radius: 8px;
  border: 1px solid #cce4fc;
  box-shadow: 0 2px 4px rgba(75, 139, 190, 0.05);
  margin-top: 10px;
}

.sh-index-label {
  color: #4a5568;
  font-size: 14px;
  font-weight: 600;
  margin-right: 10px;
}

.sh-index-value {
  color: #4B8BBE; /* Your primary blue */
  font-size: 24px;
  font-weight: 800;
}


/* --- CUSTOM INSIDE-TEXT PROGRESS BARS --- */

# /* 1. Force the wrapper to act as an anchor */
# #progress_bars_container .progress-group {
#   position: relative !important;
#   margin-bottom: 20px; 
# }
# 
# /* 2. Position the title text dead-center inside the bar */
# #progress_bars_container .progress-text {
#   position: absolute !important;
#   width: 100%;
#   text-align: center;
#   top: 0;
#   left: 0;
#   z-index: 10;
#   line-height: 24px; /* Matches the bar height below to perfectly center it vertically */
#   font-size: 13px;
#   font-weight: bold;
#   color: #2c3e50;
#   margin: 0;
#   
#   /* Adds a heavy white glow so the text remains readable even when the dark colored bar passes behind it */
#   text-shadow: 0px 0px 4px white, 0px 0px 4px white, 0px 0px 6px white;
#   pointer-events: none; /* Prevents the text from interfering with hover states */
# }
# 
# /* 3. Make the bar background thick enough to hold the text */
# #progress_bars_container .progress {
#   height: 24px !important; 
#   border-radius: 12px; /* Smooth rounded pill edges */
#   background-color: #e9ecef;
#   margin-bottom: 0;
# }
# 
# /* 4. Hide the default '0/100' numbers on the far right since you put the % in the title */
# #progress_bars_container .progress-number {
#   display: none !important;
# }

.api-progress-wrapper {
  margin-bottom: 15px; 
}

/* 2. Static label styling (above the bar) */
.api-progress-label {
  font-weight: 600;
  color: #2c3e50;
  font-size: 13px;
  margin-bottom: 4px;
  display: block;
}

/* 3. Force the wrapper to act as an anchor */
#progress_bars_container .progress-group {
  position: relative !important;
  margin-bottom: 0px !important; 
}

/* 4. Position the dynamic text inside the bar */
#progress_bars_container .progress-text {
  position: absolute !important;
  width: 100%;
  text-align: center;
  top: 0;
  left: 0;
  z-index: 10;
  line-height: 20px; /* Aligns vertically with the bar height */
  font-size: 12px;
  font-weight: 200;
  color: #2c3e50; /* Standard dark text, no glow */
  margin: 0;
  pointer-events: none;
}

/* 5. Bar background sizing */
#progress_bars_container .progress {
  height: 20px !important; 
  border-radius: 10px; 
  background-color: #e9ecef;
  margin-bottom: 0;
}

/* 6. Hide default right-aligned numbers */
#progress_bars_container .progress-number {
  display: none !important;
}

      
    ")),
    tags$script(HTML("
    $(function() {
        const log = document.getElementById('log');
        const handle = document.getElementById('log_header');
        const logWrapper = document.getElementById('log_wrapper');
        const sidebar = document.querySelector('.left-sidebar-col');
        
        let isResizing = false;
        let startY, startHeight;
    
        // 1. RESIZE LOGIC
        if(handle && log) {
          handle.addEventListener('mousedown', function(e) {
            isResizing = true;
            startY = e.clientY;
            startHeight = log.getBoundingClientRect().height;
            document.body.style.cursor = 'ns-resize';
          });

          window.addEventListener('mousemove', function(e) {
            if (!isResizing) return;
            const newHeight = startHeight + (startY - e.clientY);
            const maxH = window.innerHeight * 0.85; 
            log.style.height = Math.min(newHeight, maxH) + 'px';
          });

          window.addEventListener('mouseup', function() {
            isResizing = false;
            document.body.style.cursor = '';
          });
        }

        // 2. POSITION SYNC (Horizontal only)
        if (logWrapper && sidebar) {
          const syncPosition = function() {
            const rect = sidebar.getBoundingClientRect();
            // We match the sidebar's left position and width
            logWrapper.style.left = rect.left + 'px';
            logWrapper.style.width = rect.width + 'px';
          };

          // Watch for sidebar size changes (e.g. window resizing)
          const observer = new ResizeObserver(syncPosition);
          observer.observe(sidebar);
          
          // Initial sync
          syncPosition();
          
          // Also sync on scroll to handle any weird layout shifts
          window.addEventListener('scroll', syncPosition);
          window.addEventListener('resize', syncPosition);
        }
        
        // 3. AUTO-SCROLL ON NEW LOG MESSAGE
        $(document).on('shiny:value', function(event) {
          if (event.name === 'log') { 
            setTimeout(function() {
              if (log) log.scrollTop = log.scrollHeight; 
            }, 10); 
          }
        });
      });
    ")),
  ),
  
  # 1. Custom Title Header with Settings & Dark Mode
  tags$div(
    class = "header-container",
    h2("GScholarLENS Analysis Dashboard", style = "margin: 0; font-weight: bold;"),
    tags$div(
      class = "header-buttons",
      actionButton("settings_btn", "", icon = icon("gear", lib = "font-awesome"), class = "btn-outline-white"),
      actionButton("theme_toggle", "🌙 Dark Mode", icon = icon("moon", lib = "font-awesome"), class = "btn-outline-white")
    )
  ),
  
  # 2. Main Content Grid
  fluidRow(
    style = "margin: 0;", 
    column(
      width = 3,
      class = "left-sidebar-col",
      style = "padding: 0;", 
      
      # Section 1: Search
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        h4("Search & Identification", style = "margin-top: 0; font-weight: bold; font-size: 16px;"),
        textAreaInput("doi_text", "DOI input:", value = "", rows = 2, width = "100%"),
        textAreaInput("author_list",  "Author Name List :", value = "", rows = 2, width = "100%"),
        uiOutput("dynamic_author_filter"),
        textAreaInput("orcid_text", "ORCID input:", value = "", rows = 2, width = "100%"),
        actionButton("submit_button", "Run GScholarLENS for DOI", class = "btn-primary", style = "width: 100%; font-weight: bold; margin-top: 10px; background-color: #4B8BBE; border: none;")
      ),
      # Section 2: Timeline
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        h4("Filter by Timeline", style = "margin-top: 0; font-weight: bold; font-size: 16px;"),
        shinyjs::hidden(sliderInput("year_slider", "Publication Years", min = 0, max = 0, value = c(0, 0), step = 1, round = TRUE, width = "100%"))
      ),
      
      # Section 3: Source
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        h4("Source", style = "margin-top: 0; font-weight: bold; font-size: 16px;"),
        uiOutput("dynamic_source_ui") 
      ),
      
      # Section 4: Log Output 
      tags$div(id = "log_wrapper",
               # A proper panel header that ALSO acts as your drag handle
               tags$div(id = "log_header",
                        tags$span("Execution Log", style = "font-weight: bold; color: #333;"),
                        tags$div(class = "drag-grip") # The visual drag lines
               ),
               # The actual log output
               verbatimTextOutput("log")
      )
    ),
    
    column(
      width = 9,
      # 3) Impact Metrics
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        h3("Author Impact Metrics", style = "color: #4B8BBE; margin-top: 0; font-weight: bold;"),
        hr(style = "border-top: 1px solid #edf2f7; margin-bottom: 20px;"), # Softened the hr() line
        
        fluidRow(
          # Wrapped in minimal-table
          column(6, tags$div(class = "minimal-table", tableOutput("orcid_table"))),
          
          # Vertically centering the SH-Index next to the table
          column(6, 
                 style = "display: flex; align-items: center; justify-content: flex-start; height: 100%; min-height: 80px;", 
                 shinyjs::disabled(shiny::uiOutput("sh_index")))
        ),
        
        # Wrapped in minimal-table
        tags$div(class = "minimal-table", tableOutput("summary_table"))
      ),
      
      # 4) Visualizations
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #2E8B57; margin-bottom: 25px;",
        h3("Publication & Citation Trends", style = "color: #2E8B57; margin-top: 0; font-weight: bold;"),
        hr(),
        fluidRow(
          column(6, plotly::plotlyOutput("acounts_plot")),
          column(6, plotly::plotlyOutput("ccounts_plot"))
        ),
        tags$br(),
        plotly::plotlyOutput("cdist_plot"),
        tags$br(),
        fluidRow(
          column(6, plotly::plotlyOutput("aperc_plot")),
          column(6, plotly::plotlyOutput("cperc_plot"))
        )
      ),
      
      # 5) Detailed Publication Record
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #D6A77A; margin-bottom: 25px;",
        h3("Detailed Publication Record", style = "color: #D6A77A; margin-top: 0; font-weight: bold;"),
        hr(),
        DT::DTOutput("extended_table")
      ),
      
      tags$div(
        id = "progress_overlay",
        style = "display: none;", # Hidden until processing starts
        
        tags$div(
          style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; 
             background-color: rgba(0, 0, 0, 0.6); z-index: 9999; 
             display: flex; justify-content: center; align-items: center;",
          
          tags$div(
            class = "custom-card",
            style = "background: white; padding: 40px; border-radius: 12px; 
               box-shadow: 0 10px 25px rgba(0,0,0,0.5); width: 40%; min-width: 400px;
               text-align: center; border-top: 6px solid #f39c12;",
            
            h3("Processing Data", style = "margin-top: 0; color: #2c3e50; font-weight: bold;"),
            p("Please wait while GScholarLENS fetches and analyzes the records. This may take a moment.", 
              style = "color: #7f8c8d; margin-bottom: 25px;"),
            
            tags$div(
              id = "progress_bars_container",
              style = "text-align: left; margin-bottom: 25px;",
              
              tags$div(class = "api-progress-wrapper",
                       tags$label("DOI / ORCID Resolver", class = "api-progress-label"),
                       shinyWidgets::progressBar(id = "prog_doi", title = "0%", value = 0, total = 100, status = "warning")
              ),
              tags$div(class = "api-progress-wrapper",
                       tags$label("Scopus API", class = "api-progress-label"),
                       shinyWidgets::progressBar(id = "prog_scopus", title = "0%", value = 0, total = 100, status = "info")
              ),
              tags$div(class = "api-progress-wrapper",
                       tags$label("Web of Science API", class = "api-progress-label"),
                       shinyWidgets::progressBar(id = "prog_wos", title = "0%", value = 0, total = 100, status = "primary")
              ),
              tags$div(class = "api-progress-wrapper",
                       tags$label("Semantic Scholar API", class = "api-progress-label"),
                       shinyWidgets::progressBar(id = "prog_semantic", title = "0%", value = 0, total = 100, status = "success")
              )
            ),
            
            # --- Cancel Button ---
            actionButton(
              inputId = "cancel_button", 
              label = "Cancel Processing", 
              icon = icon("times"),
              class = "btn-danger", # Makes it red
              style = "margin-top: 20px; width: 50%; border-radius: 20px;"
            )
          )
        )
      )
    )
  )
)
