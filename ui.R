# server.R (or inside server function)
require(shiny)
require(shinyjs)
require(promises)
require(future)
require(dplyr)
require(showtext)
require(systemfonts)
require(ggplot2)
require(plotly)
require(stringr)
require(stringi)
require(tibble)
require(scales)
require(stringdist)
require(future.apply)
require(tidyr)
require(DT)
require(sodium)
require(uuid)
require(openssl)
require(xfun)




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




/* The sticky container that floats at the bottom left */
.floating-log-container {
  position: fixed;           /* Detaches it from the page flow */
  bottom: 20px;              /* 20px spacing from the bottom edge */
  left: 20px;                /* 20px spacing from the left edge */
  width: 23%;                /* Matches roughly the width of your sidebar */
  min-width: 280px;          /* Prevents it from getting too squished on small screens */
  z-index: 9999;             /* Ensures it stays on top of other scrolling content */
  
  /* Styling to match your custom cards */
  background-color: white;
  box-shadow: 0 -4px 15px rgba(0,0,0,0.15); /* Stronger shadow so it pops off the background */
  border-radius: 10px;
  border-top: 6px solid #f39c12; /* Orange accent */
  padding: 15px;
}

/* The actual text output inside the container */
.floating-log-container pre#log {
  margin: 0;
  border: none;
  background-color: #f8f9fa; /* Light grey background for the text area */
  
  /* Scrollbar settings */
  max-height: 25vh;          /* Takes up a max of 25% of the screen height */
  overflow-y: auto;          /* Enables VERTICAL scrollbar when text overflows */
  overflow-x: hidden;        /* Hides horizontal scroll */
  
  /* Text wrapping */
  white-space: pre-wrap !important;
  word-wrap: break-word !important;
  font-size: 12px;
}




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


      
    "))
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
      style = "padding: 0;", 
      
      # Section 1: Search
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        h4("Search & Identification", style = "margin-top: 0; font-weight: bold; font-size: 16px;"),
        textAreaInput("doi_text", "DOI input:", value = "", rows = 2, width = "100%"),
        textAreaInput("author_list",  "Author Name List (seperated by |) *<required>:", value = "", rows = 2, width = "100%"),
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
      tags$div(
        class = "floating-log-container",
        
        # Log Header
        tags$div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
          h4("Log", style = "margin: 0; font-weight: bold; font-size: 16px;")
        ),
        
        # The Log Output
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
          column(6, plotlyOutput("acounts_plot")),
          column(6, plotlyOutput("ccounts_plot"))
        ),
        tags$br(),
        plotlyOutput("cdist_plot"),
        tags$br(),
        fluidRow(
          column(6, plotlyOutput("aperc_plot")),
          column(6, plotlyOutput("cperc_plot"))
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
            
            shinyWidgets::progressBar(
              id = "doi_progress",
              value = 0,
              total = 100,
              title = "Initializing...",
              status = "warning",
              striped = TRUE,
              size = "sm"
            ),
            
            # --- NEW: Cancel Button ---
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