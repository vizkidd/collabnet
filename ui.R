# server.R (or inside server function)
suppressPackageStartupMessages(require(shiny))
suppressPackageStartupMessages(require(shinyjs))
suppressPackageStartupMessages(require(shinyWidgets))
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
suppressPackageStartupMessages(require(visNetwork))
suppressPackageStartupMessages(require(bslib))

is_WASM <- grepl(pattern="wasm",x=Sys.info()["machine"])
# # use a multisession plan so futures run in background R sessions
# if(!is_WASM){
  # future::plan(future::multisession)
# future::plan(future.callr::callr)
# future::plan(future::multicore)
# }else{
#   future::plan(future::sequential)
# }

ui <- fluidPage(
  shinyjs::useShinyjs(),
  # theme = bs_theme(version = 5),
  # 1. The unconditional warning wall
  tags$div(
    id = "js-dependency-warning",
    style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background-color: rgba(30, 30, 30, 0.98); color: #ff9800; z-index: 9999999; display: flex; flex-direction: column; align-items: center; justify-content: center; font-family: sans-serif; text-align: center; padding: 20px;",
    tags$h1(style = "font-size: 3em; margin-bottom: 20px;", "JavaScript is Disabled"),
    tags$p(style = "font-size: 1.5em; color: white;", "CollabNET requires JavaScript to function properly."),
    tags$p(style = "font-size: 1.2em; color: #ccc;", "Please enable JavaScript in your browser settings and refresh the page.")
  ),
  
  # 2. The script that destroys the wall if JS is enabled
  tags$script(HTML("
    document.addEventListener('DOMContentLoaded', function() {
      var warning = document.getElementById('js-dependency-warning');
      if (warning) {
        warning.style.display = 'none';
      }
    });
  ")),
  tags$head(
    tags$noscript(
      HTML("
        <style>
          #noscript-warning {
            position: fixed;
            top: 0; 
            left: 0; 
            width: 100%; 
            height: 100%;
            background-color: rgba(0, 0, 0, 0.95);
            color: #ff9800;
            z-index: 999999;
            display: flex;
            flex-direction: column;
            align-items: center;
            justify-content: center;
            font-family: Arial, sans-serif;
            text-align: center;
            padding: 20px;
          }
          #noscript-warning h1 {
            font-size: 2.5em;
            margin-bottom: 20px;
          }
          #noscript-warning p {
            font-size: 1.2em;
            color: #ffffff;
          }
        </style>
        <div id='noscript-warning'>
          <h1>JavaScript is Disabled</h1>
          <p>This Shiny application relies heavily on JavaScript for interactivity and server communication.</p>
          <p>Please enable JavaScript in your browser settings and refresh this page to continue.</p>
        </div>
      ")
    ),
    tags$script(src = "https://cdn.jsdelivr.net/npm/plotly.js-dist/plotly.min.js"),
    tags$script(HTML("
      $(document).ready(function(){
        // Initialize all Bootstrap 3 tooltips on the page
        $('[data-toggle=\"tooltip\"]').tooltip({
          placement: 'top', // You can change this to 'right', 'bottom', or 'left'
          container: 'body' // Prevents the tooltip from breaking inside hidden divs
        });
      });
    ")),
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
     $(document).ready(function() {
      // Toggle popup on icon click
      $(document).on('click', '.help-icon', function(e) {
        e.preventDefault(); // Prevents input focus when clicking the icon
        e.stopPropagation();
        // Hide all other popups first
        $('.api-help-content').not($(this).next('.api-help-content')).hide();
        // Toggle the one we just clicked
        $(this).next('.api-help-content').toggle();
      });
  
      // Close popup if clicking anywhere outside the container
      $(document).on('click', function(e) {
        if ($(e.target).closest('.api-help-container').length === 0) {
          $('.api-help-content').hide();
        }
      });
    });
    
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
        
        // LOCAL API KEY STORAGE IN BROWSER using localStorage
        // 1. When Shiny connects, check localStorage and send keys to R
        $(document).on('shiny:connected', function() {
          Shiny.setInputValue('browser_stored_keys', {
            scopus_key: localStorage.getItem('scopus_key'),
            wos_key: localStorage.getItem('wos_key'),
            semantic_key: localStorage.getItem('semantic_key'),
            crossref_key: localStorage.getItem('crossref_key'),
            opencites_key: localStorage.getItem('opencites_key')
          }, {priority: 'event'}); // priority: 'event' ensures it fires immediately
        });
    
        // 2. Listen for a command from R to save a new key
        Shiny.addCustomMessageHandler('save_key_to_browser', function(message) {
          localStorage.setItem(message.platform, message.key);
          // Optional: Give the user a tiny visual confirmation via JS
          console.log('Saved ' + message.platform + ' key to browser.');
        });
    
      });
    ")),
    tags$script(HTML("
      $(document).ready(function() {
        $(document).on('click', '.custom-card-header', function(e) {
          // Prevent collapse if the user is explicitly interacting with the checkbox
          if ($(e.target).closest('.shiny-input-container, input[type=\"checkbox\"]').length) {
            return;
          }
    
          var header = $(this);
          var card = header.closest('.custom-card');
          var content = card.find('.custom-card-content');
          var button = header.find('.btn-minimize');
    
          content.slideToggle(300, function() {
            if (content.is(':visible')) {
              button.html('&minus;');
            } else {
              button.html('&#43;'); // Plus symbol
            }
          });
        });
      });
    ")),
    tags$script(HTML("
    window.fetchAllCitationCounts = async function(doi, sources, keys, inputId, requestId) {
      try {
        let reqIndex = 0;
        if (requestId) {
          let parts = requestId.split('_');
          reqIndex = parseInt(parts[parts.length - 1]) || 0;
        }
        await new Promise(r => setTimeout(r, reqIndex * 400)); 

        const safeDoi = encodeURI(doi).replace(/#/g, '%23');
        let results = {};

        const fetchSource = async (apiName) => {
          let url, headers = { 'Accept': 'application/json' };
          if (apiName === 'crossref') {
            url = `https://api.crossref.org/works/${safeDoi}`;
            if (keys.crossref) headers['Crossref-Plus-API-Token'] = keys.crossref;
          } else if (apiName === 'opencitations') {
            url = `https://api.opencitations.net/index/v1/citation-count/${safeDoi}`;
            if (keys.opencitations) headers['authorization'] = keys.opencitations;
          } else if (apiName === 'semanticscholar') {
            url = `https://api.semanticscholar.org/graph/v1/paper/DOI:${safeDoi}?fields=citationCount`;
            if (keys.semanticscholar) headers['Authorization'] = `Bearer ${keys.semanticscholar}`;
          }

          let count = null, maxRetries = 3, waitTime = 2000;
          for (let i = 0; i < maxRetries; i++) {
            try {
              let res = await fetch(url, { headers });
              if (res.ok) {
                let data = await res.json();
                if (apiName === 'crossref') count = data.message?.['is-referenced-by-count'];
                else if (apiName === 'opencitations') count = Array.isArray(data) ? data[0]?.count : data?.count;
                else if (apiName === 'semanticscholar') count = data.citationCount;
                break; 
              }
              if (res.status === 429) {
                let retryAfter = res.headers.get('retry-after');
                waitTime = retryAfter ? (parseInt(retryAfter) + 1) * 1000 : waitTime * 1.5;
              } else if (res.status >= 400 && res.status < 500) { break; }
            } catch (e) { console.warn(`Fetch error for ${apiName}`); }
            if (i < maxRetries - 1 && count === null) await new Promise(r => setTimeout(r, waitTime));
          }
          return count !== null ? parseInt(count) : null;
        };

        const promises = sources.map(async (source) => { results[source] = await fetchSource(source); });
        await Promise.all(promises);

        Shiny.setInputValue(inputId, { doi: doi, counts: results, requestId: requestId }, {priority: 'event'});
        
      } catch (err) {
        console.error('Critical Failure on DOI:', doi, err);
        // THE SAFETY NET: ALWAYS return to R so the pipeline doesn't hang!
        Shiny.setInputValue(inputId, { doi: doi, counts: {}, requestId: requestId }, {priority: 'event'});
      }
    };
  ")),
    tags$script(HTML("
      window.fetchOpenAlexJournals = async function(journals, apiKey, mailKey, inputId) {
        try {
          let results = [];
          const total = journals.length;
          
          for (let i = 0; i < total; i++) {
            const journal = journals[i];
            const safeQuery = encodeURIComponent(journal);
            
            // NOTE: Replace the email below with your actual email!
            let url = `https://api.openalex.org/sources?search=${safeQuery}&select=display_name,summary_stats&mailto=${mailKey}`;
            if (apiKey && apiKey !== 'null') url += `&api_key=${apiKey}`;
            
            //console.log(url)
            
            let success = false;
            let retries = 0;
            const maxRetries = 3;
            
            while (!success && retries < maxRetries) {
              try {
                let res = await fetch(url);
                
                // 1. Check for explicit Rate Limit (429)
                if (res.status === 429) {
                  let retryAfter = res.headers.get('Retry-After');
                  // 'Retry-After' is usually in seconds. Fallback to 2000ms if missing.
                  let waitTime = retryAfter ? parseInt(retryAfter) * 1000 : 2000;
                  
                  console.warn(`[429 Rate Limit] Waiting ${waitTime}ms before retrying ${journal}...`);
                  await new Promise(r => setTimeout(r, waitTime));
                  retries++;
                  continue; // Loop again
                }
                
                // 2. Handle successful response
                if (res.ok) {
                  let data = await res.json();
                  if (data.results && data.results.length > 0) {
                    let bestMatch = data.results[0];
                    let title = bestMatch.display_name || journal;
                    
                    let jifValue = 0;
                    if (bestMatch.summary_stats && bestMatch.summary_stats['2yr_mean_citedness']) {
                      jifValue = parseFloat(bestMatch.summary_stats['2yr_mean_citedness']);
                    }
                    
                    let fetched_q = 'NA';
                    if (jifValue >= 4.0) fetched_q = 'Q1';
                    else if (jifValue >= 2.0) fetched_q = 'Q2';
                    else if (jifValue >= 0.75) fetched_q = 'Q3';
                    else if (jifValue > 0.0) fetched_q = 'Q4';
                    
                    results.push({ Name_norm: journal, JCR_Journal: title, Qscore: fetched_q, JIF5Years: jifValue.toFixed(2) });
                  } else {
                    results.push({ Name_norm: journal, JCR_Journal: journal, Qscore: 'NA', JIF5Years: '0' });
                  }
                  success = true; // Break the while loop
                } 
                // 3. Handle other server errors (404, 500) - Do not retry
                else {
                  console.warn(`Server returned ${res.status} for: ${journal}`);
                  results.push({ Name_norm: journal, JCR_Journal: journal, Qscore: 'NA', JIF5Years: '0' });
                  success = true; 
                }
                
              } catch (e) {
                // 4. Handle Disguised 429s (CORS errors)
                // If OpenAlex drops CORS headers on a 429, fetch() throws a TypeError.
                console.warn(`Network/CORS error for ${journal}. Assuming rate limit and backing off...`);
                
                let backoffTime = 2000 * Math.pow(2, retries); // 2s, 4s, 8s
                await new Promise(r => setTimeout(r, backoffTime));
                retries++;
              }
            }
            
            // If we completely exhausted our 3 retries
            if (!success) {
               console.error(`Failed to fetch ${journal} after ${maxRetries} retries.`);
               results.push({ Name_norm: journal, JCR_Journal: journal, Qscore: 'NA', JIF5Years: '0' });
            }
            
            // Update R progress bar
            let pct = Math.round(((i + 1) / total) * 100);
            Shiny.setInputValue(inputId + '_progress', pct, {priority: 'event'});
            
            // Keep a baseline 100ms delay to prevent triggering 429s in the first place
            await new Promise(r => setTimeout(r, 100));
          }
          
          // Send final payload
          Shiny.setInputValue(inputId, JSON.stringify(results), {priority: 'event'});
          
        } catch (err) {
          console.error('Critical OpenAlex JS Error:', err);
          Shiny.setInputValue(inputId, JSON.stringify([]), {priority: 'event'});
        }
      };
    ")),
    tags$style(HTML("
    /* Color the entire unselected background track */
    #edge-slider-wrap .irs-line {
      background: linear-gradient(to right, #2ECC71 0%, #F1C40F 50%, #E74C3C 100%) !important;
      border: none !important;
      height: 10px !important;
      border-radius: 4px !important;
    }
    /* Make the active selection window a completely clear highlighter frame */
    #edge-slider-wrap .irs-bar {
      background: rgba(255, 255, 255, 0.15) !important; 
      border: 2px solid #ffffff !important;             
      height: 10px !important;
      top: 24px !important;
    }
  "))
  ),
  
  # 1. Custom Title Header with Settings & Dark Mode
  tags$div(
    class = "header-container",
    h2("CollabNET Analysis Dashboard", style = "margin: 0; font-weight: bold;"),
    tags$div(
      class = "header-buttons",
      # tags$label(
      #   class = "btn btn-default btn-outline-white action-button",
      #   style = "margin-bottom: 0; font-weight: normal; cursor: pointer;",
      #   tags$i(id = "preupload_icon", class = "fa fa-upload"),
      #   # tags$span(id = "upload_icon", icon("upload", lib = "font-awesome")),
      #   tags$span(id = "preupload_text", ""), 
      #   tags$input(
      #     id = "preupload_btn",
      #     type = "button",
      #     style = "display: none;",
      #     onchange = "
      #       document.getElementById('preupload_text').innerText = ' Uploading...';
      #       document.getElementById('preupload_icon').className = 'fa fa-spinner fa-spin';
      #     "
      #   )
      #   # tags$input(
      #   #   id = "upload_btn",
      #   #   type = "file",
      #   #   multiple = FALSE,
      #   #   style = "display: none;",
      #   #   onchange = "
      #   #     document.getElementById('upload_text').innerText = ' Uploading...';
      #   #     document.getElementById('upload_icon').className = 'fa fa-spinner fa-spin';
      #   #   "
      #   # )
      # ),
      tags$button(
        id = "preupload_btn",
        type = "button",
        class = "btn btn-default btn-outline-white action-button", 
        style = "margin-bottom: 0; font-weight: normal; cursor: pointer;",
        tags$i(id = "preupload_icon", class = "fa fa-upload"),
        tags$span(id = "preupload_text", "") 
      ),
      tags$label(
        class = "btn btn-default btn-outline-white",
        style = "margin-bottom: 0; font-weight: normal; cursor: pointer;",
        icon("download", lib = "font-awesome"),
        tags$input(id="predownload_btn", type="button", class="action-button", style = "display: none;")
        # downloadButton(
        #   outputId = "download_btn",
        #   label = "", 
        #   icon = icon("download", lib = "font-awesome"),
        #   class = "btn-default btn-outline-white",
        #   style = "margin-bottom: 0; font-weight: normal; cursor: pointer;"
        # )
      ),
      tags$label(
        class = "btn btn-default btn-outline-white",
        style = "margin-bottom: 0; font-weight: normal; cursor: pointer;",
        icon("key", lib = "font-awesome"),
        tags$input(id="keys_btn", type="button", class="action-button", style = "display: none;")
      ),
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
      
      # Section 1: Fetch
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        tags$div(
          class = "custom-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; cursor: pointer; user-select: none; margin-bottom: 0;",
          h4("Fetch", style = "margin: 0; font-weight: bold; font-size: 16px;"),
          tags$button(
            type = "button", 
            class = "btn-minimize", 
            style = "background: transparent; border: none; font-size: 1.6em; line-height: 1; cursor: pointer; padding: 0 5px;",
            HTML("&plus;")
          ),
        ),
        tags$div(
          class = "custom-card-content",
          # style = "margin-top: 15px;", 
          style = "display: none;",
          tags$br(),
          div(
            style = "display: inline-flex; align-items: center; gap: 5px;",
            tags$b("DOI input:"),
            icon(
              "circle-question",
              "data-toggle" = "tooltip",
              style = "color: #007bc2; cursor: help;",
              title = "Type the DOI(s) here line-by-line and run CollabNET. Auto-refresh does NOT apply to DOI(s)."
              )
          ),
          textAreaInput("doi_text", value = "", rows = 2, width = "100%", label = NULL),
          div(
            style = "display: inline-flex; align-items: center; gap: 5px;",
            tags$b("ORCiD input:"),
            icon("circle-question",
                 "data-toggle" = "tooltip",
                 style = "color: #007bc2; cursor: help;",
                 title = "Type the ORCiD(s) here line-by-line and run CollabNET. Auto-refresh does NOT apply to ORCiD(s)."
                 )
          ),
          textAreaInput("orcid_text", value = "", rows = 2, width = "100%", label = NULL),
          shinyjs::hidden(div(
            id = "scopusid_label_wrapper", 
            style = "display: inline-flex; align-items: center; gap: 5px;",
            tags$b("SCOPUS IDs:"),
            icon(
              "circle-question",
              "data-toggle" = "tooltip",
              style = "color: #007bc2; cursor: help;",
              title = "Type the SCOPUS Author ID(s) here line-by-line and run CollabNET. Auto-refresh does NOT apply to SCOPUS ID(s)."
              )
          )),
          shinyjs::hidden(textAreaInput("scopusid_text", value = "", rows = 2, width = "100%", label = NULL)),
          actionButton("submit_button", "Run CollabNET", icon = icon("play", lib = "font-awesome"), class = "btn-primary", style = "width: 100%; font-weight: bold; margin-top: 10px; background-color: #4B8BBE; border: none; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
        )
      ),
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        tags$div(
          class = "custom-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; cursor: pointer; user-select: none; margin-bottom: 0;",
          h4("Lookup Controls", style = "margin: 0; font-weight: bold; font-size: 16px;"),
          tags$button(
            type = "button", 
            class = "btn-minimize", 
            style = "background: transparent; border: none; font-size: 1.6em; line-height: 1; cursor: pointer; padding: 0 5px;",
            HTML("&plus;")
          ),
        ),
        tags$div(
          class = "custom-card-content",
          # style = "margin-top: 15px;", 
          style = "display: none;",
          tags$br(),
          div(
            style = "display: inline-flex; align-items: center; gap: 5px;",
            tags$b("Lookup Keywords:"),
            icon(
              "circle-question",
              "data-toggle" = "tooltip",
              style = "color: #007bc2; cursor: help;",
              title = "Type the lookup-keywords here line-by-line. Keywords are matched in-order."
            )
          ),
          textAreaInput("author_list_text",  value = "", rows = 2, width = "100%", label = NULL),
          tags$script(HTML("
            $(document).on('blur', '#author_list_text', function() {
              // Creates a new reactive input called 'input$author_list'
              Shiny.setInputValue('author_list', $(this).val());
            });
          ")),
          uiOutput("dynamic_author_filter"),
          # The Toggle Lock Button
          actionButton("toggle_extended", "Show Lookup Controls", icon = icon("magnifying-glass"), 
                       class = "btn-secondary", style = "margin-top: 10px; width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"),
          shinyjs::hidden(
            tags$div(id = "extended_controls_container",
                     uiOutput("lookup_controls_panel")
            )
          )
        )
      ),
      # tags$div(
      #   class = "custom-card",
      #   style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
      #   h4("Extended Controls", style = "margin-top: 0; font-weight: bold; font-size: 16px;"),
      #   # The container for the extended controls
      #   uiOutput("lookup_controls_panel")
      # ),
      # Section 2: Timeline
      shinyjs::hidden(tags$div(
        id = "timeline_card",
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        tags$div(
          class = "custom-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; cursor: pointer; user-select: none; margin-bottom: 0;",
          h4("Timeline", style = "margin: 0; font-weight: bold; font-size: 16px;"),
          tags$button(
            type = "button", 
            class = "btn-minimize", 
            style = "background: transparent; border: none; font-size: 1.6em; line-height: 1; cursor: pointer; padding: 0 5px;",
            HTML("&plus;")
          ),
        ),
        tags$div(
          class = "custom-card-content",
          # style = "margin-top: 15px;", 
          style = "display: none;",
          tags$br(),
          shinyjs::hidden(sliderInput("year_slider", "Publication Years", min = 0, max = 0, value = c(0, 0), step = 1, round = TRUE, width = "100%"))
        )
      )),
      
      # Section 3: Source
      shinyjs::hidden(tags$div(
        id = "source_card",
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        tags$div(
          class = "custom-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; cursor: pointer; user-select: none; margin-bottom: 0;",
          h4("Source", style = "margin: 0; font-weight: bold; font-size: 16px;"),
          tags$button(
            type = "button", 
            class = "btn-minimize", 
            style = "background: transparent; border: none; font-size: 1.6em; line-height: 1; cursor: pointer; padding: 0 5px;",
            HTML("&plus;")
          ),
        ),
        tags$div(
          class = "custom-card-content",
          # style = "margin-top: 15px;", 
          style = "display: none;",
          tags$br(),
          uiOutput("dynamic_source_ui") 
        )
      )),
      
      # Section 4: Log Output 
      tags$div(id = "log_wrapper",
               # A proper panel header that ALSO acts as your drag handle
               tags$div(id = "log_header",
                        tags$span("Execution Log", style = "font-weight: bold; color: #333;"),
                        tags$div(class = "drag-grip") # The visual drag lines
               ),
               # The actual log output
               # verbatimTextOutput("log")
               htmlOutput("log", 
                          style = "background-color: #f5f5f5; 
                            border: 1px solid #ccc; 
                            border-radius: 4px; 
                            padding: 10px; 
                            font-family: Menlo, Monaco, Consolas, 'Courier New', monospace; 
                            font-size: 13px; 
                            color: #333; 
                            white-space: pre-wrap; 
                            word-wrap: break-word; 
                            max-height: 400px; 
                            overflow-y: auto;"
                          ),
               tags$div(style = "display: flex; justify-content: center; margin-bottom: 15px; padding-top: 10px;",
                        
                        shinyWidgets::actionBttn(
                          inputId = "clear_log", 
                          label = "Clear Log", 
                          icon = icon("trash-can"),
                          style = "minimal", 
                          color = "danger",
                          size = "xs",       # Options: "xs", "sm", "md" (default), "lg"
                          no_outline = TRUE,
                          block = FALSE      # Set to FALSE (or remove) to stop it from stretching
                        )
                        
               )
      )
    ),
    
    column(
      width = 9,
      # 3) Impact Metrics
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #4B8BBE; margin-bottom: 25px;",
        tags$div(
          class = "custom-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; cursor: pointer; user-select: none; margin-bottom: 0;",
          tags$div(
            style = "display: flex; align-items: center; gap: 12px;",
            tags$input(
              id = "enable_summary", 
              type = "checkbox", 
              # checked = TRUE,
              class = "shiny-input-checkbox",
              style = "width: 20px; height: 20px; cursor: pointer; margin: 0; accent-color: #4B8BBE;" 
            ),
            h3("Collaboration Metrics", style = "color: #4B8BBE; margin: 0; font-weight: bold; line-height: 1;", tags$i(class = "fa fa-users", style = "font-size: 0.9em; opacity: 0.85;")),
          ),
          tags$button(
            type = "button", 
            class = "btn-minimize", 
            style = "background: transparent; border: none; font-size: 1.6em; line-height: 1; cursor: pointer; color: #4B8BBE; padding: 0 5px;",
            HTML("&plus;")
          )
        ),
        # hr(style = "border-top: 1px solid #edf2f7; margin-bottom: 20px;"), # Softened the hr() line
        conditionalPanel(
          condition = "input.enable_summary == true",
          tags$div(
            class = "custom-card-content",
            # style = "margin-top: 15px;", 
            style = "display: none;",
            hr(),        
            fluidRow(
              # Wrapped in minimal-table
              column(6, tags$div(class = "minimal-table", tableOutput("summary_table"))),
              
              # Vertically centering the SH-Index next to the table
              column(6, 
                     style = "display: flex; align-items: center; justify-content: flex-start; height: 100%; min-height: 80px;", 
                     shinyjs::disabled(shiny::uiOutput("sh_index")))
            )
          )
        )
        # # Wrapped in minimal-table
        # tags$div(class = "minimal-table", tableOutput("summary_table"))
      ),
      
      # 4) Visualizations
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #2E8B57; margin-bottom: 25px;",
        tags$div(
          class = "custom-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; cursor: pointer; user-select: none; margin-bottom: 0;",
          
          tags$div(
            style = "display: flex; align-items: center; gap: 12px;",
            tags$input(
              id = "enable_plots", 
              type = "checkbox", 
              # checked = TRUE,
              class = "shiny-input-checkbox",
              style = "width: 20px; height: 20px; cursor: pointer; margin: 0; accent-color: #2E8B57;"
            ),
            h3("Publication & Citation Trends", style = "color: #2E8B57; margin: 0; font-weight: bold; line-height: 1;", tags$i(class = "fa fa-chart-line", style = "font-size: 0.9em; opacity: 0.85;")),
          ),
          tags$button(
            type = "button", 
            class = "btn-minimize", 
            style = "background: transparent; border: none; font-size: 1.6em; line-height: 1; cursor: pointer; color: #2E8B57; padding: 0 5px;",
            HTML("&plus;")
          )
        ),
        conditionalPanel(
          condition = "input.enable_plots == true",
          tags$div(
            class = "custom-card-content",
            # style = "margin-top: 15px;", 
            style = "display: none;",
            hr(),
            fluidRow(
              column(6, plotly::plotlyOutput("acounts_plot")),
              column(6, plotly::plotlyOutput("ccounts_plot"))
            ),
            tags$br(),
            plotly::plotlyOutput("cdist_plot"),
            tags$br(),
            fluidRow(
              column(6, plotly::plotlyOutput("aperc_plot", height = "150px")),
              column(6, plotly::plotlyOutput("cperc_plot", height = "150px"))
            )
          )
        )
      ),
      
      # 5) Detailed Publication Record
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #D6A77A; margin-bottom: 25px;",
        tags$div(
          class = "custom-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; cursor: pointer; user-select: none; margin-bottom: 0;",
          
          tags$div(
            style = "display: flex; align-items: center; gap: 12px;",
            
            tags$input(
              id = "enable_table", 
              type = "checkbox", 
              class = "shiny-input-checkbox", 
              style = "width: 20px; height: 20px; cursor: pointer; margin: 0; accent-color: #D6A77A;" 
            ),
            h3("Detailed Publication Record", style = "color: #D6A77A; margin: 0; font-weight: bold; line-height: 1;", tags$i(class = "fa fa-table-list", style = "font-size: 0.9em; opacity: 0.85;")),
          ),
          
          # Right Side: Fixed Toggle Button
          tags$button(
            type = "button", 
            class = "btn-minimize", 
            style = "background: transparent; border: none; font-size: 1.6em; line-height: 1; cursor: pointer; color: #D6A77A; padding: 0 5px;",
            HTML("&plus;")
          )
        ),
        conditionalPanel(
          condition = "input.enable_table == true",
          tags$div(
            class = "custom-card-content",
            # style = "margin-top: 15px;", 
            style = "display: none;",
            hr(),
            DT::DTOutput("extended_table")
          # DT::dataTableOutput("extended_table")
          )
        )
      ),
      # 6) Network graph - filtered
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #C96480; margin-bottom: 25px;",
        
        # CSS to cap the height of the selectize input and make it scrollable
        tags$style(HTML("
    #neighbourhood_nodes + .selectize-control .selectize-input {
      max-height: 120px; /* Adjust this value if you want the box taller/shorter */
      overflow-y: auto;
    }
  ")),
        
        # 1. FIXED HEADER ROW CONTAINER (margin-bottom set to 0)
        tags$div(
          class = "custom-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; cursor: pointer; user-select: none; margin-bottom: 0;",
          
          tags$div(
            style = "display: flex; align-items: center; gap: 12px;",
            
            tags$input(
              id = "enable_network", 
              type = "checkbox", 
              class = "shiny-input-checkbox", 
              style = "width: 20px; height: 20px; cursor: pointer; margin: 0; accent-color: #C96480;"
            ),
            h3("Network Graph - Filtered", style = "color: #C96480; margin: 0; font-weight: bold; line-height: 1;", tags$i(class = "fa fa-diagram-project", style = "font-size: 0.9em; opacity: 0.85;"))
          ),
          
          # Right Side: Fixed Toggle Button
          tags$button(
            type = "button", 
            class = "btn-minimize", 
            style = "background: transparent; border: none; font-size: 1.6em; line-height: 1; cursor: pointer; color: #C96480; padding: 0 5px;",
            HTML("&plus;")
          )
        ),
        conditionalPanel(
          condition = "input.enable_network == true",
          # 2. COLLAPSIBLE CONTENT CONTAINER (Using margin-top for dynamic spacing)
          tags$div(
            class = "custom-card-content",
            # style = "margin-top: 15px;", 
            style = "display: none;",
            hr(),
            
            # Grid for most controls
            tags$div(
              style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; align-items: end; margin-top: 10px; margin-bottom: 10px; padding: 15px; background: #f8f9fa; border-radius: 6px; border: 1px solid #e9ecef;",
              
              selectInput("net_col_filtered", "Choose column to visualize:", choices = NULL, width = "100%"),
              selectizeInput("custom_node_selector", "Search/Select Keyword:", 
                             multiple = TRUE,
                             choices = NULL, 
                             width = "100%", 
                             options = list(placeholder = 'Type a keyword...')),
              numericInput(
                inputId = "fr_iterations", 
                label = tags$span(
                  "Fruchterman-Reingold Iterations (Cluster Stabilization):",
                  tags$i(
                    class = "fa fa-info-circle", 
                    style = "color: #C96480; margin-left: 5px; cursor: help;", 
                    title = "Controls how many simulation cycles the layout runs. Higher numbers yield more stable, distinct clustering but take longer to calculate."
                  )
                ), 
                value = 500, 
                min = 1, 
                step = 1
              ),
              sliderInput(
                inputId = "node_freq_range", 
                label = tags$span(
                  "Node Frequency (Min/Max Appearances):",
                  tags$i(
                    class = "fa fa-info-circle", 
                    style = "color: #C96480; margin-left: 5px; cursor: help;", 
                    title = "Filter nodes by their frequency/occurrence range."
                  )
                ), 
                value = c(1, 2), 
                min = 1, 
                max = 5,
                step = 1,
                width = "100%"
              ),
              tags$div(id = "edge-slider-wrap",
                       sliderInput(
                         inputId = "edge_freq_range", 
                         label = tags$span(
                           "Edge Frequency (Co-occurrences Range):",
                           tags$i(
                             class = "fa fa-info-circle", 
                             style = "color: #C96480; margin-left: 5px; cursor: help;", 
                             title = "Filter edges. The lower handle sets the minimum connections required, and the upper handle sets the maximum allowed edges."
                           )
                         ), 
                         value = c(1, 1),
                         min = 0, 
                         max = 2, 
                         step = 1,
                         width = "100%"
                       )
              ),
              sliderInput("cluster_size_range", 
                          label = "Cluster Size (Min/Max Nodes per Group):",
                          min = 1, max = 500, value = c(1, 500)),
              checkboxInput("prune_leaves", tags$b("Trim Leaf Nodes (Degree = 1)"), value = TRUE),
              tags$div(
                style = "margin-top: 10px; margin-bottom: 10px; width: 100%;",
                
                # 1. Move the Label completely ABOVE the flex row
                tags$label(
                  style = "font-weight: bold; margin-bottom: 5px; color: #333; display: block;",
                  "Node Icon:",
                  tags$a(
                    href = "https://fontawesome.com/search?o=r&m=free", 
                    target = "_blank", # Opens in new tab
                    tags$i(
                      class = "fa fa-info-circle", 
                      style = "color: #4B8BBE; margin-left: 5px; cursor: pointer;", 
                      title = "Click to search FontAwesome. Use the 4-character unicode hex (e.g., f007 for user, f19d for graduation-cap)."
                    )
                  )
                ),
                
                # 2. Flex Row containing ONLY the input box and the preview box
                tags$div(
                  style = "display: flex; align-items: center; gap: 12px; width: 100%;",
                  
                  # Textbox container - expands to fill horizontal space
                  tags$div(
                    style = "flex-grow: 1;",
                    # Tiny CSS override to kill the default Bootstrap 15px bottom margin
                    tags$style(HTML(".kill-margin .form-group { margin-bottom: 0 !important; }")), 
                    class = "kill-margin",
                    
                    textInput(
                      inputId = "custom_icon_code", 
                      label = NULL, # Setting this to NULL eliminates layout shifts
                      value = "f007", 
                      placeholder = "e.g., f19d",
                      width = "100%"
                    )
                  ),
                  
                  # Clean, matching inline Preview Box
                  tags$div(
                    style = "width: 40px; height: 34px; display: flex; align-items: center; justify-content: center; background: #ffffff; border: 1px solid #ccc; border-radius: 4px; box-shadow: inset 0 1px 3px rgba(0,0,0,0.05); color: #4B8BBE; font-size: 1.25em;",
                    uiOutput("fa_icon_preview")
                  )
                )
              ),
              sliderInput(
                inputId = "custom_max_keywords", 
                label = tags$span(
                  "Maximum Keyword Count:",
                  tags$i(
                    class = "fa fa-info-circle", 
                    style = "color: #C96480; margin-left: 5px; cursor: help;", 
                    title = "Maximum keyword count allowed for a publication."
                  )
                ), 
                value = c(1,500), 
                min = 1, 
                max = 500,
                step = 1
              ),
              shinyjs::disabled(sliderInput("neighbourhood_slider", "Neighbourhood %:", min = 0, max = 100, value = 0, round = F, width = "100%"))
            ),
            
            # Full-width container specifically for the selectize input to prevent squishing
            tags$div(
              style = "padding: 0 15px 15px 15px; margin-bottom: 10px;",
              shinyjs::disabled(
                selectizeInput(
                  "neighbourhood_nodes", 
                  "Neighbourhood Nodes:", 
                  multiple = TRUE,
                  choices = NULL, 
                  width = "100%", 
                  options = list(
                    placeholder = 'Neighbourhood Nodes...',
                    plugins = list('remove_button') # Keeping the delete button for easy removal
                  )
                )
              )
            ),
            
            visNetwork::visNetworkOutput("network_filtered", height = "500px"),
            uiOutput("network_summary_table")
          )
        )
      ),
      tags$div(
        class = "custom-card",
        style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #391463; margin-bottom: 25px;",
        
        tags$div(
          class = "custom-card-header",
          style = "display: flex; justify-content: space-between; align-items: center; cursor: pointer; user-select: none; margin-bottom: 0;",
          
          tags$div(
            style = "display: flex; align-items: center; gap: 12px;",
            
            tags$input(
              id = "enable_venn", 
              type = "checkbox", 
              class = "shiny-input-checkbox", 
              style = "width: 20px; height: 20px; cursor: pointer; margin: 0; accent-color: #391463;"
            ),
            h3("Overlap Analysis", style = "color: #391463; margin: 0; font-weight: bold; line-height: 1;", tags$i(class = "fa fa-chart-pie", style = "font-size: 0.9em; opacity: 0.85;"))
          ),
          
          # Right Side: Fixed Toggle Button
          tags$button(
            type = "button", 
            class = "btn-minimize", 
            style = "background: transparent; border: none; font-size: 1.6em; line-height: 1; cursor: pointer; color: #391463; padding: 0 5px;",
            HTML("&plus;")
          )
        ),
        # Card Body (The Plot)
        tags$div(
          class = "custom-card-content",
          # Move your conditionalPanel inside the wrapper
          conditionalPanel(
            condition = "input.enable_venn == true",
            hr(),
            tags$div(
              style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; align-items: end; margin-top: 10px; margin-bottom: 10px; padding: 15px; background: #f8f9fa; border-radius: 6px; border: 1px solid #e9ecef;",
              selectInput("venn_col_filtered", "Choose column to visualize:", choices = NULL, width = "100%"),
              selectizeInput("custom_venn_selector", "Search/Select Keyword:", 
                             multiple = TRUE,
                             choices = NULL, 
                             width = "100%", 
                             options = list(placeholder = 'Type a keyword...')),
              colourpicker::colourInput("venn_theme_color", "Pick base theme color:", value="#391463")
            ),
            tags$div(
              style = "width: 100%; display: flex; justify-content: center; align-items: center;",
              plotOutput("venn_plot", height = "400px", width = "50%"),
              plotOutput("venn_upset_plot", height = "400px", width = "50%")
            )
          )
        )
      ),
      # # 7) Network graph - full
      # tags$div(
      #   class = "custom-card",
      #   style = "box-shadow: 0 4px 8px rgba(0,0,0,0.05); padding: 20px; border-radius: 10px; border-top: 6px solid #554348; margin-bottom: 25px;",
      #   
      #   h3("Network Graph - Full", style = "color: #554348; margin-top: 0; font-weight: bold;"),
      #   
      #   # Standard selectInput instead of uiOutput
      #   div(style = "background: #fdfdfd; padding: 10px 15px; border-radius: 6px; border: 1px solid #eaeaea; margin-bottom: 15px;",
      #       selectInput("net_col_full", "Choose column to visualize:", choices = NULL, width = "100%")
      #   ),
      #   
      #   visNetwork::visNetworkOutput("network_full", height = "500px")
      # ),
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
            
            h3("Processing Data...", style = "margin-top: 0; color: #2c3e50; font-weight: bold;"),
            p("Please wait while CollabNET fetches and analyzes the records. This may take a moment.", 
              style = "color: #7f8c8d; margin-bottom: 25px;"),
            
            tags$div(
              id = "progress_bars_container",
              style = "text-align: left; margin-bottom: 25px;",
              
              tags$div(id = "orcid_bar_container", class = "api-progress-wrapper",
                       tags$label("DOI / ORCID Resolver", class = "api-progress-label"),
                       shinyWidgets::progressBar(id = "prog_doi", title = "0%", value = 0, total = 100, status = "warning")
              ),
              tags$div(id = "scopus_bar_container", class = "api-progress-wrapper",
                       tags$label("Scopus API", class = "api-progress-label"),
                       shinyWidgets::progressBar(id = "prog_scopus", title = "0%", value = 0, total = 100, status = "info")
              ),
              tags$div(id = "journal_bar_container", class = "api-progress-wrapper",
                       tags$label("Journal Matcher", class = "api-progress-label"),
                       shinyWidgets::progressBar(id = "prog_journal", title = "0%", value = 0, total = 100, status = "warning")
              )
              # tags$div(id = "wos_bar_container", class = "api-progress-wrapper",
              #          tags$label("Web of Science API", class = "api-progress-label"),
              #          shinyWidgets::progressBar(id = "prog_wos", title = "0%", value = 0, total = 100, status = "primary")
              # ),
              # tags$div(id = "semantic_bar_container", class = "api-progress-wrapper",
              #          tags$label("Semantic Scholar API", class = "api-progress-label"),
              #          shinyWidgets::progressBar(id = "prog_semantic", title = "0%", value = 0, total = 100, status = "success")
              # )
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
