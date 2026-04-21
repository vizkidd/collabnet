suppressPackageStartupMessages(require(readxl))
suppressPackageStartupMessages(require(tools))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(igraph))
suppressPackageStartupMessages(require(plotly))

# ---------------------------
# Aggregate counts and citations by Position × Quartile
# ---------------------------
# We'll produce a long table with columns: Position, Quartile, Count, SumCitations
make_agg <- function(df, pos_col, pos_label) {
  
  # # 1. DEFENSIVE CHECK: Create Qscore if missing, or replace NAs if it exists
  # if (!"Qscore" %in% names(df)) {
  #   df$Qscore <- 'NA' # Create the column if it completely failed to attach
  # } else {
  #   df$Qscore <- df$Qscore %>% tidyr::replace_na('NA') # Fix existing NAs
  # }
  
  # 2. PROCEED WITH AGGREGATION
  return(
    df %>%
      filter(!is.na(.data[[pos_col]]) & as.numeric(.data[[pos_col]]) == 1) %>%
      group_by(Qscore, .drop = FALSE) %>%
      summarise(
        Count = n(),
        SumCitations = sum(suppressWarnings(as.numeric(Citations)), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Position = pos_label)
  )
}

library(dplyr)
library(tidyr)
library(visNetwork)

library(stringr) # Make sure you have this loaded!

build_collaboration_network <- function(df, main_authors_list) {
  
  # 1. Create ALL-TO-ALL Edges
  edges <- df %>%
    mutate(paper_id = row_number()) %>%
    select(paper_id, Authors) %>%
    separate_rows(Authors, sep = ",\\s*") %>%
    
    # 🧹 AGGRESSIVE STRING CLEANING 🧹
    mutate(
      Authors = str_squish(Authors),          # Removes all double/hidden spaces
      Authors = str_to_title(Authors)         # Standardizes case (e.g., "john doe" -> "John Doe")
    ) %>%
    
    filter(Authors != "") %>%
    
    # Self-Join to create all combinations
    inner_join(., ., by = "paper_id", relationship = "many-to-many") %>%
    
    # Keep unique pairs only
    filter(Authors.x < Authors.y) %>% 
    
    rename(from = Authors.x, to = Authors.y) %>%
    group_by(from, to) %>%
    summarise(connections = n(), .groups = "drop") %>%
    mutate(
      length = (300 / connections) + 30,
      title = paste("Co-authored:", connections, "papers"),
      value = connections 
    )
  
  # 2. Identify all unique authors
  all_authors <- unique(c(edges$from, edges$to))
  
  # 3. Calculate Node Size
  node_sizes <- bind_rows(
    edges %>% select(id = from, val = connections),
    edges %>% select(id = to, val = connections)
  ) %>%
    group_by(id) %>%
    summarise(total_connections = sum(val), .groups = "drop")
  
  # 4. Create Nodes Dataframe
  nodes <- data.frame(id = all_authors) %>%
    left_join(node_sizes, by = "id") %>%
    mutate(
      label = id,
      title = paste0(
        "<div style='padding: 8px; border-radius: 5px; background: white; color: black; box-shadow: 1px 1px 5px rgba(0,0,0,0.2);'>",
        "<b>", id, "</b><br>",
        "<i>Number of Collaborations: ", total_connections, "</i>",
        "</div>"
      ),
      size = 15 + (replace_na(total_connections, 0) * 3),
      group = ifelse(id %in% main_authors_list, "Main Author", "Co-Author"),
      shape = "icon",
      icon.face = "FontAwesome",
      icon.code = "f007", 
      icon.color = ifelse(id %in% main_authors_list, "#E74C3C", "#3498DB")
    )
  
  return(list(nodes = nodes, edges = edges))
}

# build_collaboration_network <- function(df, main_authors_list) {
#   # 1. Create ALL-TO-ALL Edges via Self-Join (Extremely Fast)
#   edges <- df %>%
#     mutate(paper_id = row_number()) %>%
#     select(paper_id, Authors) %>%
#     separate_rows(Authors, sep = ",\\s*") %>%
#     mutate(Authors = trimws(Authors)) %>%
#     filter(Authors != "") %>%
#     
#     # The Magic Step: Join the papers to themselves to create all combinations
#     inner_join(., ., by = "paper_id", relationship = "many-to-many") %>%
#     
#     # Keep unique pairs only! 
#     # 'Authors.x < Authors.y' removes A-A (self-loops) and prevents duplicate A-B / B-A edges
#     filter(Authors.x < Authors.y) %>% 
#     
#     rename(from = Authors.x, to = Authors.y) %>%
#     group_by(from, to) %>%
#     summarise(connections = n(), .groups = "drop") %>%
#     mutate(
#       length = (300 / connections) + 30,
#       title = paste("Co-authored:", connections, "papers"),
#       value = connections 
#     )
#   
#   # 2. Identify all unique authors
#   all_authors <- unique(c(edges$from, edges$to))
#   
#   # 3. Calculate Node Size (Total connections for each author)
#   node_sizes <- bind_rows(
#     edges %>% select(id = from, val = connections),
#     edges %>% select(id = to, val = connections)
#   ) %>%
#     group_by(id) %>%
#     summarise(total_connections = sum(val), .groups = "drop")
#   
#   # 4. Create Nodes Dataframe
#   nodes <- data.frame(id = all_authors) %>%
#     left_join(node_sizes, by = "id") %>%
#     mutate(
#       label = id,
#       title = paste0(
#         "<div style='padding: 8px; border-radius: 5px; background: white; color: black; box-shadow: 1px 1px 5px rgba(0,0,0,0.2);'>",
#         "<b>", id, "</b><br>",
#         "<i>Total Collaborations: ", total_connections, "</i>",
#         "</div>"
#       ),
#       size = 15 + (replace_na(total_connections, 0) * 3),
#       group = ifelse(id %in% main_authors_list, "Main Author", "Co-Author"),
#       shape = "icon",
#       icon.face = "FontAwesome",
#       icon.code = "f007", 
#       icon.color = ifelse(id %in% main_authors_list, "#E74C3C", "#3498DB")
#     )
#   
#   return(list(nodes = nodes, edges = edges))
# }


plot_glens_table <- function(rv,session){
  # req(rv$glens_year_filtered, nrow(rv$glens_year_filtered) > 0)
  if(is.null(rv$glens_year_filtered) || nrow(rv$glens_year_filtered) <= 0 || !all(c("First_Author", "Second_Author", "Co_Author", "Corresponding_Author", "Adjusted_Citations") %in% colnames(rv$glens_year_filtered)) ){
    rv$log_text <- paste(rv$log_text, "plot_glens_table(): Warning: No data available for these filters!\n", sep="")
    warning("plot_glens_table(): Warning: No data available for these filters!")
    shinyjs::hide("sh_index")
    shinyjs::hide("summary_table")
    shinyjs::hide("acounts_plot")
    shinyjs::hide("ccounts_plot")
    shinyjs::hide("cdist_plot")
    shinyjs::hide("aperc_plot")
    shinyjs::hide("cperc_plot")
    shinyjs::hide("network_filtered")
    # shinyjs::hide("extended_table")
    return() # Stop execution here
  }
  shinyjs::show("acounts_plot")
  shinyjs::show("ccounts_plot")
  shinyjs::show("cdist_plot")
  shinyjs::show("aperc_plot")
  shinyjs::show("cperc_plot")
  shinyjs::show("network_filtered")
  
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
  
  # ---- animate update via plotlyProxy ----
  acounts_proxy <- plotlyProxy("acounts_plot", session)
  
  acounts_proxy_data <- lapply(all_quartiles, function(q) {
    agg_all %>%
      filter(Qscore == q) %>%
      arrange(Position) %>%
      pull(Count)
  })
  
  # 1. Format the data explicitly as a list of traces for the 'animate' method
  acounts_animate_payload <- lapply(acounts_proxy_data, function(y_vals) {
    list(y = y_vals)
  })
  
  # 2. Invoke 'animate' with transition settings
  plotlyProxyInvoke(
    acounts_proxy,
    "animate",
    # Argument 1: The new data
    list(
      data = acounts_animate_payload,
      traces = as.list(0:(length(all_quartiles) - 1)) # Explicitly tell it which traces to map to
    ),
    
    # Argument 2: The animation settings
    list(
      transition = list(
        duration = 200,               # 800 milliseconds (0.8 seconds)
        easing = "cubic-in-out"       # Starts slow, speeds up, ends slow
      ),
      frame = list(
        duration = 200,
        redraw = FALSE                # Set to FALSE for smoother SVG morphing
      )
    )
  )
  
  ccounts_proxy <- plotlyProxy("ccounts_plot", session)
  
  # 1. FIX DATA LENGTH MISMATCH
  # Ensure every quartile has an entry for every position, padding with 0s.
  # This guarantees pull(Count) always returns arrays of the exact same length.
  agg_complete <- agg_all %>%
    tidyr::complete(
      Qscore = all_quartiles, 
      Position = unique(all_positions), # Or use a predefined 'all_positions' vector if you have one
      fill = list(Count = 0)
    )
  
  ccounts_proxy_data <- lapply(all_quartiles, function(q) {
    agg_complete %>%
      filter(Qscore == q) %>%
      arrange(Position) %>%
      pull(Count)
  })
  
  # Format the data explicitly as a list of traces for the 'animate' method
  ccounts_animate_payload <- lapply(ccounts_proxy_data, function(y_vals) {
    list(y = y_vals)
  })
  
  # 2. INVOKE ANIMATE WITH AUTORANGE AND REDRAW
  plotlyProxyInvoke(
    ccounts_proxy,
    "animate",
    
    # Argument 1: The new data and layout instructions
    list(
      data = ccounts_animate_payload,
      traces = as.list(0:(length(all_quartiles) - 1)),
      layout = list(yaxis = list(autorange = TRUE)) # Explicitly tell the layout to adapt
    ),
    
    # Argument 2: The animation settings
    list(
      transition = list(
        duration = 200,                
        easing = "cubic-in-out"        
      ),
      frame = list(
        duration = 200,
        redraw = TRUE # MUST be TRUE to allow the axis to scale down from 1400
      )
    )
  )
  
  stats_by_position <- df_ordered_debug %>%
    group_by(position_rank) %>%
    mutate(
      position_rank = factor(position_rank),
      Qscore = factor(Qscore),
      Adjusted_Citations = as.numeric(Adjusted_Citations),
      Citations = as.numeric(Citations)
    ) %>%
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
  
  make_perc_hover_text <- function(df) {
    return(paste0(
      "<b>Position:</b> ", df$position_rank,
      "<br><b>Contribution %:</b> ", df$pcontrib
    ))
  }
  
  req(pub_pdata)
  
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
  #End - Plotting
}

render_skeleton_plots <- function(rv, output){
  # print("HERE1.2.1")
  # print(str(rv$glens_year_filtered))
  # req(rv$glens_year_filtered, nrow(rv$glens_year_filtered) > 0)
  # print("HERE1.2.2")
  if(is.null(rv$glens_year_filtered) || nrow(rv$glens_year_filtered) <= 0){
    rv$log_text <- paste(rv$log_text, "render_skeleton_plots(): Warning: No data available for these filters!", sep="<br>")
    warning("render_skeleton_plots(): Warning: No data available for these filters!")
    shinyjs::hide("sh_index")
    shinyjs::hide("summary_table")
    shinyjs::hide("acounts_plot")
    shinyjs::hide("ccounts_plot")
    shinyjs::hide("cdist_plot")
    shinyjs::hide("aperc_plot")
    shinyjs::hide("cperc_plot")
    shinyjs::hide("network_filtered")
    # shinyjs::hide("extended_table")
    return() # Stop execution here
  }
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
  outputOptions(output, "acounts_plot", suspendWhenHidden = FALSE)
  
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
  outputOptions(output, "ccounts_plot", suspendWhenHidden = FALSE)
  
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
  outputOptions(output, "cdist_plot", suspendWhenHidden = FALSE)
  
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
  outputOptions(output, "aperc_plot", suspendWhenHidden = FALSE)
  
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
  outputOptions(output, "cperc_plot", suspendWhenHidden = FALSE)
  
} # End - Plot skeleton renders
