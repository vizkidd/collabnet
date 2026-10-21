suppressPackageStartupMessages(require(readxl))
suppressPackageStartupMessages(require(tools))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(igraph))
suppressPackageStartupMessages(require(plotly))
suppressPackageStartupMessages(require(igraph))
suppressPackageStartupMessages(require(visNetwork))

# ---------------------------
# Aggregate counts and citations by Position × Quartile
# ---------------------------
# We'll produce a long table with columns: Position, Quartile, Count, SumCitations
make_agg <- function(df, pos_col, pos_label) {
  # 1. Standardize Qscore to character and replace true NAs with the string "NA"
  df <- df %>%
    mutate(Qscore = as.character(Qscore),
           Qscore = ifelse(is.na(Qscore) | Qscore == "", "NA", Qscore))
  
  # DEBUG: CHECKING whether QScore shows NA strings when unavailable
  test_df <- df %>% filter(is.na(.data[[pos_col]]) | as.numeric(.data[[pos_col]]) != 1)
  print(test_df$Qscore)
  
  # 2. PROCEED WITH AGGREGATION
  return(
    df %>%
      # Ensure we only count papers where the author position is active (1)
      filter(!is.na(.data[[pos_col]]) & as.numeric(.data[[pos_col]]) == 1) %>%
      group_by(Qscore) %>% 
      summarise(
        Count = n(),
        SumCitations = sum(suppressWarnings(as.numeric(Citations)), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(Position = pos_label)
  )
}

plot_glens_table <- function(rv,df,session){
  print("plot_glens_table():")
  if(is.null(df) || nrow(df) <= 0 || !all(c("First_Author", "Second_Author", "Co_Author", "Corresponding_Author", "Adjusted_Citations") %in% colnames(df)) ){
    rv$log_text <- paste(rv$log_text, "plot_glens_table(): Warning: No data available for these filters!", sep="<br>")
    warning("plot_glens_table(): Warning: No data available for these filters!")
    # shinyjs::hide("sh_index")
    # shinyjs::hide("summary_table")
    shinyjs::hide("acounts_plot")
    shinyjs::hide("ccounts_plot")
    shinyjs::hide("cdist_plot")
    shinyjs::hide("aperc_plot")
    shinyjs::hide("cperc_plot")
    shinyjs::hide("network_filtered")
    return() # Stop execution here
  }
  
  # Show the safe plots (Counts are safe even if 0)
  shinyjs::show("acounts_plot")
  shinyjs::show("ccounts_plot")
  shinyjs::show("network_filtered")
  
  df_ordered_debug <- df %>%
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
  
  # 1. Standardize the Qscore column BEFORE aggregating
  df_ordered_debug <- df_ordered_debug %>%
    mutate(Qscore = as.character(Qscore),
           Qscore = ifelse(is.na(Qscore) | Qscore == "", "NA", Qscore))
  
  agg_first <- make_agg(df_ordered_debug, "First_Author", "First Author")
  agg_second <- make_agg(df_ordered_debug, "Second_Author", "Second Author")
  agg_co <- make_agg(df_ordered_debug, "Co_Author", "Co-Author")
  agg_cor <- make_agg(df_ordered_debug, "Corresponding_Author", "Corresponding Author")
  
  agg_all <- bind_rows(agg_first, agg_second, agg_co, agg_cor)
  
  all_positions <- c("First Author","Second Author","Co-Author","Corresponding Author")
  all_quartiles <- c("Q1","Q2","Q3","Q4", "NA")
  full_grid <- expand.grid(Position = all_positions, Qscore = all_quartiles, stringsAsFactors = FALSE)
  agg_all <- full_grid %>%
    left_join(agg_all, by = c("Position","Qscore")) %>%
    mutate(Count = tidyr::replace_na(Count, 0L),
           SumCitations = tidyr::replace_na(SumCitations, 0.0))
  
  # # Calculate totals per Quartile (ignoring position) to show in tooltips
  # agg_all <- agg_all %>%
  #   group_by(Qscore) %>%
  #   mutate(
  #     Total_Q_Pubs = sum(Count, na.rm = TRUE),
  #     Total_Q_Cites = sum(SumCitations, na.rm = TRUE)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(Position = factor(Position, levels = all_positions))
  # 
  # # Calculate totals per Position (ignoring Quartile) to show in tooltips
  # agg_all <- agg_all %>%
  #   group_by(Position) %>%
  #   mutate(
  #     Total_P_Pubs = sum(Count, na.rm = TRUE),
  #     Total_P_Cites = sum(SumCitations, na.rm = TRUE)
  #   ) %>%
  #   ungroup() %>%
  #   mutate(Position = factor(Position, levels = all_positions))
  
  agg_all <- agg_all %>%
    group_by(Position) %>%
    mutate(
      Total_P_Pubs = sum(Count, na.rm = TRUE),
      Total_P_Cites = sum(SumCitations, na.rm = TRUE)
    ) %>%
    group_by(Qscore) %>%
    mutate(
      Total_Q_Pubs = sum(Count, na.rm = TRUE),
      Total_Q_Cites = sum(SumCitations, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(Position = factor(Position, levels = all_positions))
  
  print(agg_all)
  # print("QPUBS & QCITES")
  # print(agg_all$Total_Q_Pubs)
  # print(agg_all$Total_Q_Cites)
  # ---- animate update via plotlyProxy (Counts) ----
  acounts_proxy <- plotlyProxy("acounts_plot", session)
  # acounts_proxy_data <- lapply(all_quartiles, function(q) {
  #   agg_all %>% filter(Qscore == q) %>% arrange(Position) %>% pull(Count)
  # })
  # acounts_animate_payload <- lapply(acounts_proxy_data, function(y_vals) list(y = y_vals))
  # Create a list of lists containing both the new Y values and the new Hover Text
  # min_acounts <- 0
  # max_acounts <- 1
  acounts_animate_payload <- lapply(all_quartiles, function(q) {
    sub_data <- agg_all %>% filter(Qscore == q) %>% arrange(Position)
    # print(q)
    # print(sub_data)
    # if(max_acounts < sub_data$Total_P_Pubs){
    #   max_acounts <- sub_data$Total_P_Pubs
    # }
    list(
      x = sub_data$Position,
      y = sub_data$Count,
      # customdata = sub_data$Total_Q_Pubs,
      hovertemplate = paste0(
        "<b>Position:</b> ", sub_data$Position, "<br>",
        "<b>Quartile:</b> ", q, "<br>",
        "<b>Quartile Publications:</b> ", sub_data$Count, "<br>",
        paste0("<b>Total '",sub_data$Position,"' Publications:</b> "), sub_data$Total_P_Pubs, "<br>",
        paste0("<b>Total '",q,"' Publications:</b> "), sub_data$Total_Q_Pubs, "<br>",
        "<b>Total Publications:</b> ", sum(agg_all$Count),
        "<extra></extra>" # This tag deletes the "(First Author, 2)" header
      )
    )
  })
  # print(str(acounts_animate_payload))
  # print(paste("max_acounts:",max_acounts))
  plotlyProxyInvoke(acounts_proxy, 
                    "animate",
                    list(
                      data = acounts_animate_payload, 
                      traces = as.list(0:(length(all_quartiles) - 1)),
                      layout = list(yaxis = list(range=c(0,max(na.omit(agg_all$Total_P_Pubs))), autorange = TRUE, rangemode = "nonnegative"),
                                    autosize = TRUE)
                      ),
                    list(
                      transition = list(duration = 200, easing = "cubic-in-out"), 
                      frame = list(duration = 200, redraw = F)
                      )
                    )
  
  ccounts_proxy <- plotlyProxy("ccounts_plot", session)
  agg_complete <- agg_all %>%
    tidyr::complete(Qscore = all_quartiles, Position = unique(all_positions), fill = list(Count = 0))
  # ccounts_proxy_data <- lapply(all_quartiles, function(q) {
  #   agg_complete %>% filter(Qscore == q) %>% arrange(Position) %>% pull(Count)
  # })
  # ccounts_animate_payload <- lapply(ccounts_proxy_data, function(y_vals) list(y = y_vals))
  # Use SumCitations instead of Count for the Citation plot
  # min_ccounts <- 0
  ccounts_animate_payload <- lapply(all_quartiles, function(q) {
    sub_data <- agg_all %>% filter(Qscore == q) %>% arrange(Position)
    list(
      x = sub_data$Position,
      y = sub_data$SumCitations,
      # customdata = sub_data$Total_Q_Cites,
      hovertemplate = paste0(
        "<b>Position:</b> ", sub_data$Position, "<br>",
        "<b>Quartile:</b> ", q, "<br>",
        "<b>Quartile Citations:</b> ", sub_data$SumCitations, "<br>",
        paste0("<b>Total '",sub_data$Position,"' Citations:</b> "), sub_data$Total_P_Cites, "<br>",
        paste0("<b>Total '",q,"' Citations:</b> "), sub_data$Total_Q_Cites, "<br>",
        "<b>Total Citations:</b> ", sum(agg_all$SumCitations),
        "<extra></extra>" # This tag deletes the "(First Author, 2)" header
      )
    )
  })
  # print(paste("max_ccounts:",max_ccounts))
  plotlyProxyInvoke(ccounts_proxy, 
                    "animate", 
                    list(
                      data = ccounts_animate_payload, 
                      traces = as.list(0:(length(all_quartiles) - 1)),
                      layout = list(yaxis = list(range=c(0,max(na.omit(agg_all$Total_P_Cites))), autorange = TRUE, rangemode = "nonnegative"),
                                    autosize = TRUE)
                      ),
                    list(
                      transition = list(duration = 200, easing = "cubic-in-out"),
                      frame = list(duration = 200, redraw = F)
                      )
                    )
  
  stats_by_position <- df_ordered_debug %>%
    group_by(position_rank) %>%
    mutate(position_rank = factor(position_rank), Qscore = factor(Qscore), Adjusted_Citations = as.numeric(Adjusted_Citations), Citations = as.numeric(Citations)) %>%
    summarise(min = min(Citations, na.rm = TRUE), q25 = quantile(Citations, 0.25, na.rm = TRUE), med = median(Citations, na.rm = TRUE), mean = mean(Citations, na.rm = TRUE), q75 = quantile(Citations, 0.75, na.rm = TRUE), max = max(Citations, na.rm = TRUE), .groups = "drop")
  
  df_plot <- df_ordered_debug %>%
    left_join(stats_by_position, by = "position_rank") %>%
    mutate(position_rank = factor(position_rank), Qscore = factor(Qscore), Adjusted_Citations = as.numeric(Adjusted_Citations), Citations = as.numeric(Citations))
  
  group_counts <- df_plot %>% count(position_rank)
  
  # =====================================================================
  # PLOT 1: CITATION DISTRIBUTION (Protected Flow)
  # =====================================================================
  if(nrow(df_plot) <= 1 || all(group_counts$n <= 1) || all(df_plot$Citations == 0)){
    shinyjs::hide("cdist_plot")
  } else {
    shinyjs::show("cdist_plot")
    
    make_dist_hover_text <- function(df) {
      return(paste0("<b>Position:</b> ", df$position_rank, "<br><b>Citations:</b> ", df$Citations, "<br><b>Qscore:</b> ", df$Qscore, "<br><b>Author Count:</b> ", df$Author_Count, "<br><b>Adjustment Weight:</b> ", df$Adjustment_Weights, "<br><b>Adjusted Citations:</b> ", df$Adjusted_Citations, "<br><b>Min:</b> ", df$min, "<br><b>25%:</b> ", df$q25, "<br><b>Median:</b> ", df$med, "<br><b>Mean:</b> ", round(df$mean, 1), "<br><b>75%:</b> ", df$q75, "<br><b>Max:</b> ", df$max))
    }
    
    cdist_proxy <- plotlyProxy("cdist_plot", session)
    for (i in seq_along(all_positions)) {
      pos <- all_positions[i]
      rows <- which(df_plot$position_rank == pos)
      y_violin <- if (length(rows) > 0) log1p(df_plot$Citations[rows]) else numeric(0)
      x_violin <- rep(i, length(y_violin))
      n_rows <- length(rows)
      if (n_rows > 0) {
        x_scatter <- i + runif(n_rows, -0.18, 0.18)
        y_scatter <- log1p(df_plot$Citations[rows])
        text_scatter <- make_dist_hover_text(df_plot[rows, , drop = FALSE])
        size_scatter <- (scale(df_plot$Adjusted_Citations[rows]) * 10) + 15
      } else {
        x_scatter <- numeric(0); y_scatter <- numeric(0); text_scatter <- character(0); size_scatter <- numeric(0)
      }
      
      violin_trace_idx <- (i - 1) * 2
      scatter_trace_idx <- (i - 1) * 2 + 1
      
      plotlyProxyInvoke(cdist_proxy, 
                        "restyle",
                        list(
                          x=list(x_violin),y = list(y_violin),
                          layout = list(yaxis = list(autorange = TRUE))
                          ),
                        list(violin_trace_idx)
                        )
      cdist_proxy_data <- list(x = list(x_scatter), y = list(y_scatter), text = list(text_scatter), `marker.size` = list(size_scatter))
      plotlyProxyInvoke(cdist_proxy, 
                        "restyle", 
                        cdist_proxy_data, 
                        list(scatter_trace_idx)
                        )
    }
  }
  
  # =====================================================================
  # PLOT 2: AUTHOR CONTRIBUTION % (Protected Flow)
  # =====================================================================
  total_pubs <- nrow(df_plot)
  pub_pdata <- df_plot %>% group_by(position_rank) %>% count() %>% ungroup() %>% mutate(pcontrib = if (is.na(total_pubs) || total_pubs == 0) 0 else (n / total_pubs) * 100)
  
  if(nrow(pub_pdata) <= 0 || total_pubs == 0){
    shinyjs::hide("aperc_plot")
  } else {
    shinyjs::show("aperc_plot")
    
    aperc_proxy <- plotlyProxy("aperc_plot", session)
    n_pos <- length(all_positions)
    
    aperc_vals <- sapply(all_positions, function(pos) {
      i <- which(pub_pdata$position_rank == pos)
      val <- if (length(i) == 1) pub_pdata$pcontrib[i] else 0
      if (is.na(val) || is.nan(val)) 0 else val
    })
    
    if(sum(aperc_vals) > 0) aperc_vals <- (aperc_vals / sum(aperc_vals)) * 100
    
    aperc_x_list <- lapply(aperc_vals, function(v) list(v)) 
    aperc_y_list <- lapply(seq_len(n_pos), function(i) list("Publications"))
    # aperc_text_list <- lapply(seq_len(n_pos), function(i) {
    #   list(paste0("<b>Position:</b> ", all_positions[i], "<br><b>Contribution %:</b> ", round(aperc_vals[i], 1), "%"))
    # })
    
    aperc_hover_list <- lapply(seq_len(n_pos), function(i) {
      list(paste0(
        "<b>Position:</b> ", all_positions[i], "<br>",
        "<b>Contribution %:</b> ", round(aperc_vals[i], 1), "%",
        "<extra></extra>" 
      ))
    })
    
    plotlyProxyInvoke(aperc_proxy, 
                      "restyle", 
                      list(
                        x = unname(aperc_x_list), y = unname(aperc_y_list), 
                        # text = unname(aperc_text_list), 
                        # textposition = rep(list("inside"), n_pos),
                        hovertemplate = unname(aperc_hover_list),
                        text = rep(list(""), n_pos)
                        ), 
                      as.list(0:(n_pos - 1))
                      )
  }
  
  # =====================================================================
  # PLOT 3: CITATION CONTRIBUTION % (Protected Flow)
  # =====================================================================
  total_cites <- sum(df_plot$Citations)
  cites_pdata <- df_plot %>% group_by(position_rank) %>% summarise(TotalCitations = sum(Citations), .groups = 'drop') %>% mutate(pcontrib = if (is.na(total_cites) || total_cites == 0) 0 else (TotalCitations / total_cites) * 100)
  
  if(nrow(cites_pdata) <= 0 || total_cites == 0){
    shinyjs::hide("cperc_plot")
  } else {
    shinyjs::show("cperc_plot")
    
    cperc_proxy <- plotlyProxy("cperc_plot", session)
    n_pos <- length(all_positions)
    
    cperc_vals <- sapply(all_positions, function(pos) {
      idx <- which(cites_pdata$position_rank == pos)
      val <- if (length(idx) == 1) cites_pdata$pcontrib[idx] else 0
      if (is.na(val) || is.nan(val)) 0 else val
    })
    
    if(sum(cperc_vals) > 0) cperc_vals <- (cperc_vals / sum(cperc_vals)) * 100
    
    cperc_x_list <- lapply(cperc_vals, function(v) list(v))
    cperc_y_list <- lapply(seq_len(n_pos), function(i) list("Citations"))
    # cperc_text_list <- lapply(seq_len(n_pos), function(i) {
    #   list(paste0("<b>Position:</b> ", all_positions[i], "<br><b>Contribution %:</b> ", round(cperc_vals[i], 1), "%"))
    # })
    
    cperc_hover_list <- lapply(seq_len(n_pos), function(i) {
      list(paste0(
        "<b>Position:</b> ", all_positions[i], "<br>",
        "<b>Contribution %:</b> ", round(cperc_vals[i], 1), "%",
        "<extra></extra>" 
      ))
    })
    
    plotlyProxyInvoke(cperc_proxy, 
                      "restyle", 
                      list(
                        x = unname(cperc_x_list), y = unname(cperc_y_list), 
                        # text = unname(cperc_text_list), 
                        # textposition = rep(list("inside"), n_pos)
                        hovertemplate = unname(cperc_hover_list),
                        text = rep(list(""), n_pos)
                        ), 
                      as.list(0:(n_pos - 1))
                      )
  }
}

render_skeleton_plots <- function(rv, df, output){
  
  all_positions <- c("First Author","Second Author","Co-Author","Corresponding Author")
  all_quartiles <- c("Q1","Q2","Q3","Q4", "NA")
  
  # ---------------------------------------------------------
  # 1. BUILD SAFE SKELETON DATA (Handles Empty/Null df)
  # ---------------------------------------------------------
  if (is.null(df) || nrow(df) <= 0) {
    rv$log_text <- paste(rv$log_text, "render_skeleton_plots(): No data. Rendering empty skeleton plots.", sep="<br>")
    warning("render_skeleton_plots(): No data. Rendering empty skeleton plots.")
    
    # Create a perfect zero-value grid so bar charts render empty axes without crashing
    full_grid <- expand.grid(Position = all_positions, Qscore = all_quartiles, stringsAsFactors = FALSE)
    agg_all <- full_grid %>%
      mutate(Count = 0L, 
             SumCitations = 0.0,
             Total_Position = 0L,
             Total_Citations = 0.0,
             Total_QCitations = 0.0) %>%
      mutate(Position = factor(Position, levels = all_positions),
             Qscore = factor(Qscore, levels = all_quartiles)) %>%
      arrange(Qscore, Position)
    
  } else {
    # Normal data processing
    df_ordered_debug <- df %>%
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
    
    full_grid <- expand.grid(Position = all_positions, Qscore = all_quartiles, stringsAsFactors = FALSE)
    
    agg_all <- full_grid %>%
      left_join(agg_all, by = c("Position","Qscore")) %>%
      mutate(Count = tidyr::replace_na(Count, 0L),
             SumCitations = tidyr::replace_na(SumCitations, 0.0))
    
    agg_all$Position <- factor(agg_all$Position, levels = all_positions)
    agg_all$Qscore <- factor(agg_all$Qscore, levels = all_quartiles)
    
    agg_all <- agg_all %>%
      group_by(Position) %>% mutate(Total_Position = sum(Count, na.rm = TRUE)) %>% ungroup() %>%
      group_by(Position) %>% mutate(Total_Citations = sum(SumCitations, na.rm = TRUE)) %>% ungroup() %>%
      group_by(Qscore) %>% mutate(Total_QCitations = sum(SumCitations, na.rm = TRUE)) %>% ungroup() %>%
      mutate(Position = factor(Position, levels = all_positions), Qscore = factor(Qscore, levels = all_quartiles)) %>%
      arrange(Qscore, Position)
  }
  
  # ---------------------------------------------------------
  # 2. DEFINING STYLES
  # ---------------------------------------------------------
  position_colors <- c("First Author" = "#D6A77A", "Second Author" = "#B66BA4", "Co-Author" = "#2E8B57", "Corresponding Author" = "#4B8BBE")
  position_border_colors <- c("First Author" = "#ff9f40ff", "Second Author" = "#9966ffff", "Co-Author" = "#4bc093ff", "Corresponding Author" = "#36a2ebff")
  position_45plots <- c("First Author" = "#dbf2f2", "Second Author" = "#ebe0ff", "Co-Author" = "#ffe9d4", "Corresponding Author" = "#d7ecfb")
  position_45plots_border <- c("First Author" = "#8fcedb", "Second Author" = "#cfaeec", "Co-Author" = "#fbb774", "Corresponding Author" = "#7dc2f1")
  quartile_alpha <- c("Q1" = 0.9, "Q2" = 0.70, "Q3" = 0.50, "Q4" = 0.30, "NA" = 0.2)
  
  # ---------------------------------------------------------
  # 3. RENDER ALL SKELETONS (These will render empty but VALID objects)
  # ---------------------------------------------------------
  # output$acounts_plot <- renderPlotly({
  #   plot_ly(
  #     data = agg_all, x = ~Position, y = ~Count, split = ~Qscore, type = "bar",
  #     hoverinfo = "text", hovertext = ~paste("Position:", Position, "<br>Quartile:", Qscore, "<br>Count:", Count, "<br>Total:", Total_Position),
  #     marker = list(color = ~position_colors[Position], opacity = ~quartile_alpha[Qscore], line = list(width = 1, color = ~position_border_colors[Position]))
  #   ) %>%
  #     layout(
  #       title = list(text = "Publication Count based on Authorship with Journal Rank Categorization", x = 0.5),
  #       barmode = "stack", xaxis = list(title = "", tickangle = 15), yaxis = list(title = ""),
  #       showlegend = FALSE, transition = list(duration = 1000, easing = "ease-in-out")
  #     )
  # })
  print(paste("full_grid:", nrow(full_grid)))
  # print(agg_all$Total_Position)
  print(str(agg_all))
  output$acounts_plot <- renderPlotly({
    plot_ly(
      data = agg_all, 
      x = ~Position, 
      y = ~Count, 
      split = ~Qscore, 
      type = "bar",
      # hovertemplate is better than hovertext for proxies
      # hovertemplate = "<b>%{x}</b><br>Quartile: %{fullData.name}<br>Publications: %{y}<extra></extra>",
      # customdata = ~Total_Position, # Map the total here
      # hovertemplate = paste0(
      #   "<b>%{x}</b><br>",
      #   "Quartile: %{fullData.name}<br>",
      #   "Pubs in this Pos: %{y}<br>",
      #   "Total Pubs in %{fullData.name}: %{customdata}",
      #   "<extra></extra>"
      # ),
      marker = list(
        color = ~position_colors[Position], 
        opacity = ~quartile_alpha[Qscore], 
        line = list(width = 1.2, color = ~position_border_colors[Position])
      )
    ) %>%
      layout(
        title = list(text = "Publication Count based on Authorship with Journal Rank Categorization", x = 0.5),
        barmode = "stack", 
        xaxis = list(title = "", tickangle = 15, categoryorder = "array", categoryarray = all_positions), 
        yaxis = list(title = "", range=c(0,1), rangemode = "nonnegative", autorange = TRUE),
        showlegend = FALSE,
        autosize = TRUE
      )
  })
  outputOptions(output, "acounts_plot", suspendWhenHidden = FALSE)
  
  # output$ccounts_plot <- renderPlotly({
  #   plot_ly(
  #     data = agg_all, x = ~Position, y = ~SumCitations, split = ~Qscore, type = "bar",
  #     hoverinfo = "text", hovertext = ~paste("Position:", Position, "<br>Position Citations:", Total_Citations, "<br>Quartile:", Qscore, "<br>Quartile Citations:", Total_QCitations),
  #     marker = list(color = ~position_colors[Position], opacity = ~quartile_alpha[Qscore], line = list(width = 1, color = ~position_border_colors[Position]))
  #   ) %>%
  #     layout(
  #       title = list(text = "Citation Count based on Authorship with Journal Rank Categorization", x = 0.5),
  #       barmode = "stack", xaxis = list(title = "", tickangle = 15), yaxis = list(title = ""),
  #       showlegend = FALSE, transition = list(duration = 1000, easing = "ease-in-out")
  #     )
  # })
  print(agg_all$Total_QCitations)
  output$ccounts_plot <- renderPlotly({
    plot_ly(
      data = agg_all, 
      x = ~Position, 
      y = ~SumCitations, 
      split = ~Qscore, 
      type = "bar",
      # hovertemplate = "<b>%{x}</b><br>Quartile: %{fullData.name}<br>Citations: %{y}<extra></extra>",
      # customdata = ~Total_QCitations, # Map the total here
      # hovertemplate = paste0(
      #   "<b>%{x}</b><br>",
      #   "Quartile: %{fullData.name}<br>",
      #   "Citations in this Pos: %{y}<br>",
      #   "Total Citations in %{fullData.name}: %{customdata}",
      #   "<extra></extra>"
      # ),
      marker = list(
        color = ~position_colors[Position], 
        opacity = ~quartile_alpha[Qscore], 
        line = list(width = 1.2, color = ~position_border_colors[Position])
      )
    ) %>%
      layout(
        title = list(text = "Citation Count based on Authorship with Journal Rank Categorization", x = 0.5),
        barmode = "stack", 
        xaxis = list(title = "", tickangle = 15, categoryorder = "array", categoryarray = all_positions), 
        yaxis = list(title = "", range=c(0,1), rangemode = "nonnegative", autorange = TRUE),
        showlegend = FALSE,
        autosize = TRUE
      )
  })
  outputOptions(output, "ccounts_plot", suspendWhenHidden = FALSE)
  
  output$cdist_plot <- renderPlotly({
    p <- plot_ly()
    for (i in seq_along(all_positions)) {
      pos <- all_positions[i]
      fill_col <- position_colors[pos]
      border_col <- position_border_colors[pos]
      
      # FIX: Providing numeric(0) explicitly makes Plotly serialize this as `[]` instead of `undefined`
      p <- add_trace(p,
                     x = numeric(0), y = numeric(0), name = pos,
                     type = "violin", orientation = "v", width = 0.7, scalemode = "width", spanmode = "hard",
                     box = list(visible = FALSE), meanline = list(visible = TRUE), points = FALSE, showlegend = FALSE, hoverinfo = "none")
      
      p <- add_trace(p,
                     x = numeric(0), y = numeric(0), name = pos, type = "scatter", mode = "markers",
                     marker = list(size = numeric(0), color = border_col, line = list(width = 0.5, color = border_col)),
                     text = character(0), hoverinfo = "text", showlegend = FALSE)
    }
    
    p %>% layout(
      title = list(text = "Citation Distribution based on Authorship (Log Scale)", x = 0.5),
      yaxis = list(title = "log(1 + Citations)"),
      xaxis = list(title = "", tickmode = "array", tickvals = seq_along(all_positions), ticktext = all_positions),
      showlegend = FALSE
    )
  })
  outputOptions(output, "cdist_plot", suspendWhenHidden = FALSE)
  
  output$aperc_plot <- renderPlotly({
    p <- plot_ly()
    for (pos in all_positions) {
      p <- add_trace(p, type = "bar", orientation = "h", x = 0, y = "Publications", name = pos,
                     marker = list(color = position_45plots[pos], line = list(color = position_45plots_border[pos], width = 1)),
                     hoverinfo = "text", showlegend = FALSE)
    }
    p %>% layout(barmode = "stack", xaxis = list(title = "", range = c(0, 100), dtick = 10, showgrid = TRUE, ticksuffix = "%"),
                 yaxis = list(title = "", showticklabels = FALSE, fixedrange = TRUE),
                 margin = list(l = 10, r = 10, t = 50, b = 30), title = list(text = "Author Contribution in % based on Authorship", x = 0.5))
  })
  outputOptions(output, "aperc_plot", suspendWhenHidden = FALSE)
  
  output$cperc_plot <- renderPlotly({
    p <- plot_ly()
    for (pos in all_positions) {
      p <- add_trace(p, type = "bar", orientation = "h", x = 0, y = "Citations", name = pos,
                     marker = list(color = position_45plots[pos], line = list(color = position_45plots_border[pos], width = 0.8)),
                     hoverinfo = "text", showlegend = FALSE)
    }
    p %>% layout(barmode = "stack", xaxis = list(title = "", range = c(0, 100), dtick = 10, ticksuffix = "%"),
                 yaxis = list(title = "", showticklabels = FALSE, fixedrange = TRUE),
                 margin = list(l = 20, r = 20, t = 50, b = 30), title = list(text = "Citation Contribution in % based on Authorship", x = 0.5))
  })
  outputOptions(output, "cperc_plot", suspendWhenHidden = FALSE)
  
  # Finally, hide the parent containers visually, but now the JS instances are safely waiting for the Proxy!
  if(is.null(df) || nrow(df) <= 0) {
    # shinyjs::hide("sh_index")
    # shinyjs::hide("summary_table")
    shinyjs::hide("acounts_plot")
    shinyjs::hide("ccounts_plot")
    shinyjs::hide("cdist_plot")
    shinyjs::hide("aperc_plot")
    shinyjs::hide("cperc_plot")
    shinyjs::hide("network_filtered")
  }
}
