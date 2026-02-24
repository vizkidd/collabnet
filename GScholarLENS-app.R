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


font_add(
  family = "schibsted-grotesk",
  regular = "www/fonts/SchibstedGrotesk.ttf"
)

showtext_auto()

# use a multisession plan so futures run in background R sessions
plan(multisession)

source("./GScholarLENS-DOI2Data.R")
source("./GScholarLENS-ORCID2Data.R")
source("./GScholarLENS-Data2GLENS.R")
source("./GScholarLENS-PlotGLENS.R")

#Flow functions
extend_input_table <- function(rv) {
  
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
  df <- rv$glens_etable_final
  
  # Strict H-index (as you changed)
  compute_h_index <- function(citations_vec) {
    v <- citations_vec[!is.na(citations_vec)]
    if (length(v) == 0) return(0L)
    v <- sort(v, decreasing = TRUE)
    h <- 0L
    for (i in seq_along(v)) {
      if (v[i] > i) h <- i else break
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
    h <- compute_h_index(sub$Adjusted_Citations)
    results[[pos]] <- list(
      h_index = h,
      n_papers = nrow(sub)
    )
  }
  
  # Classical H-indices
  h_cites <- compute_h_index(df$Citations)
  h_adjcites <- compute_h_index(df$Adjusted_Citations)
  
  rv$summary_table <- tibble(
    Position = positions,
    H_index = sapply(results, function(x) x$h_index),
    Num_papers = sapply(results, function(x) x$n_papers)
  )
  
  rv$summary_table <- rv$summary_table %>%
    add_row(Position = "h-index(Citations)",
            H_index = h_cites,
            Num_papers = nrow(df)) %>%
    add_row(Position = "h-index(Adj.Citations)",
            H_index = h_adjcites,
            Num_papers = nrow(df))
  
  # Correct Sh-index: sum ONLY the 4 positional H indices
  rv$sh_index <- sum(rv$summary_table$H_index[
    rv$summary_table$Position %in% positions
  ], na.rm = TRUE)
  
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

    # plotlyProxyInvoke(
    #   aperc_proxy,
    #   "restyle",
    #   list(y = list("Publications")),
    #   seq_along(all_positions) - 1
    # )

    n <- length(all_positions)
    # build values in TRACE ORDER
    aperc_vals <- sapply(all_positions, function(pos) {
      i <- which(pub_pdata$position_rank == pos)
      if (length(i) == 1) pub_pdata$pcontrib[i] else 0
    })
    
    # normalize to 100%
    aperc_vals <- aperc_vals / sum(aperc_vals) * 100
    
    # IMPORTANT: each attribute must be a list-of-lists
    aperc_x_list <- lapply(aperc_vals, function(v) list(v))
    print(aperc_x_list)
    aperc_y_list <- lapply(seq_len(n), function(i) list("Publications"))
    aperc_text_list <- lapply(seq_len(n), function(i) {
      list(
        paste0(
          "<b>Position:</b> ", all_positions[i],
          "<br><b>Contribution %:</b> ", round(aperc_vals[i], 1)
        )
      )
    })
    aperc_textpos_list <- lapply(seq_len(n), function(i) list("inside"))
    
    aperc_trace_idxs <- as.list(0:(n - 1))
    
    plotlyProxyInvoke(
      aperc_proxy,
      "restyle",
      list(
        x = I(aperc_x_list),
        y = I(aperc_y_list),
        text = aperc_text_list,
        textposition = aperc_textpos_list
      ),
      aperc_trace_idxs
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
  
    # plotlyProxyInvoke(
    #   cperc_proxy,
    #   "restyle",
    #   list(
    #     x = list(cites_pdata$pcontrib),   # SINGLE value
    #     y = "Citations",            # SINGLE shared category
    #     text = list(
    #       paste0(
    #         "<b>Position:</b> ", cites_pdata$position_rank,
    #         "<br><b>Contribution %:</b> ", cites_pdata$pcontrib
    #       )
    #     )
    #   ),
    #   0
    # )
  
    
  for (i in seq_along(all_positions)) {
    # row <- which(cites_pdata$position_rank == all_positions[i])
    # # print(cites_pdata[row,])
    # # print(cites_pdata$pcontrib[row])
    # if (length(row) == 0) {
    #   # print("EMPTY ROW")
    #   plotlyProxyInvoke(
    #     cperc_proxy,
    #     "restyle",
    #     list(
    #       x = numeric(0),
    #       y = character(0),
    #       text = list(character(0))
    #     ),
    #     list(i - 1)
    #   )
    # } else {
      plotlyProxyInvoke(
        cperc_proxy,
        "restyle",
        list(
          x = list(cites_pdata$pcontrib[i]),   # SINGLE value
          y = list("Citations"),            # SINGLE shared category
          text = list(
            paste0(
              "<b>Position:</b> ", cites_pdata$position_rank[i],
              "<br><b>Contribution %:</b> ", cites_pdata$pcontrib[i]
            )
          )
        ),
        i - 1
      )
    # }

  }# End - for
  
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
          width = 0.25,
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
          width = 0.25,
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
        x = numeric(0),         # IMPORTANT: numeric(0) to make trace numeric
        y = character(0),       # single shared category
        name = pos,
        marker = list(
          color = position_colors[pos],
          line = list(color = position_border_colors[pos], width = 0.6)
        ),
        hoverinfo = "text",     # we'll provide text via restyle
        showlegend = FALSE
      )
    }
    
    rv$aperc_plot <- p %>%
      layout(
        barmode = "stack",
        xaxis = list(title = NULL, range = c(0,100), ticksuffix = "%"),
        yaxis = list(title = NULL, showticklabels = FALSE),
        title = list(text = "Author Contribution in % based on Authorship", x = 0.5)
      ) 
    rv$aperc_plot
  })
  
  output$cperc_plot <- renderPlotly({
    p <- plot_ly()
    for (pos in all_positions) {
      print(position_colors[pos])
      print(position_border_colors[pos])
      p <- add_trace(
        p,
        type = "bar",
        orientation = "h",
        x = numeric(0),
        y = character(0),
        name = pos,
        marker = list(
          color = position_colors[pos],
          line = list(color = position_border_colors[pos], width = 0.6)
        ),
        hoverinfo = "text",
        showlegend = FALSE
      )
    }
    
    rv$cperc_plot <- p %>%
      layout(
        barmode = "stack",   # <-- REQUIRED
        xaxis = list(title = NULL, range = c(0, 100)),
        yaxis = list(title = NULL, showticklabels = FALSE),
        title = list(
          text = "Citation Contribution in % based on Authorship",
          x = 0.5
        )
      )
    
    rv$cperc_plot
  })
  
} # End - Plot skeleton renders

#SHINY BLOCK
ui <- fluidPage( #bootstrapPage(
  shinyjs::useShinyjs(), 
  # numericInput('n', 'Number of obs', n),
  # textOutput("time"),
  textAreaInput(
    "doi_text",
    "DOI input:",
    value = ""
  ),
  textAreaInput(
    "author_list",
    "Author Name List (seperated by |) *<required>:",
    value = ""
  ),
  textAreaInput(
    "orcid_text",
    "ORCID input:",
    value = ""
  ),
  actionButton("submit_button", "Run GScholarLENS for DOI"),
  tags$br(),
  verbatimTextOutput("log"),
  shinyjs::hidden(
    sliderInput( 
      "year_slider", "Year Slider", 
      min = 0, max = 0, 
      value = c(0, 0),
      step = 1,          # Ensures only integer steps
      round = TRUE       # Ensures display values are integers
    )),
  tags$br(),
  tableOutput("orcid_table"),
  shinyjs::disabled(shiny::uiOutput("sh_index")),
  # tableOutput("data_table"),
  tableOutput("summary_table"),
  plotlyOutput("acounts_plot"),
  plotlyOutput("ccounts_plot"),
  plotlyOutput("cdist_plot"),
  plotlyOutput("aperc_plot"),
  plotlyOutput("cperc_plot"),
  DT::DTOutput("extended_table")
  # actionButton("update", "Show Time"),
)

# # Define the server code
server <- function(input, output, session) {
  rv <- reactiveValues(
    glens_input_table = data.frame(),
    glens_etable_final = data.frame(),
    glens_year_filtered = data.frame(),
    summary_table = data.frame(),
    sh_index = 0,
    is_glens_exec = F,
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
  
  #Slider Event
  observeEvent(input$year_slider, {
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
    rv$glens_year_filtered <- rv$glens_etable_final %>%
      filter(Year >= input$year_slider[1]) %>%
      filter(Year <= input$year_slider[2])
      # filter(dplyr::between(
      #   Year,
      #   input$year_slider[1],
      #   input$year_slider[2]
      # ))
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
          dom = 'Bfrtip',       # 2. Add 'B' to the layout (B = Buttons)
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print') # 3. Define buttons
        )
      )
    })
    
    output$sh_index <-  renderUI({
      HTML(paste("<b>Sh-Index:</b>", rv$sh_index))
    })
    
    plot_glens_table(rv, output, session)
    
    shinyjs::enable(id="year_slider")
  })
  #Submit Button Event
  observeEvent(input$submit_button, {   # same as bindEvent(input$submit_button)
    # basic input guard
    rv$is_glens_exec <- T
    rv$log_text <- ""
    
    # check_orcid_input <- F
    # input_is_orcid <- F
    if (is.null(input$author_list) || stringi::stri_isempty(input$author_list)) {
      rv$log_text <- paste(rv$log_text, "Author list required.\n")
      # #INPUT IS PROLLY ORCID
      # check_orcid_input <- T
      output$log <- renderText({rv$log_text})
      shinyjs::enable(id = "submit_button")
      rv$is_glens_exec <- F
      req(input$author_list)
      return()
    }
    
    shinyjs::disable(id = "submit_button")
    shinyjs::hide(id="year_slider")  
    
    if (is.null(input$doi_text) || stringi::stri_isempty(input$doi_text)) {
      rv$log_text <- paste(rv$log_text, "No DOIs provided in Input.\n")
      #INPUT IS PROLLY ORCID
      # check_orcid_input <- T
    }
    orcid_list <- str_split(input$orcid_text, "\n")[[1]]
    # print(orcid_list)
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
        
        res <- tryCatch(GET(url=paste0("https://pub.orcid.org/v3.0/", orcid_list[x], "/works"),add_headers(Accept = "application/xml"),
                            user_agent(user_agent_str),
                            timeout(30)), error = function(e) e)
        
        print(res)
        xml_txt <- NULL
        if (inherits(res, "response") && status_code(res) == 200) {
          # extract as text
          xml_txt <- content(res, as = "text", encoding = "UTF-8")
        }else{
          rv$log_text <- paste(rv$log_text, "Couldn't find ORC-ID:", orcid_list[x],"\nResponse:", res,"\nStatus Code:", status_code(res))
          output$log <- renderText({rv$log_text})
          return()
        }
        
        xml_vec <- read_xml(xml_txt)
        xml_vec_ns <- xml_ns(xml_vec)
        xml_groups <- xml_find_all(xml_vec, ".//activities:group", xml_vec_ns)
        # print(length(xml_vec))
        # print(length(xml_groups))
        
        orcid_df <- map_dfr(xml_groups, function(g) {
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
        
        return(orcid_df)
      }, simplify = F))
      
      # print(orcid2doi_table)
      
      
       if(nrow(orcid2doi_table) <= 0){
         rv$log_text <-  paste(rv$log_text, "\nError: Could not find data for OCR-ID(s).")
         output$log <- renderText({ rv$log_text })
         shinyjs::enable(id = "submit_button")
         rv$is_glens_exec <- F
         return()
       }
       orcid2doi_table[is.na(orcid2doi_table$external_id_url), c("external_id_url")] <- orcid2doi_table[is.na(orcid2doi_table$external_id_url), c("external_id_value")]
       doi_lines <- c(doi_lines, orcid2doi_table$external_id_url) #strsplit("DOIs from the API", "\n")[[1]]
    }
    
    
    if (!is.null(input$doi_text) && !stringi::stri_isempty(input$doi_text)) {
      rv$log_text <- paste(rv$log_text, "DOI(s) provided as input.\n")
      doi_lines <- c(doi_lines, strsplit(input$doi_text, "\n")[[1]])
    }

    if(length(doi_lines) <= 0){
      rv$log_text <- paste(rv$log_text, "Cannot fetch DOI(s) for any input.\n")
      output$log <- renderText({rv$log_text})
      shinyjs::enable(id = "submit_button")
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
    progress <- Progress$new(session, min = 0, max = doi_count)
    progress$set(message = "Calculation in progress", detail = "Starting...", value = 0)
    
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
    maybe_update_log <- function() {
      now <- Sys.time()
      if ((processed_counter %% update_every_n == 0L) ||
          as.numeric(difftime(now, last_log_time, units = "secs")) >= update_every_secs ||
          processed_counter == doi_count) {
        # update text
        output$log <- renderText({
          sprintf("Processed %d/%d DOIs — found %d result rows so far",
                  processed_counter, doi_count, found_counter)
        })
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
          print(e)
          # NULL
          return(NULL)
        })
      }) %...>% (function(res_df) {
        # this runs on the main R session when the future resolves
        # print(res_df)
        processed_counter <<- processed_counter + 1L
        if (!is.null(res_df) && nrow(res_df) > 0) {
          accumulated[[length(accumulated) + 1]] <<- res_df
          found_counter <<- found_counter + nrow(res_df)
          # output$log <- renderText(paste("Found",found_counter,"DOIs"))
        }
        # increment the progress bar (non-blocking)
        progress$inc(1)
        maybe_update_log()
        
        # resolve to something useful for the final aggregator
        list(index = i, doi = doi, result = res_df)
      }) %...!% (function(err){
        # on future error: increment processed and progress, update log if needed
        processed_counter <<- processed_counter + 1L
        progress$inc(1)
        maybe_update_log()
        # return a list showing error
        list(index = i, doi = doi, result = NULL, error = conditionMessage(err))
      })
    })
    
    # Use promise_all to wait until all DOI futures finish.
    # promise_all accepts a named list; using .list argument
    promise_all(.list = promises_list) %...>% (function(all_results) {
      # all_results is a list of resolved values from each DOI promise
      # combine accumulated results (we also have them in accumulated list)
      # glens_input_table <- data.frame()
      if (length(accumulated) > 0) {
        rv$glens_input_table <- unique(bind_rows(accumulated))
        # output$data_table <- renderTable({
        #   glens_input_table
        # }, striped = TRUE)
      } 
      # else {
      #   output$data_table <- renderTable({
      #     data.frame()   # empty
      #   })
      # }
      
      # final, guaranteed log update
      rv$log_text <- paste(rv$log_text,sprintf("Done. Processed %d DOIs. Found %d result rows.", doi_count, found_counter))
      output$log <- renderText({
        rv$log_text
      })
      
      # print(glens_input_table)
      # output$data_table <- renderTable(glens_input_table, striped = TRUE)
      
      #SCRIPT 2 STARTS HERE
      target_variants <- unlist(stringr::str_split(input$author_list, "\\|"))
      target_variants <- str_trim(target_variants)
      target_variants <- target_variants[target_variants != ""]
      # print(target_variants)
      # print(colnames(rv$glens_input_table))
      
      # Normalize variants
      rv$target_variants_norm <- list()
      for (v in target_variants) {
        vn <- normalize_name(v)
        parts <- extract_parts(vn)
        rv$target_variants_norm[[v]] <- list(norm = vn, parts = parts)
      }
      rv$author_match_regex <- build_name_regex_for_variants(target_variants)
      
      extend_input_table(rv)
      
      rv$glens_year_filtered <- rv$glens_etable_final
      
      if(nrow(rv$glens_year_filtered) <= 0){
        #No names were matched. return
        output$log <- renderText(sprintf("No names were matched."))
        shinyjs::enable(id = "submit_button")
        progress$close()
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
      #Enable button after all 3 script flows end
      shinyjs::enable(id = "submit_button")
      progress$close()
      # NULL
      return(NULL)
    }) %...!% (function(err) {
      # overall failure handler
      progress$close()
      print(conditionMessage(err))
      output$log <- renderText(sprintf("Failed: %s", conditionMessage(err)))
      shinyjs::enable(id = "submit_button")
      # NULL
      return(NULL)
    })
    
    # immediately show a short message so user sees something while promises run
    output$log <- renderText(sprintf("Started processing %d DOIs...", doi_count))
  })
}


jcr_base <- "2024-JCR_IMPACT_FACTOR"
jcr_file_xlsx <- paste0(jcr_base, ".xlsx")
jcr_file_xls  <- paste0(jcr_base, ".xls")
jcr_file_csv  <- paste0(jcr_base, ".csv")

jcr_path <- NULL
if (file.exists(jcr_file_xlsx)) jcr_path <- jcr_file_xlsx
if (is.null(jcr_path) && file.exists(jcr_file_xls)) jcr_path <- jcr_file_xls
if (is.null(jcr_path) && file.exists(jcr_file_csv)) jcr_path <- jcr_file_csv

if (is.null(jcr_path)) {
  warning("Cannot find '2024-JCR_IMPACT_FACTOR(.xlsx/.csv)' in working directory.\n")
  # jcr_path <- readline(prompt = "Enter full path to JCR file (xlsx or csv): ")
  # jcr_path <- str_trim(jcr_path)
  warning("JCR file not found. Exiting.")
  return()
} else {
  cat("Found JCR file:", jcr_path, "\n")
}
jcr <- read_jcr(jcr_path)

# If JIF columns exist, ensure numeric
if ("JIF" %in% names(jcr)) jcr$JIF <- suppressWarnings(as.numeric(jcr$JIF))
if ("JIF5Years" %in% names(jcr)) jcr$JIF5Years <- suppressWarnings(as.numeric(jcr$JIF5Years))

jcr$Name_norm <- sapply(jcr$Name, function(x) normalize_journal(x))
# rv$glens_etable_final$Name_norm <- sapply(rv$glens_etable_final$Journal, function(x) normalize_journal(x))

jcr_names_norm <- jcr |>
  select(Name, Name_norm, JIF5Years, Qscore) 

shinyApp(ui = ui, server = server, options = list(port=2447))
