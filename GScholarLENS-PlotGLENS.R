suppressPackageStartupMessages(require(readxl))
suppressPackageStartupMessages(require(tools))
suppressPackageStartupMessages(require(stringr))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(tidyr))
suppressPackageStartupMessages(require(ggplot2))

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
        "<i>Total Collaborations: ", total_connections, "</i>",
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
