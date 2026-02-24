require(readxl)
require(tools)
require(stringr)
require(dplyr)
require(tidyr)
require(ggplot2)

# ---------------------------
# Aggregate counts and citations by Position × Quartile
# ---------------------------
# We'll produce a long table with columns: Position, Quartile, Count, SumCitations
make_agg <- function(df, pos_col, pos_label) {
  df$Qscore <- df$Qscore %>% replace_na('NA')
  # print(colnames(df))
  # print(df %>% select(JIF5Years,Qscore,First_Author,Second_Author,Co_Author,Corresponding_Author) ) #User_Journal,Name_norm,
  # print(df %>% select(JIF5Years,Qscore,First_Author,Second_Author,Co_Author,Corresponding_Author) %>% #User_Journal,Name_norm,
  #         filter(!is.na(.data[[pos_col]]) & as.numeric(.data[[pos_col]]) == 1))
  # # stop()
  
  return( df %>%
    filter(!is.na(.data[[pos_col]]) & as.numeric(.data[[pos_col]]) == 1) %>%
    # replace_na('NA') %>%
    group_by(Qscore, .drop = F) %>%
    summarise(
      Count = n(),
      # SumCitations = sum(suppressWarnings(as.numeric(Adjusted_Citations)), na.rm = TRUE),
      SumCitations = sum(suppressWarnings(as.numeric(Citations)), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Position = pos_label) )
}
