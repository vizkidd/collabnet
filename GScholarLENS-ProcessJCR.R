
# use a multisession plan so futures run in background R sessions
plan(multisession)

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
