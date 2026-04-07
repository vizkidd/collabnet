suppressPackageStartupMessages(require(readxl))
suppressPackageStartupMessages(require(future))

# is_WASM <- grepl(pattern="wasm",x=Sys.info()["machine"])
# # use a multisession plan so futures run in background R sessions
# if(!is_WASM){
  future::plan(future::multisession)
# future::plan(future::multicore)
# }else{
#   future::plan(future::sequential)
# }

source("GScholarLENS-Data2GLENS.R", local=TRUE)


########JOURNAL MATCHING AND SCORING HELPERS
# ---------------------------
# Read JCR file and pick relevant columns
# ---------------------------
read_jcr <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx","xls")) {
    j <- readxl::read_excel(path)
  } else if (ext %in% c("csv","txt")) {
    j <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Unsupported JCR file type")
  }
  #Rename 8th column to Qscore
  colnames(j)[8] <- "Qscore"
  # Normalize column names and select the ones requested (if present)
  names(j) <- str_trim(names(j))
  want <- c("Name", "Abbr Name","ISSN","EISSN","JIF","JIF5Years","Category","Qscore","Rank","Rank out of Total Journals")
  present <- want[want %in% names(j)]
  # print(colnames(j))
  # print(present)
  j2 <- j[, present, drop = FALSE]
  # rename to consistent names
  colnames(j2) <- make.names(colnames(j2))
  # ensure Rank columns are numeric if present
  if ("Rank" %in% names(j2)) j2$Rank <- suppressWarnings(as.numeric(j2$Rank))
  if ("Rank.out.of.Total.Journals" %in% names(j2)) j2$Rank.out.of.Total.Journals <- suppressWarnings(as.numeric(j2$Rank.out.of.Total.Journals))
  j2
}


jcr_base <- "2024-JCR_IMPACT_FACTOR"
jcr_file_xlsx <- paste0(jcr_base, ".xlsx")
jcr_file_xls  <- paste0(jcr_base, ".xls")
jcr_file_csv  <- paste0(jcr_base, ".csv")

jcr_path <- NULL
if (file.exists(jcr_file_xlsx)) jcr_path <- jcr_file_xlsx
if (is.null(jcr_path) && file.exists(jcr_file_xls)) jcr_path <- jcr_file_xls
if (is.null(jcr_path) && file.exists(jcr_file_csv)) jcr_path <- jcr_file_csv

# warning(paste("Current Directory:", getwd()))
# warning("Files in VFS:")
# warning(paste(list.files(all.files = TRUE), collapse=", "))

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

jcr$Name_norm <- unname(sapply(jcr$Name, function(x) normalize_journal(x)))
# rv$glens_etable_final$Name_norm <- sapply(rv$glens_etable_final$Journal, function(x) normalize_journal(x))

jcr_names_norm <- jcr |>
  select(Name, Name_norm, JIF5Years, Qscore) 

# print(str(jcr_names_norm))