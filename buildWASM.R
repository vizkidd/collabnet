# require(shinylive)
# require(httpuv)
# # install.packages("pak")
# # pak::pak("r-wasm/rwasm")
# require(rwasm)
# # Update the R package
# pak::pak("posit-dev/r-shinylive")
# # Force a fresh download of the WebAssembly assets
# shinylive::assets_download()

# 1. Create a clean staging directory
if(fs::dir_exists("wasm_build"))
  fs::dir_delete("wasm_build")
if(fs::dir_exists("staging_app"))
  fs::dir_delete("staging_app")
fs::dir_create("staging_app")

# 2. Copy only required files (e.g., app.R, your sourced scripts, and the Excel file)
app_files <- c("ui.R", "server.R", "app.R")#, "2024-JCR_IMPACT_FACTOR.xlsx")
# Also include your helper scripts
helper_scripts <- list.files(pattern = "GScholarLENS.*\\.R")

# cat("WASM <- TRUE", file="staging_app/app.R")
cat(unlist(lapply(c(helper_scripts, app_files), function(x){
  return(grep(pattern = "source\\(.*\\)", x = readLines(x), value = TRUE, invert = T))
})), file="staging_app/app.R", sep="\n")

fs::dir_copy("www/","staging_app/")
# fs::file_copy(c(app_files, helper_scripts), "staging_app/")
fs::file_copy("2024-JCR_IMPACT_FACTOR.xlsx", "staging_app/")

# rwasm::add_pkg("/media/everest/disk01/vishvesh/QuickBLAST/", repo_dir = "./repo") 

shinylive::export(appdir = "./staging_app/", destdir = "./wasm_build/", wasm_packages=T, quiet=F)

#Download and install the latest version of webR from github

# Load required package for API parsing (install.packages("jsonlite") if needed)
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The 'jsonlite' package is required but not installed.")
}

# 1. Define your target directory (relative to your working directory)
target_dir <- "wasm_build/shinylive/webr"

# Create the target directory if it doesn't exist
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

# 2. Query the GitHub API for the latest webR release
message("Querying GitHub API for the latest webR release...")
api_url <- "https://api.github.com/repos/r-wasm/webr/releases/latest"
release_data <- tryCatch({
  jsonlite::fromJSON(api_url)
}, error = function(e) {
  stop("Failed to fetch release data from GitHub API. Check your internet connection or API rate limits.")
})

# 3. Extract the download URL for the .tar.gz asset
assets <- release_data$assets
tar_asset <- assets[grepl("webr-.*\\.tar\\.gz$", assets$name), ]

if (nrow(tar_asset) == 0) {
  stop("Could not find a .tar.gz file in the latest release assets.")
}

download_url <- tar_asset$browser_download_url[1]
tar_filename <- tar_asset$name[1]

# 4. Set up an isolated temporary workspace
tmp_dir <- tempdir()
dest_file <- file.path(tmp_dir, tar_filename)
extract_dir <- file.path(tmp_dir, "webr_extracted")

# Clean up previous extraction attempts in temp if they exist
if (dir.exists(extract_dir)) unlink(extract_dir, recursive = TRUE)
dir.create(extract_dir)

# 5. Download the release asset
message(sprintf("Downloading %s...", tar_filename))
download.file(url = download_url, destfile = dest_file, mode = "wb", quiet = FALSE)

# 6. Extract the archive
message("Extracting archive...")
untar(tarfile = dest_file, exdir = extract_dir)

# 7. Locate the extracted contents
# webR usually extracts into a subfolder named "webr" inside the exdir
extracted_subdirs <- list.dirs(extract_dir, recursive = FALSE)
webr_folder <- extracted_subdirs[grepl("webr", basename(extracted_subdirs))]

# Fallback in case it extracted everything directly into the root of extract_dir
if (length(webr_folder) == 0) {
  webr_folder <- extract_dir
} else {
  webr_folder <- webr_folder[1]
}

# 8. Copy files over to your project directory
message(sprintf("Syncing files to %s...", target_dir))
files_to_copy <- list.files(webr_folder, full.names = TRUE)

# Copy each item (files and directories) into the target directory
for (item in files_to_copy) {
  file.copy(
    from = item, 
    to = target_dir, 
    recursive = TRUE, 
    overwrite = TRUE
  )
}

# 9. Clean up
message("Cleaning up temporary files...")
unlink(dest_file)
unlink(extract_dir, recursive = TRUE)

message("Success! The latest version of webR has been deployed.")

httpuv::runStaticServer("./wasm_build/", headers = list(
  "Cross-Origin-Opener-Policy" = "same-origin",
  "Cross-Origin-Embedder-Policy" = "require-corp"
)) #host = "0.0.0.0"

# # httpuv::runServer(
# #   host = "127.0.0.1", port = 7446,
# #   app = list(
# #     staticPaths = list(
# #       "/" = httpuv::staticPath(
# #         "./wasm_build/",
# #         headers = list(
# #           "Cross-Origin-Opener-Policy" = "same-origin",
# #           "Cross-Origin-Embedder-Policy" = "require-corp"
# #         )
# #       )
# #     ),
# #     call = function(req) {
# #       list(
# #         status = 404L, 
# #         headers = list("Content-Type" = "text/plain"), 
# #         body = paste(req, "404 - Not Found")
# #       )
# #     }
# #   )
# # )