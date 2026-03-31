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
shinylive::export(appdir = "./staging_app/", destdir = "./wasm_build/")
httpuv::runStaticServer("./wasm_build/")