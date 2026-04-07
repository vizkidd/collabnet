# server.R (or inside server function)
# require(shiny)
suppressPackageStartupMessages(require(shinyjs))
# suppressPackageStartupMessages(require(webr))
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
# require(munsell)
suppressPackageStartupMessages(require(readxl))
suppressPackageStartupMessages(require(tools))
suppressPackageStartupMessages(require(utils))

is_WASM <- grepl(pattern="wasm",x=Sys.info()["machine"])
# # use a multisession plan so futures run in background R sessions
# if(!is_WASM){
  future::plan(future::multisession)
# future::plan(future::multicore)
# }else{
#   future::plan(future::sequential)
# }

# # Add this to your server function to see the actual file list in the browser console
# message(paste("Current Directory:", getwd()))
# message("Files in VFS:")
# message(list.files(all.files = TRUE))

#source("server.R", local = TRUE)
# safe_source("server.R")
source("GScholarLENS-DOI2Data.R", local = TRUE)
source("GScholarLENS-ORCID2Data.R", local = TRUE)
source("GScholarLENS-SCOPUS2Data.R", local = TRUE)
source("GScholarLENS-Data2GLENS.R", local = TRUE)
source("GScholarLENS-PlotGLENS.R", local = TRUE)

font_add(
  family = "schibsted-grotesk",
  regular = "www/fonts/SchibstedGrotesk.ttf"
)
theme_set(theme_minimal(base_family = "schibsted-grotesk"))
showtext_auto()

# # 1. Verify showtext is active
# print(showtext::showtext_auto())
# # 2. List all fonts R currently 'sees' via systemfonts
# library(systemfonts)
# print(match_fonts("Schibsted Grotesk"))
# # 3. Check ggplot2 default
# print(theme_get()$text$family)
# # Check if R actually registered the alias
# # This lists all available font families registered in the session
print(sysfonts::font_families())
# # This checks if your specific font is in that list
# print("schibsted-grotesk" %in% sysfonts::font_families())
# # Check the file path visibility (Shiny looks relative to the project root)
# print(file.exists("www/fonts/SchibstedGrotesk.ttf"))
# return()

update_geom_defaults("text", list(family = "schibsted-grotesk"))
update_geom_defaults("label", list(family = "schibsted-grotesk"))

shinyApp(ui = ui, server = server, options = list(port=2447))
#shinyApp(ui = ui, server = server, options = list(port=structure("/tmp/glens.sock", mask=385, group="www-data")))
