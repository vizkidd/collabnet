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
suppressPackageStartupMessages(require(future.callr))

is_WASM <- grepl(pattern="wasm",x=Sys.info()["machine"])
# # use a multisession plan so futures run in background R sessions
# if(!is_WASM){
# }else{
if (is_WASM) {
  future::plan(future::sequential)
} else {
  future::plan(future::multisession)
}

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

tryCatch({
  if (file.exists("www/fonts/SchibstedGrotesk.ttf")) {
    font_add(
      family = "schibsted-grotesk",
      regular = "www/fonts/SchibstedGrotesk.ttf"
    )
    theme_set(theme_minimal(base_family = "schibsted-grotesk"))
    showtext_auto()
  } else {
    message("Font file www/fonts/SchibstedGrotesk.ttf not found; using default font.")
  }
}, error = function(e) {
  message("Font loading warning: ", conditionMessage(e))
})

options(shiny.maxRequestSize = 1024 * 1024^2) #1GB

if ("schibsted-grotesk" %in% sysfonts::font_families()) {
  update_geom_defaults("text", list(family = "schibsted-grotesk"))
  update_geom_defaults("label", list(family = "schibsted-grotesk"))
}

shinyApp(ui = ui, server = server, options = list(port=2447))
#shinyApp(ui = ui, server = server, options = list(port=structure("/tmp/glens.sock", mask=385, group="www-data")))
