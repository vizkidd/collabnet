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
require(sodium)
require(uuid)
require(openssl)
require(xfun)

source("./ui.R")
source("./server.R")
source("./GScholarLENS-DOI2Data.R")
source("./GScholarLENS-ORCID2Data.R")
source("./GScholarLENS-SCOPUS2Data.R")
source("./GScholarLENS-Data2GLENS.R")
source("./GScholarLENS-PlotGLENS.R")

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