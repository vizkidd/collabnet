# P10-A: edition switch (full | free | demo). editions/build_editions.R writes edition.R (constants only) into each
# stripped app dir; code for modules that are off is physically removed there. From source with no edition.R the
# app is the full edition, so the dev workflow and tests are unchanged.
if (!exists("COLLABNET_EDITION") && file.exists("edition.R")) source("edition.R", local = TRUE)
if (!exists("COLLABNET_EDITION")) COLLABNET_EDITION <- "full"
if (!exists("COLLABNET_MODULES")) COLLABNET_MODULES <- c(console = TRUE, ai = TRUE, export = TRUE, file_import = TRUE,
                                                         lookup = TRUE, fetch = TRUE, fetch_bridge = TRUE, ads = FALSE, bmc = FALSE)
if (!exists("COLLABNET_AD_LEVEL")) COLLABNET_AD_LEVEL <- 0L

edition_has <- function(module) isTRUE(COLLABNET_MODULES[module][[1]])

cn_ad_slot <- function(slot, min_level = 1L) NULL   # P10-A8: inert stub; the real one exists only where ads is on

cn_bmc_banner <- function() NULL   # P10-A8: inert stub; the real one exists only where bmc is on
# Demo: support link (no countdown, unlocks nothing). Plain link -- no third-party widget script.
cn_bmc_banner <- function() {
  if (!edition_has("bmc")) return(NULL)
  shiny::tags$div(class = "cn-bmc-banner", style = "margin: 10px 0; text-align: center;",
                  shiny::tags$a(href = "https://buymeacoffee.com/vishveshkac", target = "_blank", rel = "noopener noreferrer",
                                class = "btn btn-warning btn-sm", shiny::icon("mug-hot"), " Support CollabNET (Buy Me a Coffee)"))
}

cn_fetch_bridge_ui <- function() NULL   # P10-A8: inert stub; the real one exists only where fetch_bridge is on
