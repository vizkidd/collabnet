suppressPackageStartupMessages(require(xml2))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(purrr))
suppressPackageStartupMessages(require(tibble))


xtext <- function(node, xpath, ns) {
  x <- xml_find_first(node, xpath, ns)
  if (length(x) == 0) NA_character_ else xml_text(x)
}

