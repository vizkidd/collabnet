require(xml2)
require(dplyr)
require(purrr)
require(tibble)


xtext <- function(node, xpath, ns) {
  x <- xml_find_first(node, xpath, ns)
  if (length(x) == 0) NA_character_ else xml_text(x)
}

