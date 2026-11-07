# C3-2 (audit P3-6): Content-Security-Policy for the app page itself (ui.R emits it as a <meta>).
# The Shinylive outer shell gets its own hash-based policy from csp_inject.py; this one covers the
# app document (native runApp, and the Shinylive app iframe). Base R only.
#
# Relaxations, each measured in the native full-app browser smoke (tests/browser/smoke_full.py):
#  - script-src 'unsafe-inline': ui.R/server.R emit inline <script> blocks and on*= handlers
#    (onchange/onclick) built at runtime; hashes cannot cover runtime-built handlers.
#  - script-src 'unsafe-eval': only if CN_CSP_ALLOW_EVAL (see FIXES_C3.md for the measurement).
#  - style-src 'unsafe-inline': Shiny/bslib/visNetwork/plotly set inline styles.
# A <meta> CSP cannot be report-only and does not apply to elements before it, so it is emitted first.
CN_CSP_API_HOSTS <- c("https://api.elsevier.com", "https://api.crossref.org", "https://api.openalex.org",
                      "https://api.semanticscholar.org", "https://api.opencitations.net", "https://pub.orcid.org",
                      "https://doi.org", "https://www.ncbi.nlm.nih.gov", "https://repo.r-wasm.org",
                      "https://api.openai.com", "https://api.anthropic.com",   # D2-B3 in-page chat providers
                      "https://generativelanguage.googleapis.com", "https://openrouter.ai")
CN_CSP_LOCAL_BRIDGE <- c("ws://127.0.0.1:*", "ws://localhost:*", "http://127.0.0.1:*", "http://localhost:*")
CN_CSP_ALLOW_EVAL <- TRUE   # measured C3-2: without it the page throws "Evaluating a string as JavaScript violates ... unsafe-eval" (htmlwidgets/Shiny JS() evaluation) and the app breaks

cn_app_csp_policy <- function(allow_eval = CN_CSP_ALLOW_EVAL) {
  script <- c("'self'", "'unsafe-inline'", if (isTRUE(allow_eval)) "'unsafe-eval'", "'wasm-unsafe-eval'", "blob:")
  paste(c(
    "default-src 'self'",
    paste("script-src", paste(script, collapse = " ")),
    "worker-src 'self' blob:",
    "child-src 'self' blob:",
    "frame-src 'self' blob:",
    paste("connect-src", paste(c("'self'", "ws:", "wss:", "blob:", "data:", CN_CSP_API_HOSTS, CN_CSP_LOCAL_BRIDGE), collapse = " ")),
    "img-src 'self' data: blob: https:",
    "style-src 'self' 'unsafe-inline' https://fonts.googleapis.com",
    "font-src 'self' data: https://fonts.gstatic.com",
    "object-src 'none'",
    "base-uri 'self'",
    "form-action 'self'"
  ), collapse = "; ")
}

# option(collabnet.csp = "off") disables the meta tag (escape hatch for debugging).
cn_csp_meta <- function() {
  if (identical(getOption("collabnet.csp"), "off")) return(NULL)
  shiny::tags$meta(`http-equiv` = "Content-Security-Policy", content = cn_app_csp_policy())
}
