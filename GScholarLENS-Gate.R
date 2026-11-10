## ---- E1/E3: export gate (E3: Buy Me a Coffee; verified unlock via the Cloudflare Worker, honour button only when no Worker) ----
## Whenever publication data is loaded, every export first opens the gate dialog (www/js/collabnet-gate.js):
## a countdown (default 5 min) or a voluntary "I've paid" click marks the session paid. Paid state lives ONLY in
## this R session (session$userData) and the page's JS variable: never persisted, never restored from .cnet.
## All configuration is here (R options) and mirrored to JS via the "cn_gate_config" message.
##   options(collabnet.export_gate = FALSE)    -> gate off (dev/tests)
##   options(collabnet.gate_countdown = 3)     -> short countdown (tests)

## E3c: ONE Cloudflare Worker verifies donations for every copy. Its origin ("gate API") is resolved as:
##   options(collabnet.gate_api) -> env COLLABNET_GATE_API -> (E3 options(collabnet.gate_verify_url)) -> value baked by
##   buildWASM.R --gate-api (collabnet_gate_api_baked) -> committed default in gate_config.json -> "same-origin".
## Returns "same-origin", "" (no Worker: placeholder/empty -> honour fallback) or "https://host[/prefix]" (no trailing /).
CN_GATE_PLACEHOLDER_RE <- "YOUR-WORKER|REPLACE|example\\.invalid"
cn_gate_api_normalise <- function(v) {
  v <- trimws(as.character(v %||% "")[1]); if (is.na(v)) v <- ""
  if (!nzchar(v) || v == "none" || grepl(CN_GATE_PLACEHOLDER_RE, v, ignore.case = TRUE)) return("")
  if (v %in% c("same-origin", "auto")) return("same-origin")
  if (!grepl("^https?://[^/[:space:]]+", v)) return("")
  sub("/+$", "", sub("/api/gate/?$", "", v))
}
cn_gate_api_config_file <- function(path = "gate_config.json") {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path)$gate_api, error = function(e) NULL)
}
cn_gate_api <- function() {
  o <- getOption("collabnet.gate_api"); if (!is.null(o)) return(cn_gate_api_normalise(o))
  e <- Sys.getenv("COLLABNET_GATE_API", ""); if (nzchar(e)) return(cn_gate_api_normalise(e))
  vu <- getOption("collabnet.gate_verify_url", "auto"); if (!identical(vu, "auto")) return(cn_gate_api_normalise(vu))   # E3 option
  if (exists("collabnet_gate_api_baked", inherits = TRUE)) return(cn_gate_api_normalise(get("collabnet_gate_api_baked", inherits = TRUE)))
  f <- cn_gate_api_config_file(); if (!is.null(f)) return(cn_gate_api_normalise(f))
  "same-origin"
}
## Origin of a cross-origin gate API (for the CSP connect-src), else NULL.
cn_gate_api_origin <- function(api = cn_gate_api()) {
  if (!grepl("^https?://", api)) return(NULL)
  regmatches(api, regexpr("^https?://[^/]+", api))
}
## Server-side check of a Worker-signed token (native R): POST <api>/api/gate/status {token}. The token secret is NOT here;
## only the Worker can say "valid". Returns "valid", "invalid" or "unreachable" (no Worker / network / 5xx / webR).
cn_gate_verify_token <- function(token, api_base, timeout = 5) {
  if (!nzchar(api_base %||% "") || identical(R.version$os, "emscripten") || !requireNamespace("curl", quietly = TRUE)) return("unreachable")
  h <- curl::new_handle(timeout = timeout, connecttimeout = timeout, customrequest = "POST",
                        postfields = as.character(jsonlite::toJSON(list(token = as.character(token)), auto_unbox = TRUE)))
  curl::handle_setheaders(h, "content-type" = "application/json")
  r <- tryCatch(curl::curl_fetch_memory(paste0(api_base, "/api/gate/status"), handle = h), error = function(e) NULL)
  if (is.null(r) || r$status_code >= 500 || r$status_code == 404) return("unreachable")
  j <- tryCatch(jsonlite::fromJSON(rawToChar(r$content)), error = function(e) NULL)
  if (is.list(j) && isTRUE(j$valid)) "valid" else if (is.list(j) && identical(j$valid, FALSE)) "invalid" else "unreachable"
}
## Absolute gate API base as seen from this session ("same-origin" -> the page's origin).
cn_gate_api_base <- function(session, api = cn_gate_api()) {
  if (!identical(api, "same-origin")) return(api)
  cd <- tryCatch(session$clientData, error = function(e) NULL); if (is.null(cd)) return("")
  pr <- shiny::isolate(cd$url_protocol); hn <- shiny::isolate(cd$url_hostname); pt <- shiny::isolate(cd$url_port)
  if (!nzchar(hn %||% "")) return("")
  paste0(pr, "//", hn, if (nzchar(pt %||% "")) paste0(":", pt))
}

cn_gate_config <- function() list(
  enabled = isTRUE(getOption("collabnet.export_gate", TRUE)),
  countdown_seconds = as.integer(getOption("collabnet.gate_countdown", 300L)),
  bmc_url = getOption("collabnet.gate_bmc_url", "https://buymeacoffee.com/vishveshkac"),
  bmc_membership_url = getOption("collabnet.gate_bmc_membership_url", "https://buymeacoffee.com/vishveshkac/membership"),
  # E3: Worker API base for the verified BMC unlock ("auto" = <page origin>/api/gate/); honour_fallback auto|on|off
  verify_url = getOption("collabnet.gate_verify_url", "auto"),
  gate_api = cn_gate_api(),   # E3c: "same-origin" | "" (no Worker) | Worker origin
  honour_fallback = match.arg(getOption("collabnet.gate_honour_fallback", "auto"), c("auto", "on", "off")),
  policy_url = getOption("collabnet.gate_policy_url", "https://vizkidd.github.io/collabnet/donation-policy/"),
  exempt_sources = as.character(getOption("collabnet.gate_exempt_sources", character(0))),
  gate_cnet = isTRUE(getOption("collabnet.gate_cnet", TRUE)))

## Source tags of the loaded table. Rows carry a `Source` column when they came from an API fetch
## ("SCOPUS", "DOI/ORCID", see server.R / GScholarLENS-DOI2Data.R); rows without it came from a file upload.
cn_gate_sources <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(character(0))
  if (!"Source" %in% names(df)) return("upload")
  s <- toupper(trimws(as.character(df$Source)))
  tags <- ifelse(is.na(s) | !nzchar(s), "upload",
          ifelse(s == "SCOPUS", "scopus", ifelse(s == "DOI/ORCID", "orcid", "upload")))
  sort(unique(tags))
}

## TRUE when exports must pass the gate: enabled, data loaded, and not every source exempt.
cn_gate_applies <- function(sources, cfg = cn_gate_config()) {
  isTRUE(cfg$enabled) && length(sources) > 0 && !all(sources %in% cfg$exempt_sources)
}

## Per-session state (environment in session$userData; created lazily).
cn_gate_state <- function(session) {
  ud <- session$userData
  if (is.null(ud$cn_gate)) { e <- new.env(); e$paid <- FALSE; e$grants <- list(); e$sources <- character(0); ud$cn_gate <- e }
  ud$cn_gate
}

## The shared R-side helper. downloadHandlers call it with consume = TRUE (one grant per completed countdown);
## agent/MCP export tools call it the same way. Returns TRUE if the export may proceed.
cn_gate_allow <- function(session, kind, consume = TRUE) {
  if (is.null(session)) return(TRUE)
  st <- cn_gate_state(session)
  if (!cn_gate_applies(st$sources) || isTRUE(st$paid)) return(TRUE)
  if (kind == "cnet" && !isTRUE(cn_gate_config()$gate_cnet)) return(TRUE)
  n <- st$grants[[kind]] %||% 0L
  if (n > 0) { if (consume) st$grants[[kind]] <- n - 1L; return(TRUE) }
  FALSE
}

CN_GATE_AGENT_MSG <- "export gated: user must complete the gate in the UI"

## Agent/MCP tools: if blocked, also open the gate dialog in the UI so the user can complete it.
cn_gate_agent_check <- function(session, kind = "agent_export") {
  if (cn_gate_allow(session, kind)) return(TRUE)
  try(session$sendCustomMessage("cn_gate_open", list(kind = kind)), silent = TRUE)
  FALSE
}

## Server wiring: call once inside server().
register_cn_gate <- function(input, session, data_rx) {
  st <- cn_gate_state(session)
  session$sendCustomMessage("cn_gate_config", cn_gate_config())
  observe({
    st$sources <- cn_gate_sources(tryCatch(data_rx(), error = function(e) NULL))
    session$sendCustomMessage("cn_gate_state", list(sources = I(st$sources), gated = cn_gate_applies(st$sources)))
  })
  # Verified BMC token (or, with no Worker reachable, the honour button) marks this session paid; FALSE re-gates it.
  # E3c: a token is re-checked server-side with the Worker (forged/refunded -> refused). If R cannot reach the Worker it
  # follows the browser (honour path), like the JS side; the honour button itself only appears when no Worker answers.
  observeEvent(input$cn_gate_paid, {
    p <- input$cn_gate_paid
    if (!isTRUE(p$paid)) { st$paid <- FALSE; return() }   # E3: FALSE after a refund revocation
    tok <- p$token
    if (is.character(tok) && length(tok) == 1 && nzchar(tok)) {
      st$verified <- cn_gate_verify_token(tok, cn_gate_api_base(session))
      st$paid <- st$verified != "invalid"
      if (!st$paid) try(session$sendCustomMessage("cn_gate_rejected", list(reason = "token rejected by the donation checker")), silent = TRUE)
    } else st$paid <- TRUE
  })
  # countdown completed for one request: one grant for that export kind, then ack so JS triggers the download.
  observeEvent(input$cn_gate_grant, {
    g <- input$cn_gate_grant; k <- as.character(g$kind %||% "")
    if (nzchar(k)) st$grants[[k]] <- (st$grants[[k]] %||% 0L) + 1L
    session$sendCustomMessage("cn_gate_granted", list(nonce = g$nonce))
  })
  invisible(st)
}

## downloadHandler content wrapper: refuses (writes nothing useful) if the gate was bypassed.
cn_gate_content <- function(session, kind, content) {
  force(content)
  function(file) {
    if (!cn_gate_allow(session, kind)) {
      writeLines("Export gated: complete the export dialog in CollabNET first.", file)
      return(invisible(NULL))
    }
    # E3e backstop: content() must never throw -- a throwing (or req()-stopped) content() makes Shiny answer the
    # download URL with an HTML error page that the browser saves as e.g. "download_btn.html". The real guard is the
    # export-ready pre-check (www/js/collabnet-export-ready.js) that stops empty exports before they start.
    tryCatch(content(file), error = function(e) {
      cn_log_warn("export %s produced no data: %s", kind, conditionMessage(e))
      writeLines(cn_export_empty_message(), file, useBytes = TRUE)
    })
    invisible(NULL)
  }
}
cn_export_empty_message <- function() "No data to export \u2014 load or fetch data first, or loosen the current filters/lookup."
# E3e: which exports have something to export (sent to the browser as "cn_export_ready").
cn_export_ready_state <- function(view_df, full_df, net, frame = "Current View") {
  view <- !is.null(view_df) && is.data.frame(view_df) && nrow(view_df) > 0
  full <- !is.null(full_df) && is.data.frame(full_df) && nrow(full_df) > 0
  has_net <- is.list(net) && !is.null(net$graph) && inherits(net$graph, "igraph") && igraph::vcount(net$graph) > 0
  list(download_table_csv = view, download_table_xlsx = view,
       download_btn = if (identical(frame, "Full Data")) full else view,
       predownload_btn = full,
       dl_net_nodes_csv = has_net, dl_net_edges_csv = has_net, dl_net_graphml = has_net,
       download_methods = full,
       message = cn_export_empty_message())
}
