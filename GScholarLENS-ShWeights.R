# GScholarLENS-ShWeights.R -- base R only (sourced by server.R and, guarded, by ui.R; concatenated in the WASM bundle).
# =============================================================================
# C1-4: Sh-index author-position weights
# Sh-index = h-index of Adjusted_Citations, where Adjusted_Citations = Citations x position weight of the target
# author on that paper. Defaults are the GScholarLENS weights (unchanged from extend_input_table() before C1-4).
# =============================================================================
SH_DEFAULT_WEIGHTS <- c(First_Author = 0.90, Second_Author = 0.50, Corresponding_Author = 1.00,
                        Co_Author_small = 0.25, Co_Author_large = 0.10, Generic_Match = 1.00)
SH_COAUTHOR_SMALL_MAX <- 6L     # co-author on a paper with <= 6 authors uses Co_Author_small, otherwise Co_Author_large
SH_WEIGHT_LABELS <- c(First_Author = "First author", Second_Author = "Second author",
                      Corresponding_Author = "Corresponding (last) author",
                      Co_Author_small = "Middle co-author, \u2264 6 authors", Co_Author_large = "Middle co-author, > 6 authors",
                      Generic_Match = "Keyword match (position unknown)")
SH_WEIGHT_MIN <- 0; SH_WEIGHT_MAX <- 2

# Validate / complete a weight vector: missing names take the default; NA, non-numeric, negative or > max are
# rejected (replaced by the default) and reported.
sh_validate_weights <- function(w = NULL) {
  out <- SH_DEFAULT_WEIGHTS; errors <- character()
  if (is.null(w)) return(list(weights = out, errors = errors))
  w <- unlist(w)
  for (k in intersect(names(w), names(out))) {
    v <- suppressWarnings(as.numeric(w[[k]]))
    if (length(v) != 1L || is.na(v) || !is.finite(v) || v < SH_WEIGHT_MIN || v > SH_WEIGHT_MAX)
      errors <- c(errors, sprintf("%s must be a number between %g and %g", k, SH_WEIGHT_MIN, SH_WEIGHT_MAX))
    else out[[k]] <- v
  }
  unknown <- setdiff(names(w), names(out))
  if (length(unknown)) errors <- c(errors, paste0("unknown weight(s): ", paste(unknown, collapse = ", ")))
  list(weights = out, errors = errors)
}

# Per-row weight. newman = TRUE: every matched position gets 1/(n-1), n = Author_Count; single-author papers
# (n = 1, where 1/(n-1) is undefined) get weight 1. Rows whose label is Not_found always get 0.
sh_position_weights <- function(label, author_count, weights = SH_DEFAULT_WEIGHTS, newman = FALSE) {
  w <- sh_validate_weights(weights)$weights
  n <- suppressWarnings(as.integer(author_count)); n[is.na(n) | n < 1L] <- 1L
  out <- rep(0.0, length(label)); found <- !is.na(label) & label != "Not_found"
  if (isTRUE(newman)) {
    out[found] <- ifelse(n[found] <= 1L, 1, 1 / (n[found] - 1))
    return(out)
  }
  for (k in c("Generic_Match", "Corresponding_Author", "First_Author", "Second_Author"))
    out[!is.na(label) & label == k] <- w[[k]]
  co <- !is.na(label) & label == "Co_Author"
  out[co & n <= SH_COAUTHOR_SMALL_MAX] <- w[["Co_Author_small"]]
  out[co & n > SH_COAUTHOR_SMALL_MAX] <- w[["Co_Author_large"]]
  out
}

# (Re)compute Adjustment_Weights and Adjusted_Citations of an extended table (needs label + Author_Count).
sh_apply_weights <- function(df, weights = SH_DEFAULT_WEIGHTS, newman = FALSE) {
  if (!is.data.frame(df) || !nrow(df) || !"label" %in% names(df)) return(df)
  ac <- if ("Author_Count" %in% names(df)) df$Author_Count else rep(1L, nrow(df))
  wts <- sh_position_weights(df$label, ac, weights, newman)
  df$Adjustment_Weights <- wts
  cites <- suppressWarnings(as.numeric(df$Citations)); cites[is.na(cites)] <- 0.0
  df$Adjusted_Citations <- cites * wts
  df
}

# Which weights differ from the defaults (character, empty at defaults); Newman counts as a difference.
sh_weights_diff <- function(weights = SH_DEFAULT_WEIGHTS, newman = FALSE) {
  w <- sh_validate_weights(weights)$weights
  d <- names(w)[abs(w - SH_DEFAULT_WEIGHTS[names(w)]) > 1e-12]
  c(if (isTRUE(newman)) "Newman 1/(n\u22121) for all positions",
    if (!isTRUE(newman) && length(d)) sprintf("%s %g (default %g)", SH_WEIGHT_LABELS[d], w[d], SH_DEFAULT_WEIGHTS[d]))
}

sh_weights_table <- function(weights = SH_DEFAULT_WEIGHTS, newman = FALSE) {
  w <- sh_validate_weights(weights)$weights
  data.frame(position = unname(SH_WEIGHT_LABELS[names(w)]), key = names(w),
             weight = if (isTRUE(newman)) NA_real_ else unname(w), default = unname(SH_DEFAULT_WEIGHTS[names(w)]),
             stringsAsFactors = FALSE)
}

# One-sentence description (UI help, methods text, MCP).
sh_weights_description <- function(weights = SH_DEFAULT_WEIGHTS, newman = FALSE) {
  w <- sh_validate_weights(weights)$weights
  base <- "The Sh-index is the h-index of position-weighted citations (citations of each paper multiplied by the weight of the author's position on it)"
  if (isTRUE(newman))
    return(paste0(base, "; here every author position was weighted 1/(n\u22121), n = number of authors on the paper (weight 1 for single-author papers), a pairwise-tie weight rather than fractional credit 1/n."))
  paste0(base, sprintf("; weights: first %g, second %g, corresponding/last %g, middle co-author %g (\u2264 %d authors) or %g (> %d authors), keyword match without position %g%s.",
         w[["First_Author"]], w[["Second_Author"]], w[["Corresponding_Author"]], w[["Co_Author_small"]], SH_COAUTHOR_SMALL_MAX,
         w[["Co_Author_large"]], SH_COAUTHOR_SMALL_MAX, w[["Generic_Match"]],
         if (length(sh_weights_diff(w, FALSE))) " (custom, not comparable to standard Sh-index values)" else " (GScholarLENS defaults)"))
}


# Banner shown whenever the active weights differ from the defaults (or the Newman toggle is on). NULL at defaults.
# Text is passed as text nodes (htmltools escapes), never as HTML.
sh_banner_ui <- function(weights = SH_DEFAULT_WEIGHTS, newman = FALSE, errors = character()) {
  d <- sh_weights_diff(weights, newman)
  if (!length(d) && !length(errors)) return(NULL)
  shiny::tags$div(class = "alert alert-warning cn-sh-banner", role = "alert", `aria-live` = "polite",
    shiny::tags$strong("Non-standard Sh-index weights. "),
    if (length(d)) paste0("Active changes: ", paste(d, collapse = "; "),
      ". Sh-index values and everything derived from them are not comparable to standard Sh-index values."),
    if (length(errors)) shiny::tags$div("Ignored invalid input (last valid value kept): ", paste(errors, collapse = "; ")))
}

# D1-4: keep-last-valid resolution of raw (possibly mid-typing) inputs. Each valid value (0..2) is taken; an
# invalid one (NA / empty while typing, out of range) keeps the LAST VALID value (not the default) and is reported.
sh_resolve_weights <- function(raw = NULL, last_valid = SH_DEFAULT_WEIGHTS) {
  lv <- sh_validate_weights(last_valid)$weights; out <- lv; errors <- character()
  if (is.null(raw)) return(list(weights = out, errors = errors))
  raw <- as.list(raw)
  for (k in intersect(names(raw), names(out))) {
    v <- suppressWarnings(as.numeric(unlist(raw[[k]])))
    if (length(v) != 1L || is.na(v) || !is.finite(v) || v < SH_WEIGHT_MIN || v > SH_WEIGHT_MAX)
      errors <- c(errors, sprintf("%s must be a number between %g and %g", k, SH_WEIGHT_MIN, SH_WEIGHT_MAX))
    else out[[k]] <- v
  }
  list(weights = out, errors = errors)
}

# D1-4: the canonical weights state read by every consumer: list(mode in {default, custom, newman}, weights,
# newman, errors). mode is derived from the weights actually used, so the banner can never diverge from the data.
sh_weights_state <- function(weights = SH_DEFAULT_WEIGHTS, newman = FALSE, errors = character()) {
  w <- sh_validate_weights(weights)$weights
  mode <- if (isTRUE(newman)) "newman" else if (length(sh_weights_diff(w, FALSE))) "custom" else "default"
  list(mode = mode, weights = w, newman = identical(mode, "newman"), errors = as.character(errors))
}

# D1-4: position label from the 0/1 position flags when an extended table has no `label` column (e.g. a table
# imported with position flags already present): without it sh_apply_weights() silently did nothing while the
# banner announced non-default weights.
sh_label_from_flags <- function(df) {
  lab <- rep("Not_found", nrow(df))
  flag <- function(k) if (k %in% names(df)) { v <- suppressWarnings(as.integer(df[[k]])); !is.na(v) & v >= 1L } else rep(FALSE, nrow(df))
  for (k in c("Corresponding_Author", "Co_Author", "Second_Author", "First_Author")) lab[flag(k)] <- k
  lab
}

# D1-4: the weighted table for a canonical state. Default mode returns the input untouched, so switching back to
# default reproduces the original values exactly; custom / Newman always recompute from the base table (never
# from a previously weighted one), so any switch order gives the same result as going there directly.
sh_weighted_table <- function(df, sw = sh_weights_state()) {
  if (!is.data.frame(df) || !nrow(df) || identical(sw$mode, "default")) return(df)
  if (!"label" %in% names(df)) {
    if (!any(c("First_Author", "Second_Author", "Co_Author", "Corresponding_Author") %in% names(df))) return(df)
    df$label <- sh_label_from_flags(df)
    out <- sh_apply_weights(df, sw$weights, sw$newman); out$label <- NULL
    return(out)
  }
  sh_apply_weights(df, sw$weights, sw$newman)
}

# Server wiring of the Publication Metrics weight editor (inputs sh_w_<key>, sh_newman, sh_weights_reset,
# sh_weights_toggle). Returns the canonical reactive sh_weights() = list(mode, weights, newman, errors).
# D1-4: only the numeric inputs are debounced; the Newman toggle and Reset take effect immediately; an invalid
# value keeps the last valid weight (inline feedback next to the inputs + banner), it never crashes nor snaps
# to the default.
register_sh_weights <- function(input, output, session, debounce_ms = 400) {
  last_valid <- shiny::reactiveVal(SH_DEFAULT_WEIGHTS)
  errors_rv <- shiny::reactiveVal(character())
  raw_rx <- shiny::reactive({
    raw <- lapply(names(SH_DEFAULT_WEIGHTS), function(k) input[[paste0("sh_w_", k)]] %||% SH_DEFAULT_WEIGHTS[[k]])
    names(raw) <- names(SH_DEFAULT_WEIGHTS); raw
  })
  raw_d <- shiny::debounce(raw_rx, debounce_ms)
  shiny::observeEvent(raw_d(), {
    r <- sh_resolve_weights(raw_d(), shiny::isolate(last_valid()))
    if (!identical(r$weights, shiny::isolate(last_valid()))) last_valid(r$weights)
    if (!identical(r$errors, shiny::isolate(errors_rv()))) errors_rv(r$errors)
  }, ignoreNULL = FALSE)
  register_sh_weights_toggle(input, session)   # D1-5
  sh_weights <- shiny::reactive(sh_weights_state(last_valid(), isTRUE(input$sh_newman), errors_rv()))
  output$sh_weights_table <- shiny::renderTable({
    sw <- sh_weights(); t <- sh_weights_table(sw$weights, sw$newman)
    data.frame(`Author position` = t$position, Weight = if (sw$newman) "1/(n−1)" else format(t$weight),
               Default = format(t$default), check.names = FALSE)
  }, striped = TRUE, spacing = "xs")
  output$sh_weights_help <- shiny::renderText({ sw <- sh_weights(); sh_weights_description(sw$weights, sw$newman) })
  output$sh_weights_banner <- shiny::renderUI({ sw <- sh_weights(); sh_banner_ui(sw$weights, sw$newman, sw$errors) })
  output$sh_weights_feedback <- shiny::renderUI({
    e <- sh_weights()$errors
    if (length(e)) shiny::tags$div(class = "text-danger small", role = "status", paste(e, collapse = "; "), " (last valid value kept)")
  })
  shiny::observeEvent(input$sh_newman, {
    if (requireNamespace("shinyjs", quietly = TRUE)) for (k in names(SH_DEFAULT_WEIGHTS)) shinyjs::toggleState(paste0("sh_w_", k), condition = !isTRUE(input$sh_newman))
  }, ignoreNULL = FALSE)
  shiny::observeEvent(input$sh_weights_reset, {
    last_valid(SH_DEFAULT_WEIGHTS); errors_rv(character())
    for (k in names(SH_DEFAULT_WEIGHTS)) shiny::updateNumericInput(session, paste0("sh_w_", k), value = SH_DEFAULT_WEIGHTS[[k]])
    shiny::updateCheckboxInput(session, "sh_newman", value = FALSE)
  })
  sh_weights
}

# D1-5: show/hide toggle of the extended weight parameters. Same mechanism and styling as the sidebar
# "Show/Hide Lookup Controls" toggle (actionButton toggle_extended + server-side shinyjs show/hide +
# updateActionButton label/icon + btn-warning/btn-secondary swap), plus aria-expanded kept in sync.
SH_TOGGLE_ID <- "sh_weights_toggle"; SH_PANEL_ID <- "sh_weights_container"
sh_toggle_state <- function(open) if (isTRUE(open))
  list(label = "Hide extended weight parameters", icon = "lock", add = "btn-warning", remove = "btn-secondary", expanded = "true") else
  list(label = "Show extended weight parameters", icon = "magnifying-glass", add = "btn-secondary", remove = "btn-warning", expanded = "false")

register_sh_weights_toggle <- function(input, session) {
  open <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input[[SH_TOGGLE_ID]], {
    open(!isTRUE(open())); st <- sh_toggle_state(open())
    if (requireNamespace("shinyjs", quietly = TRUE)) {
      if (open()) shinyjs::show(SH_PANEL_ID) else shinyjs::hide(SH_PANEL_ID)
      shinyjs::removeClass(SH_TOGGLE_ID, st$remove); shinyjs::addClass(SH_TOGGLE_ID, st$add)
      shinyjs::runjs(sprintf("(function(){var b=document.getElementById('%s');if(b)b.setAttribute('aria-expanded','%s');})();",
                             SH_TOGGLE_ID, st$expanded))
    }
    shiny::updateActionButton(session, SH_TOGGLE_ID, label = st$label, icon = shiny::icon(st$icon))
  })
  open
}

# Publication Metrics panel UI: banner (always visible, outside the collapsible) + toggle button + collapsible
# panel with weights table, editable inputs, Newman toggle and Reset (C1-4, D1-5).
sh_weights_ui <- function() { st <- sh_toggle_state(FALSE); shiny::tagList(
  shiny::uiOutput("sh_weights_banner"),
  shiny::actionButton(SH_TOGGLE_ID, st$label, icon = shiny::icon(st$icon), class = st$add,
    style = "margin-top: 10px; width: 100%; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;",
    "data-toggle" = "tooltip", title = "Show or hide the Sh-index author-position weights (custom weights, Newman 1/(n−1), reset)",
    `aria-expanded` = st$expanded, `aria-controls` = SH_PANEL_ID),
  shiny::tags$div(id = SH_PANEL_ID, class = "cn-sh-weights", style = "display: none;", role = "region",
    `aria-label` = "Sh-index author-position weights",
    shiny::tags$h5("Sh-index author-position weights", style = "font-weight: bold; margin-top: 12px;"),
    shiny::tags$p(class = "small", shiny::textOutput("sh_weights_help", inline = TRUE)),
    shiny::fluidRow(
      shiny::column(6, shiny::tags$div(class = "minimal-table", shiny::tableOutput("sh_weights_table"))),
      shiny::column(6,
        lapply(names(SH_DEFAULT_WEIGHTS), function(k)
          shiny::numericInput(paste0("sh_w_", k), SH_WEIGHT_LABELS[[k]], value = SH_DEFAULT_WEIGHTS[[k]],
                       min = SH_WEIGHT_MIN, max = SH_WEIGHT_MAX, step = 0.05, width = "100%")),
        shiny::uiOutput("sh_weights_feedback"),
        shiny::tags$div(title = "Every author position on a paper gets 1/(n−1), n = number of authors. Single-author papers (n = 1, division by zero) get weight 1. Newman's 1/(n−1) is a pairwise-tie weight; fractional per-author credit would be 1/n.",
          shiny::checkboxInput("sh_newman", "Newman weights for all positions: 1/(n−1)", value = FALSE)),
        shiny::actionButton("sh_weights_reset", "Reset to defaults", class = "btn-sm")
      )
    )
  )
)}

# D1-4 follow-up: the weights actually in use, for non-reactive readers (.cnet settings, MCP metrics tool).
# Prefers the canonical sh_weights() of the current session (registered in session$userData by server.R) so
# saved/reported weights match the screen while a numeric field holds an invalid value; otherwise validates
# the raw inputs (invalid -> default) so NA is never written to a .cnet.
sh_current_weights <- function(raw_get, session = shiny::getDefaultReactiveDomain()) {
  f <- if (!is.null(session)) tryCatch(session$userData$cn_sh_weights, error = function(e) NULL)
  if (is.function(f)) {
    st <- tryCatch(shiny::isolate(f()), error = function(e) NULL)
    if (is.list(st) && !is.null(st$weights)) return(list(weights = unlist(st$weights), newman = isTRUE(st$newman)))
  }
  w <- sh_validate_weights(lapply(stats::setNames(names(SH_DEFAULT_WEIGHTS), names(SH_DEFAULT_WEIGHTS)),
                                  function(k) raw_get(paste0("sh_w_", k)) %||% SH_DEFAULT_WEIGHTS[[k]]))$weights
  list(weights = w, newman = isTRUE(raw_get("sh_newman")))
}
