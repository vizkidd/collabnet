# =============================================================================
# GScholarLENS-Runtime.R -- runtime guards shared by EVERY edition (P10-A8: moved out of GScholarLENS-Console.R,
# which is now a console-only file shipped in the full edition only).
#   - D1-1 layer 3: quit()/q() guard in webR (cn_install_quit_override, cn_quit_guard_check, cn_quit_mask_env)
#   - D2-A3: session heartbeat reply for www/js/collabnet-session-guard.js (register_session_heartbeat)
# =============================================================================

CN_QUIT_MSG <- "quit() is disabled in the CollabNET console; use 'Restart R session' in the console header."

cn_quit_condition <- function(detail = NULL) {
  structure(class = c("collabnet_quit_blocked", "error", "condition"),
            list(message = if (is.null(detail)) CN_QUIT_MSG else paste0(CN_QUIT_MSG, " (", detail, ")"),
                 call = NULL))
}

.cn_quit_fns <- c("quit", "q")
.cn_lookup_fns <- c("do.call", "match.fun", "get", "get0", "mget", "getExportedValue", "dynGet",
                    "Recall", "lapply", "sapply", "vapply", "mapply", "Map", "Reduce", "Filter",
                    "Position", "Find", "outer", "tryCatch", "withCallingHandlers", "on.exit",
                    "reg.finalizer", "Negate", "Vectorize", "local", "eval", "evalq", "forceAndCall")
.cn_native_fns <- c(".Call", ".External", ".External2", ".C", ".Internal", ".Primitive")
.cn_parse_fns <- c("parse", "str2lang", "str2expression")
.cn_kill_fns <- c("Sys.kill", "pskill")

# Returns NULL if the code is allowed, otherwise a short reason string.
cn_quit_guard_check <- function(exprs, depth = 0L) {
  if (depth > 3L) return(NULL)
  fname <- function(h) {
    if (is.symbol(h)) return(as.character(h))
    if (is.call(h) && length(h) == 3L && as.character(h[[1L]]) %in% c("::", ":::")) return(as.character(h[[3L]]))
    if (is.character(h) && length(h) == 1L) return(h)
    NA_character_
  }
  is_quit_ref <- function(x) (is.symbol(x) || (is.character(x) && length(x) == 1L)) &&
    as.character(x) %in% .cn_quit_fns
  reason <- NULL
  # drop empty arguments (x[, 1], function(a) formals) which are the missing-argument symbol
  nonempty <- function(lst) Filter(function(i) !(is.symbol(lst[[i]]) && !nzchar(as.character(lst[[i]]))), seq_along(lst))
  kids <- function(x) { l <- as.list(x); lapply(nonempty(l), function(i) l[[i]]) }
  walk <- function(x) {
    if (!is.null(reason)) return(invisible())
    if (is.expression(x) || is.pairlist(x) || (is.list(x) && !is.call(x))) { for (el in kids(x)) walk(el); return(invisible()) }
    if (!is.call(x)) return(invisible())
    head <- x[[1L]]
    fn <- fname(head)
    args <- kids(x)[-1L]
    if (is.call(head) && length(head) == 3L && as.character(head[[1L]]) %in% c("::", ":::") &&
        as.character(head[[3L]]) %in% .cn_quit_fns) { reason <<- deparse(head); return(invisible()) }
    if (!is.na(fn)) {
      if (fn %in% .cn_quit_fns && (is.symbol(head) || is.character(head) || is.call(head))) {
        reason <<- paste0(fn, "()"); return(invisible())
      }
      if (fn %in% c("::", ":::") && length(args) == 2L && as.character(args[[2L]]) %in% .cn_quit_fns) {
        reason <<- deparse(x); return(invisible())
      }
      if (fn %in% .cn_lookup_fns && any(vapply(args, is_quit_ref, logical(1)))) {
        reason <<- paste0(fn, "(", "quit/q", ")"); return(invisible())
      }
      if (fn %in% .cn_native_fns && length(args) >= 1L) {
        a1 <- args[[1L]]
        nm <- if (is.symbol(a1) || is.character(a1)) as.character(a1) else if (is.call(a1)) fname(a1[[1L]]) else ""
        if (length(nm) == 1L && !is.na(nm) && grepl("^(quit|q|do_quit|R_CleanUp|Rf_quit|R_Suicide)$", nm)) {
          reason <<- paste0(fn, "(", nm, ")"); return(invisible())
        }
      }
      if (fn %in% .cn_kill_fns && any(grepl("Sys.getpid", vapply(args, function(a) paste(deparse(a), collapse = ""), "")))) {
        reason <<- paste0(fn, "(Sys.getpid())"); return(invisible())
      }
      if (fn %in% .cn_parse_fns) {
        txt <- Filter(function(a) is.character(a), args)
        for (t in txt) {
          sub <- tryCatch(parse(text = t, keep.source = FALSE), error = function(e) NULL)
          r <- if (!is.null(sub)) cn_quit_guard_check(sub, depth + 1L) else NULL
          if (!is.null(r)) { reason <<- r; return(invisible()) }
        }
      }
    }
    for (el in kids(x)) walk(el)
    invisible()
  }
  walk(exprs)
  reason
}

.cn_quit_mask_fun <- function(...) stop(cn_quit_condition())

# Environment that masks quit/q; used as the parent of the console evaluation environment.
cn_quit_mask_env <- function(parent = parent.frame()) {
  e <- new.env(parent = parent)
  assign("quit", .cn_quit_mask_fun, envir = e)
  assign("q", .cn_quit_mask_fun, envir = e)
  assign(".cn_quit_mask", TRUE, envir = e)
  lockEnvironment(e, bindings = TRUE)
  e
}

cn_is_webr <- function() {
  identical(tolower(R.version$os), "emscripten") || exists("webr_eval", mode = "function") ||
    "webr" %in% loadedNamespaces()
}

# Layer 3 (webR only unless force = TRUE, used by tests). Returns a restore function (invisibly).
cn_install_quit_override <- function(force = FALSE) {
  if (!isTRUE(force) && !cn_is_webr()) return(invisible(NULL))
  orig_quit <- base::quit
  guarded <- function(save = "default", status = 0, runLast = TRUE) {
    if (isTRUE(getOption("collabnet.allow_quit", FALSE))) return(orig_quit(save, status, runLast))
    stop(cn_quit_condition())
  }
  envs <- list(baseenv(), .BaseNamespaceEnv)
  olds <- lapply(envs, function(e) list(quit = get("quit", envir = e), q = get("q", envir = e)))
  for (e in envs) for (nm in .cn_quit_fns) {
    ok <- tryCatch({
      if (bindingIsLocked(nm, e)) { unlockBinding(nm, e); on.exit(lockBinding(nm, e), add = TRUE) }
      assign(nm, guarded, envir = e); TRUE
    }, error = function(err) FALSE)
    if (!ok) warning("CollabNET: could not override base::", nm, " (", nm, "() may still end the R session)")
  }
  restore <- function() {
    for (i in seq_along(envs)) for (nm in .cn_quit_fns) {
      e <- envs[[i]]
      locked <- bindingIsLocked(nm, e)
      if (locked) unlockBinding(nm, e)
      assign(nm, olds[[i]][[nm]], envir = e)
      if (locked) lockBinding(nm, e)
    }
    invisible(TRUE)
  }
  invisible(restore)
}

# D2-A3: heartbeat reply for collabnet-session-guard.js. The page sends input$cn_hb_ping (a counter) every ~10 s
# while visible; we echo it. No reply while R is idle => the page shows the "R session stopped responding" overlay.
# A busy R answers late (queued input), which the page shows as "R is busy..." instead.
register_session_heartbeat <- function(input, session) {
  shiny::observeEvent(input$cn_hb_ping, {
    seq <- suppressWarnings(as.numeric(input$cn_hb_ping))
    if (length(seq) == 1 && is.finite(seq)) session$sendCustomMessage("cn_hb_pong", seq)
  })  # ignoreNULL (default) skips the start-up run; every real ping must be answered
  invisible(TRUE)
}
