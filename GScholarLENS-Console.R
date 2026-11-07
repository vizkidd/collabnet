# =============================================================================
# GScholarLENS-Console.R -- CollabNET debug mode + floating R console
# =============================================================================
#
# Interface: PROTOCOL.md section 5.
#   console_eval_capture(code, envir, timeout = 10, max_chars = 20000)  (pure)
#   register_console(input, output, session, envir)  -> list(state, ai_enabled, eval_ai)
#   console_ui()       -> tagList (css + js + floating console container)
#   debug_toggle_ui()  -> the "Debug" switch for the header button bar
#
# Inputs : input$debug_mode (default FALSE, not persisted),
#          input$console_ai_access (forced FALSE whenever debug is off),
#          input$console_cmd = {id, code}
# Output : session$sendCustomMessage("collabnet_console",
#            {id, origin: "user"|"ai", code, entries, truncated, elapsed_ms})
#          and {type: "state", debug, ai_access}
#
# SECURITY / RUNTIME LIMITS (read before enabling "Allow AI access"):
#   * The app normally runs in webR (R compiled to WebAssembly, in the browser).
#     Code typed into the console (or sent by an AI via collabnet_r_eval) runs
#     in that webR sandbox. It CAN read and modify all app state reachable from
#     the console environment (a child of the server() function environment:
#     reactive values, loaded datasets, API keys held in R variables, ...), and
#     it CAN use the browser's network (fetch / url() / download.file through
#     webR's XHR layer, subject to CORS). It CANNOT reach the user's real
#     filesystem: webR only sees its in-memory virtual filesystem (Emscripten
#     MEMFS) plus anything the app explicitly mounted or uploaded.
#   * The timeout uses setTimeLimit(elapsed = timeout, transient = TRUE). R only
#     checks time limits at R-level interrupt checkpoints (R function calls,
#     loops in the interpreter, Sys.sleep, ...). A tight compiled loop (C/C++/
#     Fortran code that never calls R_CheckUserInterrupt) is NOT interrupted.
#     In webR there is additionally no way to interrupt such a loop from the
#     page at all (no SIGINT; the webR worker would have to be terminated and
#     the app reloaded). In webR the evaluation also blocks the single R
#     thread, so the UI is unresponsive while it runs.
#   * Output is truncated to max_chars characters in total (truncated = TRUE).
#   * All output is rendered in the browser with textContent only (no HTML).
#   * Debug mode is OFF by default and not persisted; turning it off hides the
#     console and forces "Allow AI access" OFF. The R side is authoritative:
#     eval_ai() re-checks ai_enabled() at call time and rejects otherwise.
# =============================================================================

# ---- quit guard (D1-1) --------------------------------------------------------
# quit()/q() in webR terminates the Emscripten runtime (ExitStatus exit(0)) and kills the whole app.
# Three layers, shared by the console and the MCP collabnet_r_eval tool (both go through
# console_eval_capture):
#   1. cn_quit_guard_check(): static AST scan of the parsed code (rejects before anything runs).
#   2. cn_quit_mask_env(): the console environment's parent masks quit/q with functions that signal
#      the same condition (catches indirect calls the scan cannot see).
#   3. cn_install_quit_override(): webR only (R.version$os == "emscripten"), replaces base::quit/q in
#      baseenv() and the base namespace so they throw unless options(collabnet.allow_quit = TRUE).
#      Not installed on desktop R, where quit() is legitimate.
# Static checks can be bypassed (e.g. code built at run time); layers 2-3 are the backstop and the
# app frame/shell show an "R session has ended" overlay if R exits anyway (collabnet-session-guard.js).

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

# ---- pure evaluator ---------------------------------------------------------

console_eval_capture <- function(code, envir = parent.frame(), timeout = 10,
                                 max_chars = 20000) {
  t0 <- proc.time()[["elapsed"]]
  entries <- list()
  add <- function(type, text) {
    text <- paste(as.character(text), collapse = "\n")
    if (!nzchar(text)) return(invisible())
    n <- length(entries)
    # merge consecutive chunks of the same stream type
    if (n > 0L && type %in% c("output", "value") && identical(entries[[n]]$type, type)) {
      entries[[n]]$text <<- paste(entries[[n]]$text, text, sep = "\n")
    } else {
      entries[[n + 1L]] <<- list(type = type, text = text)
    }
    invisible()
  }
  finish <- function() {
    total <- 0
    truncated <- FALSE
    kept <- list()
    for (e in entries) {
      len <- nchar(e$text, type = "chars")
      if (total + len > max_chars) {
        room <- max(0, max_chars - total)
        if (room > 0) {
          e$text <- substr(e$text, 1L, room)
          kept[[length(kept) + 1L]] <- e
        }
        truncated <- TRUE
        break
      }
      total <- total + len
      kept[[length(kept) + 1L]] <- e
    }
    if (truncated) {
      kept[[length(kept) + 1L]] <- list(
        type = "message",
        text = sprintf("[output truncated at %d characters]", as.integer(max_chars)))
    }
    list(entries = kept, truncated = truncated,
         elapsed_ms = round((proc.time()[["elapsed"]] - t0) * 1000))
  }

  code <- paste(as.character(code), collapse = "\n")
  exprs <- tryCatch(parse(text = code, keep.source = FALSE),
                    error = function(e) e)
  if (inherits(exprs, "error")) {
    add("error", paste0("Error: syntax error: ", conditionMessage(exprs)))
    return(finish())
  }
  if (length(exprs) == 0L) return(finish())
  blocked <- cn_quit_guard_check(exprs)
  if (!is.null(blocked)) {
    add("error", paste0("Error: ", CN_QUIT_MSG, " [blocked: ", blocked, "]"))
    return(finish())
  }

  # Output capture via a text connection sink; flushed whenever a condition is
  # signalled so messages/warnings interleave with printed output in order.
  out_lines <- character()
  tc <- textConnection("out_lines", "w", local = TRUE)
  sink_n <- sink.number()
  sink(tc)
  consumed <- 0L
  flush_out <- function(type = "output") {
    # complete a pending partial line (e.g. cat("x") without newline) so it is
    # not glued onto the next chunk
    if (isTRUE(tryCatch(isIncomplete(tc), error = function(e) FALSE))) cat("\n")
    cur <- textConnectionValue(tc)
    if (length(cur) > consumed) {
      add(type, cur[(consumed + 1L):length(cur)])
      consumed <<- length(cur)
    }
  }
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    while (sink.number() > sink_n) sink()
    try(close(tc), silent = TRUE)
  }, add = TRUE)

  fmt_cond <- function(c, prefix) {
    call <- conditionCall(c)
    # top-level conditions: hide our own eval()/print() wrapper call
    if (is.call(call) && (identical(call, quote(eval(ex, envir))) ||
                          identical(call, quote(print(res$value))))) call <- NULL
    msg <- sub("\n$", "", conditionMessage(c))
    if (!is.null(call) && !identical(prefix, "")) {
      dc <- paste(deparse(call, nlines = 1L), collapse = "")
      paste0(prefix, " in ", dc, ": ", msg)
    } else if (!identical(prefix, "")) {
      paste0(prefix, ": ", msg)
    } else msg
  }
  run_with_handlers <- function(expr_fun) {
    withCallingHandlers(
      expr_fun(),
      message = function(m) {
        flush_out()
        add("message", sub("\n$", "", conditionMessage(m)))
        invokeRestart("muffleMessage")
      },
      warning = function(w) {
        flush_out()
        add("warning", fmt_cond(w, "Warning"))
        invokeRestart("muffleWarning")
      }
    )
  }

  setTimeLimit(elapsed = timeout, transient = TRUE)
  for (ex in exprs) {
    ok <- tryCatch({
      res <- run_with_handlers(function() withVisible(eval(ex, envir)))
      flush_out()
      if (isTRUE(res$visible)) {
        run_with_handlers(function() print(res$value))
        flush_out("value")
      }
      TRUE
    }, error = function(e) {
      flush_out()
      msg <- conditionMessage(e)
      if (inherits(e, "collabnet_quit_blocked")) {
        add("error", paste0("Error: ", msg))
      } else if (grepl("elapsed time limit", msg, fixed = TRUE)) {
        add("error", sprintf("Error: evaluation timed out after %gs (%s)", timeout, msg))
      } else {
        add("error", fmt_cond(e, "Error"))
      }
      FALSE
    })
    if (!ok) break
  }
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  while (sink.number() > sink_n) sink()
  finish()
}

# ---- server-side registration -----------------------------------------------

register_console <- function(input, output, session,
                             envir = new.env(parent = parent.frame()),
                             timeout = 10, max_chars = 20000, .send = NULL) {
  # .send: optional function(type, message) replacing session$sendCustomMessage (tests)
  # D1-1 layer 2: quit/q are masked by the parent of the evaluation environment.
  if (!exists(".cn_quit_mask", envir = envir, inherits = TRUE)) parent.env(envir) <- cn_quit_mask_env(parent.env(envir))
  state <- shiny::reactiveValues(debug = FALSE, ai_access = FALSE)
  ai_counter <- 0L

  send <- function(msg) {
    if (is.function(.send)) .send("collabnet_console", msg)
    else session$sendCustomMessage("collabnet_console", msg)
  }
  push_state <- function() {
    send(list(type = "state",
              debug = isTRUE(shiny::isolate(state$debug)),
              ai_access = isTRUE(shiny::isolate(state$ai_access))))
  }
  # Callable both inside and outside a reactive context.
  ai_enabled <- function() {
    rd <- function() isTRUE(state$debug) && isTRUE(state$ai_access)
    tryCatch(rd(), error = function(e) shiny::isolate(rd()))
  }

  shiny::observeEvent(input$debug_mode, {
    d <- isTRUE(input$debug_mode)
    state$debug <- d
    if (!d) {
      state$ai_access <- FALSE
      shiny::updateCheckboxInput(session, "console_ai_access", value = FALSE)
    }
    push_state()
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  shiny::observeEvent(input$console_ai_access, {
    a <- isTRUE(input$console_ai_access)
    if (a && !isTRUE(state$debug)) {
      a <- FALSE
      shiny::updateCheckboxInput(session, "console_ai_access", value = FALSE)
    }
    state$ai_access <- a
    push_state()
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  shiny::observeEvent(input$console_cmd, {
    cmd <- input$console_cmd
    id <- if (!is.null(cmd$id)) as.character(cmd$id) else ""
    code <- if (!is.null(cmd$code)) paste(as.character(cmd$code), collapse = "\n") else ""
    if (!isTRUE(state$debug)) {
      res <- list(entries = list(list(type = "error",
                    text = "Error: debug mode is off; the R console is disabled.")),
                  truncated = FALSE, elapsed_ms = 0)
    } else {
      res <- console_eval_capture(code, envir, timeout = timeout, max_chars = max_chars)
    }
    send(list(id = id, origin = "user", code = code, entries = res$entries,
              truncated = res$truncated, elapsed_ms = res$elapsed_ms))
  })

  eval_ai <- function(code) {
    code <- paste(as.character(code), collapse = "\n")
    ai_counter <<- ai_counter + 1L
    id <- paste0("ai-", ai_counter)
    if (!ai_enabled()) {
      err <- "AI access to the R console is disabled (requires debug mode ON and 'Allow AI access' ON)."
      res <- list(entries = list(list(type = "error", text = paste0("Error: ", err))),
                  truncated = FALSE, elapsed_ms = 0, ok = FALSE, error = err)
    } else {
      res <- console_eval_capture(code, envir, timeout = timeout, max_chars = max_chars)
      res$ok <- TRUE
    }
    send(list(id = id, origin = "ai", code = code, entries = res$entries,
              truncated = res$truncated, elapsed_ms = res$elapsed_ms))
    res
  }

  list(state = state, ai_enabled = ai_enabled, eval_ai = eval_ai, envir = envir)
}

# ---- UI ---------------------------------------------------------------------

# Bootstrap-3/Shiny checkbox markup built by hand so we can add
# autocomplete="off" (prevents browsers restoring the state on reload).
# Shiny's checkbox binding and updateCheckboxInput() work with it unchanged.
.console_switch <- function(inputId, label, class = NULL, title = NULL) {
  shiny::tags$div(
    class = paste(c("form-group shiny-input-container collabnet-switch", class), collapse = " "),
    title = title,
    shiny::tags$div(
      class = "checkbox",
      shiny::tags$label(
        shiny::tags$input(id = inputId, type = "checkbox", autocomplete = "off"),
        shiny::tags$span(label)
      )
    )
  )
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

debug_toggle_ui <- function() {
  shiny::tags$div(
    class = "btn btn-outline-white collabnet-debug-toggle",
    `data-toggle` = "tooltip",
    title = "Debug mode: show the floating R console (off by default, not saved)",
    `data-placement` = "bottom",
    shiny::tags$i(class = "fa fa-bug", `aria-hidden` = "true"),          # D3-2: icon-only; text kept for screen readers
    .console_switch("debug_mode", shiny::tags$span(class = "sr-only", "Debug mode"))
  )
}

console_ui <- function() {
  btn <- function(id, label, title, icon_cls) {
    shiny::tags$button(id = id, type = "button", class = "btn btn-xs cn-console-btn",
                       title = title, `aria-label` = title,
                       shiny::tags$i(class = icon_cls), shiny::tags$span(class = "cn-btn-label", label))
  }
  shiny::tagList(
    shiny::tags$link(rel = "stylesheet", href = "css/collabnet-console.css"),
    shiny::tags$script(src = "js/collabnet-console.js"),
    shiny::tags$script(src = "js/collabnet-session-guard.js"),
    shiny::tags$div(
      id = "collabnet-console",
      class = "collabnet-console is-hidden is-minimized",
      `data-state` = "hidden",
      role = "region", `aria-label` = "R debug console",
      shiny::tags$div(
        class = "cn-console-header",
        id = "collabnet-console-header",
        shiny::tags$span(class = "cn-console-title", "R console"),
        shiny::tags$span(id = "collabnet-console-status", class = "cn-console-status", "debug off"),
        shiny::tags$div(class = "cn-console-spacer"),
        .console_switch("console_ai_access", "Allow AI access", class = "cn-ai-switch",
                        title = "Let the in-app AI / MCP clients run R code here (collabnet_r_eval)"),
        btn("collabnet-console-restart", "Restart R session",
            "Restart R session: reloads the app (offers to save a .cnet first); quit() is disabled here",
            "fa fa-refresh"),
        btn("collabnet-console-clear", "Clear", "Clear output", "fa fa-eraser"),
        btn("collabnet-console-min", "Minimize", "Minimize", "fa fa-window-minimize"),
        btn("collabnet-console-restore", "Restore", "Restore (normal size)", "fa fa-window-restore"),
        btn("collabnet-console-max", "Maximize", "Maximize", "fa fa-window-maximize")
      ),
      shiny::tags$div(
        class = "cn-console-body",
        shiny::tags$div(id = "collabnet-console-output", class = "cn-console-output",
                        role = "log", `aria-live` = "polite"),
        shiny::tags$div(
          class = "cn-console-inputrow",
          shiny::tags$span(class = "cn-console-prompt", ">"),
          # no id on purpose: Shiny would bind every textarea with an id as an input
          shiny::tags$textarea(`data-cn` = "input", class = "cn-console-input",
                               rows = "2", spellcheck = "false", autocomplete = "off",
                               placeholder = "R code. Enter = run, Shift+Enter = newline, Up/Down = history"),
          shiny::tags$button(id = "collabnet-console-run", type = "button",
                             class = "btn btn-primary btn-xs cn-console-run", "Run")
        )
      )
    )
  )
}
