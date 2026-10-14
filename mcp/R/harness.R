# =============================================================================
# harness.R — loaded into the webR session by the MCP server.
#
# Everything here is prefixed `mcp_` so it cannot collide with CollabNET code.
# =============================================================================

mcp_has <- function(p) isTRUE(requireNamespace(p, quietly = TRUE))

# -----------------------------------------------------------------------------
# Synthetic corpus generator.
#
# The whole point is reproducing the pathological case: a mostly normal corpus
# with a small number of hyperauthored papers. `n_mega` papers get `mega_size`
# authors drawn from a dedicated collaboration pool, which is what an ATLAS or
# CMS record looks like. Author counts otherwise follow a log-normal, which
# matches observed distributions far better than a uniform draw.
# -----------------------------------------------------------------------------
mcp_fixture <- function(n_papers   = 500L,
                        pool       = 2000L,
                        meanlog    = 1.4,
                        sdlog      = 0.7,
                        n_mega     = 0L,
                        mega_size  = 1000L,
                        mega_pool  = 3000L,
                        delim      = ", ",
                        seed       = 1L) {
  set.seed(seed)

  first <- c("Alice","Bruno","Chen","Dara","Emil","Farah","Gita","Hugo","Ines",
             "Jonas","Kira","Liam","Mira","Nils","Omar","Priya","Quinn","Rosa",
             "Sven","Tara","Uma","Viktor","Wen","Xenia","Yusuf","Zara")
  last  <- c("Adeyemi","Bakker","Costa","Dubois","Eriksen","Fontaine","Gupta",
             "Hoffman","Ivanov","Jensen","Kowalski","Lindqvist","Moreau",
             "Nakamura","Okafor","Petrov","Quintero","Rossi","Silva","Tanaka",
             "Ueda","Vargas","Wang","Xu","Yilmaz","Zhang")

  make_pool <- function(n, tag = "") {
    unique(paste0(sample(first, n, TRUE), " ", sample(last, n, TRUE), tag,
                  ifelse(duplicated(seq_len(n)), "", "")))[seq_len(n)]
  }
  # guarantee uniqueness without a rejection loop
  authors <- paste0(sample(first, pool, TRUE), " ",
                    sample(last, pool, TRUE), " ", seq_len(pool))
  collab  <- paste0("HEP ", sample(last, mega_pool, TRUE), " ", seq_len(mega_pool))

  k <- pmax(1L, pmin(200L, as.integer(round(rlnorm(n_papers, meanlog, sdlog)))))

  rows <- vapply(seq_len(n_papers), function(i) {
    # preferential attachment-ish: bias toward the low end of the pool so some
    # authors are prolific, which is what creates real community structure
    idx <- sample.int(pool, k[i], replace = FALSE,
                      prob = 1 / sqrt(seq_len(pool)))
    paste(authors[idx], collapse = delim)
  }, character(1))

  if (n_mega > 0L) {
    mega <- vapply(seq_len(n_mega), function(i) {
      paste(sample(collab, min(mega_size, mega_pool)), collapse = delim)
    }, character(1))
    rows <- c(rows, mega)
    k    <- c(k, rep(mega_size, n_mega))
  }

  n <- length(rows)
  data.frame(
    Title     = paste("Synthetic publication", seq_len(n)),
    Authors   = rows,
    Year      = sample(2005:2025, n, TRUE),
    Citations = as.integer(rnbinom(n, size = 1.2, mu = 18)),
    Journal   = sample(paste("Journal of Test Volume", 1:40), n, TRUE),
    Qscore    = sample(c("Q1","Q2","Q3","Q4",NA), n, TRUE,
                       prob = c(.3,.25,.2,.15,.1)),
    Source    = "synthetic",
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Benchmark one or more network builders on the same fixture.
#
# `variants` is a named list of argument lists passed to
# build_collaboration_network(). Everything is wrapped in tryCatch so one
# variant blowing the heap does not lose the results of the others.
# -----------------------------------------------------------------------------
mcp_benchmark_network <- function(df, variants = NULL, target_col = "Authors",
                                  target_delim = ", ", reps = 1L) {

  if (is.null(variants)) {
    variants <- list(
      "newman+star (default)"  = list(weight_scheme = "newman", oversize_mode = "star"),
      "count+drop"             = list(weight_scheme = "count",  oversize_mode = "drop"),
      "newman+star+backbone"   = list(weight_scheme = "newman", oversize_mode = "star",
                                      backbone_alpha = 0.05),
      "newman+clique (slow)"   = list(weight_scheme = "newman", oversize_mode = "clique")
    )
  }

  out <- lapply(names(variants), function(nm) {
    args <- variants[[nm]]
    args$df <- df
    args$target_col <- target_col
    args$target_delim <- target_delim
    if (is.null(args$node_freq_range))    args$node_freq_range    <- c(1, Inf)
    if (is.null(args$edge_freq_range))    args$edge_freq_range    <- c(1, Inf)
    if (is.null(args$cluster_size_range)) args$cluster_size_range <- c(1, Inf)
    if (is.null(args$authors_per_pub))    args$authors_per_pub    <- c(1, 100)

    times <- numeric(reps); nodes <- NA_integer_; edges <- NA_integer_
    err <- NA_character_; mem <- NA_real_

    for (r in seq_len(reps)) {
      gc(reset = TRUE, full = TRUE)
      t0 <- proc.time()[["elapsed"]]
      res <- tryCatch(do.call(build_collaboration_network, args),
                      error = function(e) { err <<- conditionMessage(e); NULL })
      times[r] <- proc.time()[["elapsed"]] - t0
      if (!is.null(res)) {
        nodes <- nrow(res$nodes); edges <- nrow(res$edges)
      }
      mem <- max(mem, sum(gc()[, "max used"] * c(8, 56)) / 1e6, na.rm = TRUE)
    }

    data.frame(variant = nm,
               secs_median = round(stats::median(times), 3),
               secs_min = round(min(times), 3),
               nodes = nodes, edges = edges,
               peak_MB = round(mem, 1),
               error = err,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Head-to-head against the original join-based implementation, if it is still
# reachable under another name. Rename the old one to
# `build_collaboration_network_legacy` before calling this.
mcp_benchmark_vs_legacy <- function(df, ...) {
  if (!exists("build_collaboration_network_legacy"))
    return(data.frame(note = "define build_collaboration_network_legacy first"))
  t0 <- proc.time()[["elapsed"]]
  old <- tryCatch(build_collaboration_network_legacy(df, ...), error = function(e) NULL)
  t_old <- proc.time()[["elapsed"]] - t0
  t1 <- proc.time()[["elapsed"]]
  new <- tryCatch(build_collaboration_network(df, ...), error = function(e) NULL)
  t_new <- proc.time()[["elapsed"]] - t1
  data.frame(
    impl  = c("legacy", "sparse"),
    secs  = round(c(t_old, t_new), 3),
    nodes = c(if (is.null(old)) NA else nrow(old$nodes),
              if (is.null(new)) NA else nrow(new$nodes)),
    edges = c(if (is.null(old)) NA else nrow(old$edges),
              if (is.null(new)) NA else nrow(new$edges)),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Static dependency scan: which packages do the sources actually need, and are
# they installable in this WASM runtime? This is the check that would have
# caught ggVennDiagram (called via ::, require() commented out).
# -----------------------------------------------------------------------------
mcp_scan_deps <- function(dir = "/collabnet") {
  files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  pkgs <- unlist(lapply(files, function(f) {
    txt <- readLines(f, warn = FALSE)
    txt <- txt[!grepl("^\\s*#", txt)]                    # ignore commented-out
    a <- regmatches(txt, gregexpr("(?<=\\b)[A-Za-z][A-Za-z0-9.]*(?=::)", txt, perl = TRUE))
    b <- regmatches(txt, gregexpr("(?<=require\\()[A-Za-z][A-Za-z0-9.]*", txt, perl = TRUE))
    d <- regmatches(txt, gregexpr("(?<=library\\()[A-Za-z][A-Za-z0-9.]*", txt, perl = TRUE))
    unlist(c(a, b, d))
  }))
  pkgs <- sort(unique(pkgs))
  pkgs <- setdiff(pkgs, c("base","utils","stats","methods","tools","grDevices",
                          "graphics","datasets","self","e","x","T","F"))
  data.frame(
    package   = pkgs,
    available = vapply(pkgs, mcp_has, logical(1)),
    stringsAsFactors = FALSE
  )
}

# Which packages are referenced only through `pkg::` with no require()/library()
# anywhere — the exact shape of BUG-02.
mcp_undeclared_deps <- function(dir = "/collabnet") {
  files <- list.files(dir, pattern = "\\.R$", full.names = TRUE)
  ns <- c(); dec <- c()
  for (f in files) {
    txt <- readLines(f, warn = FALSE)
    txt <- txt[!grepl("^\\s*#", txt)]
    ns  <- c(ns, unlist(regmatches(txt, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)", txt, perl = TRUE))))
    dec <- c(dec, unlist(regmatches(txt, gregexpr("(?<=require\\()[A-Za-z][A-Za-z0-9.]*", txt, perl = TRUE))),
             unlist(regmatches(txt, gregexpr("(?<=library\\()[A-Za-z][A-Za-z0-9.]*", txt, perl = TRUE))))
  }
  setdiff(sort(unique(ns)), sort(unique(dec)))
}

# -----------------------------------------------------------------------------
# Invariant tests. These are the assertions I could not run when reviewing the
# code; run them after any change to the graph engine or the fixes.
# -----------------------------------------------------------------------------
mcp_tests <- function() {
  t <- list()
  ok <- function(name, expr) {
    r <- tryCatch(isTRUE(expr), error = function(e) paste("ERR:", conditionMessage(e)))
    t[[length(t) + 1L]] <<- data.frame(test = name, pass = isTRUE(r),
                                       detail = if (isTRUE(r)) "" else as.character(r),
                                       stringsAsFactors = FALSE)
  }

  ok("h-index >= not >",            compute_h_index(c(3, 3, 3)) == 3L)
  ok("h-index zero vector",         compute_h_index(c(0, 0)) == 0L)
  ok("h-index handles NA",          compute_h_index(c(5, NA, 5, 1)) == 2L)
  ok("g-index basic",               compute_g_index(c(10, 8, 6, 1)) == 4L)

  ok("split_authors semicolons",
     identical(split_authors("Smith, J.; Doe, A."), c("J. Smith", "A. Doe")))
  ok("split_authors plain list",
     length(split_authors("Alice Adeyemi, Bruno Bakker, Chen Costa")) == 3L)

  ok("decide_label always returns token_count",
     !is.null(decide_label_for_target("Nobody Here", list(), NULL)$token_count))

  # two papers, one shared pair: Newman weight must be 1/(k-1) summed
  d <- data.frame(Authors = c("A One, B Two", "A One, B Two, C Three"),
                  stringsAsFactors = FALSE)
  n <- build_collaboration_network(d, target_delim = ", ",
                                   node_freq_range = c(1, Inf),
                                   edge_freq_range = c(1, Inf),
                                   cluster_size_range = c(1, Inf),
                                   prune_leaves = FALSE,
                                   detect_communities = FALSE,
                                   compute_centrality = FALSE)
  ab <- n$edges[(n$edges$from == "A One" & n$edges$to == "B Two") |
                (n$edges$from == "B Two" & n$edges$to == "A One"), ]
  ok("newman weight A-B == 1 + 0.5", nrow(ab) == 1L && abs(ab$weight[1] - 1.5) < 1e-6)
  ok("raw count A-B == 2",           nrow(ab) == 1L && ab$connections[1] == 2L)

  # hyperauthored paper must not be expanded under star mode
  big <- data.frame(Authors = paste(paste0("P", 1:400), collapse = ", "),
                    stringsAsFactors = FALSE)
  s <- build_collaboration_network(big, authors_per_pub = c(1, 100),
                                   oversize_mode = "star",
                                   node_freq_range = c(1, Inf),
                                   edge_freq_range = c(1, Inf),
                                   cluster_size_range = c(1, Inf),
                                   prune_leaves = FALSE,
                                   detect_communities = FALSE,
                                   compute_centrality = FALSE)
  ok("star folds 400-author paper to <= 400 edges", nrow(s$edges) <= 400L)

  ok("upper triplets are strictly upper",
     { m <- Matrix::crossprod(Matrix::sparseMatrix(i = c(1,1,2), j = c(1,2,2), x = 1))
       tr <- cn_upper_triplets(m); all(tr$i < tr$j) })

  ok("cn_match_targets is word-bounded",
     !cn_match_targets("Elias Ng", "Li") && cn_match_targets("Li Wei", "Li"))

  do.call(rbind, t)
}

mcp_sysinfo <- function() {
  list(
    r_version = R.version.string,
    machine   = unname(Sys.info()[["machine"]]),
    is_wasm   = grepl("wasm", Sys.info()[["machine"]], fixed = TRUE),
    wd        = getwd(),
    collabnet_files = list.files("/collabnet", pattern = "\\.R$"),
    key_functions = vapply(
      c("build_collaboration_network", "cn_project", "cn_slider_bounds",
        "compute_h_index", "split_authors", "decide_label_for_target",
        "extend_input_table", "compute_indices"),
      function(f) exists(f, mode = "function"), logical(1))
  )
}
