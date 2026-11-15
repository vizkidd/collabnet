# require(shinylive)
# require(httpuv)
# # install.packages("pak")
# # pak::pak("r-wasm/rwasm")
# require(rwasm)
# # Update the R package
# pak::pak("posit-dev/r-shinylive")
# # Force a fresh download of the WebAssembly assets
# shinylive::assets_download()

# CLI:  Rscript buildWASM.R [--skip-export] [--serve-local]
# P10-A7: --serve-local (or env CN_SERVE=1) serves wasm_build/ with httpuv at the end; default off (prints the command).
# --skip-export reuses an existing wasm_build/ (or --build-dir=DIR) instead of running shinylive::export.
# P10-A8: reads only files inside this directory (edition dir or repo root); missing inputs fail before the export.
cn_build_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  o <- list(target = "ghpages", dry_run = FALSE, serve = FALSE, base_path = "/", project_name = "collabnet",
            paid_plan = FALSE, skip_export = FALSE, build_dir = "wasm_build",
            serve_local = identical(Sys.getenv("CN_SERVE"), "1"))
  for (a in args) {
    kv <- regmatches(a, regexec("^--([a-z-]+)(=(.*))?$", a))[[1]]
    if (!length(kv)) stop("buildWASM: unexpected argument ", a, call. = FALSE)
    k <- kv[2]; v <- kv[4]
    switch(k,
      "target" = { if (!v %in% c("ghpages", "workers", "pages", "both")) stop("buildWASM: --target must be ghpages|workers|pages|both", call. = FALSE); o$target <- v },
      "dry-run" = o$dry_run <- TRUE, "serve" = o$serve <- TRUE, "paid-plan" = o$paid_plan <- TRUE, "skip-export" = o$skip_export <- TRUE,
      "serve-local" = o$serve_local <- TRUE,
      "base-path" = { if (!grepl("^/?([A-Za-z0-9._-]+/?)*$", v)) stop("buildWASM: bad --base-path ", v, call. = FALSE)
                      o$base_path <- if (nzchar(gsub("/", "", v))) paste0("/", gsub("^/+|/+$", "", v), "/") else "/" },
      "project-name" = o$project_name <- v, "build-dir" = o$build_dir <- v,
      stop("buildWASM: unknown option --", k, call. = FALSE))
  }
  if (o$serve && o$target %in% c("both", "ghpages")) stop("buildWASM: --serve needs --target=workers or --target=pages", call. = FALSE)
  o
}
cn_node_check <- function() {
  node <- Sys.which("node")
  if (!nzchar(node) || !nzchar(Sys.which("npx")))
    stop("buildWASM: Node.js >= 18 (with npm/npx) is required for the post-build patches and Cloudflare deploys; install it from https://nodejs.org/", call. = FALSE)
  v <- suppressWarnings(as.integer(sub("^v([0-9]+).*", "\\1", system2(node, "--version", stdout = TRUE)[1])))
  if (is.na(v) || v < 18) stop("buildWASM: Node.js >= 18 required (found ", v, ")", call. = FALSE)
  invisible(TRUE)
}
# P10-A8: every edition dir is self-contained. Build inputs are taken ONLY from the working dir (no parent/repo-root
# fallback: that would leak another edition's files into this bundle). cn_build_preflight() checks them all BEFORE the
# slow shinylive export and stops with the full list of what is missing.
cn_build_input <- function(path) {
  if (file.exists(path)) return(path)
  stop("buildWASM: ", path, " not found in ", normalizePath(getwd()), " (this edition dir is incomplete; regenerate it with ",
       "Rscript editions/build_editions.R)", call. = FALSE)
}
CN_BUILD_INPUTS <- c("app.R", "ui.R", "server.R", "edition.R", "GScholarLENS-Edition.R", "GScholarLENS-Runtime.R", "www",
                     "tools/csp_inject.mjs", "tools/html_head_patch.mjs", "tools/sw_patch.mjs")
CN_BUILD_CLOUDFLARE <- FALSE
cn_build_preflight <- function(inputs = CN_BUILD_INPUTS) {
  if (!file.exists("edition.R") && dir.exists("editions")) inputs <- setdiff(inputs, "edition.R")   # repo root = full
  miss <- inputs[!file.exists(inputs)]
  if (length(miss)) stop("buildWASM: missing build input(s) in ", normalizePath(getwd()), ": ", paste(miss, collapse = ", "),
                         "\n  Regenerate the edition dir: Rscript editions/build_editions.R --out=builds", call. = FALSE)
  invisible(TRUE)
}
BUILD <- cn_build_args()
cn_build_preflight()
if (!CN_BUILD_CLOUDFLARE && BUILD$target != "ghpages")
  stop("buildWASM: this edition builds the static bundle only (--target=ghpages); Cloudflare targets belong to the full edition", call. = FALSE)
baked_lines <- character(0); extra_origin <- NULL   # P10-A8: filled only by the export block (full)
cn_node_check()

if (!BUILD$skip_export) {   # E2: --skip-export reuses an existing build dir
# 1. Create a clean staging directory
if(fs::dir_exists("wasm_build"))
  fs::dir_delete("wasm_build")
if(fs::dir_exists("staging_app"))
  fs::dir_delete("staging_app")
fs::dir_create("staging_app")

# 2. Copy only required files (e.g., app.R, your sourced scripts, and the Excel file)
app_files <- c("ui.R", "server.R", "app.R")#, "2024-JCR_IMPACT_FACTOR.xlsx")
# Also include your helper scripts
helper_scripts <- c(
  "GScholarLENS-DOI2Data.R", "GScholarLENS-ORCID2Data.R",
  "GScholarLENS-SCOPUS2Data.R", "GScholarLENS-ProcessJCR.R", "GScholarLENS-Session.R",
  "GScholarLENS-Data2GLENS.R", "GScholarLENS-PlotGLENS.R",
  "GScholarLENS-Graph.R", "GScholarLENS-helpers.R",
  "GScholarLENS-Fixes.R",
  "GScholarLENS-Runtime.R", "GScholarLENS-ShWeights.R", "GScholarLENS-CSP.R",
  "GScholarLENS-Keys.R"
)
# P10-A8: module files are named only inside their edition blocks (they are not shipped in the other editions)
helper_scripts <- c("edition.R", "GScholarLENS-Edition.R", helper_scripts)   # P10-A: edition constants first
helper_scripts <- helper_scripts[file.exists(helper_scripts)]   # edition.R is absent in the repo root (= full)

# cat("WASM <- TRUE", file="staging_app/app.R")
# # Read and concatenate helper scripts and app files
# worker_js_content <- if (file.exists(worker_js_path)) paste(readLines(worker_js_path, warn = FALSE), collapse = "\n") else ""

# P1-5: build provenance, embedded in the bundled app (shown in About, written to .cnet and the methods text).
.bi_commit <- Sys.getenv("GITHUB_SHA", tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1], error = function(e) NA_character_))
.bi_assets <- tryCatch(shinylive::assets_info(quiet = TRUE), error = function(e) NULL)
.bi_edition <- if (file.exists("edition.R")) { .e <- new.env(); sys.source("edition.R", .e); get0("COLLABNET_EDITION", .e, ifnotfound = "full") } else "full"
collabnet_build_info <- list(
  edition = .bi_edition,   # P10-B: the full site build checks that its Free input really is a free build
  commit = substr(.bi_commit, 1, 12), built_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  shinylive_r = tryCatch(as.character(utils::packageVersion("shinylive")), error = function(e) NA_character_),
  shinylive_assets = tryCatch(as.character(shinylive::assets_version()), error = function(e) NA_character_),
  webR = tryCatch({ f <- list.files(file.path(dirname(shinylive::assets_dir()), shinylive::assets_version(), "shinylive", "webr"), "^webr\\.mjs$", full.names = TRUE)
                    v <- if (length(f)) regmatches(readLines(f[1], warn = FALSE), regexpr("[0-9]+\\.[0-9]+\\.[0-9]+(-[a-z0-9.]+)?", readLines(f[1], warn = FALSE))) else character(0)
                    if (length(v)) v[1] else NA_character_ }, error = function(e) NA_character_),
  R_build = paste(R.version$major, R.version$minor, sep = "."))
build_info_line <- paste0("collabnet_build_info <- ", paste(deparse(collabnet_build_info), collapse = " "))

app_content_lines <- unlist(lapply(c(helper_scripts, app_files), function(x){
  lines <- readLines(x)
  lines <- grep(pattern = "^\\s*source\\s*\\(", x = lines, value = TRUE, invert = TRUE)
  # P10-A8: also `if (!exists("f")) source("X.R")` guards (ui.R): in the bundle every file is already concatenated, and
  # webR has no X.R on disk, so a guard whose symbol is stripped by the edition would try to open it (preload error).
  lines <- grep(pattern = "^\\s*if\\s*\\(\\s*!\\s*exists\\([^)]*\\)[^)]*\\)\\s*source\\s*\\(", x = lines, value = TRUE, invert = TRUE)   # E-23: anchored (was any line containing "source(...)")
  # lines <- grep(pattern = "require\\(httr2\\)", x = lines, value = TRUE, invert = TRUE)
  # if (nzchar(worker_js_content)) {
  #   worker_js_json <- jsonlite::toJSON(worker_js_content, auto_unbox = TRUE)
  #                 sprintf('tags$script(HTML(%s))', worker_js_json), lines)
  # }
  return(lines)
}))

# P10-A8: stringdist's .onLoad asks parallel::detectCores(), which is NA in webR, and prints "Could not detect number of
# cores, defaulting to 1." as a preload message. It honours OMP_NUM_THREADS first, so set it (webR is single-threaded).
omp_line <- 'if (!nzchar(Sys.getenv("OMP_NUM_THREADS"))) Sys.setenv(OMP_NUM_THREADS = "1")'
cat(c(omp_line, build_info_line, baked_lines, app_content_lines), file="staging_app/app.R", sep="\n")

fs::dir_copy("www/","staging_app/")
# fs::file_copy(c(app_files, helper_scripts), "staging_app/")
#fs::file_copy("2024-JCR_IMPACT_FACTOR.xlsx", "staging_app/")

# rwasm::add_pkg("/media/everest/disk01/vishvesh/QuickBLAST/", repo_dir = "./repo") 

# remotes::install_github("posit-dev/r-shinylive")

# D3-6: shinylive's export template has an optional {{#title}} slot (shinylive memory-bank/quartoAndExport.md:90-95);
# pass it when this shinylive version supports template_params. html_head_patch.py below enforces it either way.
if ("template_params" %in% names(formals(shinylive::export))) {
  shinylive::export(appdir = "./staging_app/", destdir = "./wasm_build/", wasm_packages=T, quiet=F,
                    template_params = list(title = "CollabNET"))
} else {
  shinylive::export(appdir = "./staging_app/", destdir = "./wasm_build/", wasm_packages=T, quiet=F)
}
# P1-5: record versions next to the bundle (CI prints it; also embedded in app.R above)
jsonlite::write_json(collabnet_build_info, "wasm_build/collabnet_build_info.json", auto_unbox = TRUE, pretty = TRUE)
}   # end if (!BUILD$skip_export)
if (BUILD$skip_export && !identical(BUILD$build_dir, "wasm_build")) {
  if (fs::dir_exists("wasm_build")) fs::dir_delete("wasm_build"); fs::dir_copy(BUILD$build_dir, "wasm_build")
}
# P3-6: inject the Content-Security-Policy <meta> into the exported shell (idempotent; untested in a browser)
# E2b: Node port tools/csp_inject.mjs (byte-identical to csp_inject.py); fails the build instead of shipping without CSP.
csp_js <- cn_build_input("tools/csp_inject.mjs")   # P3-6/E2b
csp_status <- system2("node", c(csp_js, "wasm_build/index.html", if (!is.null(extra_origin)) c("--extra-connect", extra_origin)))
if (!identical(csp_status, 0L)) stop("buildWASM: CSP injection (tools/csp_inject.mjs) failed; refusing to ship wasm_build/index.html without the CSP meta", call. = FALSE)
# P10-B: detailed loading screen (free + full; demo keeps Shinylive's default). Copied next to index.html like the session
# guard; the build-time byte sizes go on the script tag (data-*) because webR downloads in a worker the page cannot see.
local({
  wb <- "wasm_build"; sl <- file.path(wb, "shinylive"); pk <- file.path(sl, "webr", "packages")
  sz <- function(fs_) sum(file.size(fs_), na.rm = TRUE)
  all_sl <- list.files(sl, recursive = TRUE, full.names = TRUE); in_pk <- startsWith(all_sl, paste0(pk, "/"))
  rt <- sz(all_sl[!in_pk]); pb <- sz(all_sl[in_pk]); ab <- sz(file.path(wb, "app.json"))
  np <- length(list.dirs(pk, recursive = FALSE))
  file.copy(cn_build_input("www/js/collabnet-loader.js"), file.path(wb, "collabnet-loader.js"), overwrite = TRUE)
  file.copy(cn_build_input("www/css/collabnet-loader.css"), file.path(wb, "collabnet-loader.css"), overwrite = TRUE)
  idx <- file.path(wb, "index.html"); h <- readLines(idx, warn = FALSE, encoding = "UTF-8")
  h <- h[!grepl("data-collabnet-loader", h, fixed = TRUE)]
  tag <- c('<link rel="stylesheet" href="collabnet-loader.css" data-collabnet-loader="1">',
           sprintf('<script src="collabnet-loader.js" data-collabnet-loader="1" data-runtime-bytes="%.0f" data-packages-bytes="%.0f" data-app-bytes="%.0f" data-packages="%d"></script>', rt, pb, ab, np))
  at <- grep("</head>", h, fixed = TRUE)[1]
  if (is.na(at)) stop("buildWASM: no </head> in wasm_build/index.html for the loader", call. = FALSE)
  h <- append(h, tag, after = at - 1)
  con <- file(idx, open = "w", encoding = "UTF-8"); writeLines(h, con); close(con)
  message(sprintf("loader: runtime %.1f MB, %d packages %.1f MB, app.json %.1f MB", rt / 2^20, np, pb / 2^20, ab / 2^20))
})
# D3-6/7: tab title "CollabNET" + favicon links/files in the outer shell (the app itself runs in an iframe).
# E2: Node port tools/html_head_patch.mjs (byte-identical to html_head_patch.py, kept one more release).
head_status <- system2("node", c(cn_build_input("tools/html_head_patch.mjs"), "wasm_build/index.html", "--title", "CollabNET", "--icons-from", "www"))
if (!identical(head_status, 0L)) stop("buildWASM: html_head_patch.mjs failed (no <head> in wasm_build/index.html?)")
# D1-7 / D2-A1: guard the Shinylive service worker against 'only-if-cached' requests (see sw_patch.py).
# Fails the build loudly (instead of warning) if node is missing (cn_node_check above) or the fetch-handler
# pattern is not found, e.g. after a Shinylive upgrade changed the service worker shape.
# E2: Node port tools/sw_patch.mjs (byte-identical to sw_patch.py, kept one more release).
sw_status <- system2("node", c(cn_build_input("tools/sw_patch.mjs"), "wasm_build/shinylive-sw.js"))
if (!identical(sw_status, 0L)) stop("buildWASM: sw_patch.mjs failed; the Shinylive fetch-handler pattern was not found in wasm_build/shinylive-sw.js")


# # -----------------------------------------------------------------------------
# # 3. FAST COLD-START OPTIMIZATION: Inject packages into library.data.gz
# # -----------------------------------------------------------------------------
# # Pre-bakes all packages from wasm_build/shinylive/webr/packages/ directly into
# # the WebR virtual filesystem (library.data.gz & library.js.metadata).
# # This avoids downloading ~60 .tgz packages via HTTP on cold start, dropping boot
# # time from >90s to <5s and enabling full offline operation.
# message("\n=== Injecting downloaded packages into library.data.gz (VFS pre-bake) ===")
# if (file.exists("inject_packages.py")) {
#   inject_status <- system("python3 inject_packages.py wasm_build")
#   if (inject_status == 0) {
#     message("Successfully pre-baked packages into library.data.gz!")
#   } else {
#     warning("Package injection script returned non-zero exit code: ", inject_status)
#   }
# } else {
#   message("inject_packages.py not found in working directory; skipping VFS injection.")
# }


# #Download and install the latest version of webR from github
# # Load required package for API parsing (install.packages("jsonlite") if needed)
# if (!requireNamespace("jsonlite", quietly = TRUE)) {
#   stop("The 'jsonlite' package is required but not installed.")
# }

# # 1. Define your target directory (relative to your working directory)
# target_dir <- "wasm_build/shinylive/webr"

# # Create the target directory if it doesn't exist
# if (!dir.exists(target_dir)) {
#   dir.create(target_dir, recursive = TRUE)
# }

# # 2. Query the GitHub API for the latest webR release
# message("Querying GitHub API for the latest webR release...")
# api_url <- "https://api.github.com/repos/r-wasm/webr/releases/latest"
# release_data <- tryCatch({
#   jsonlite::fromJSON(api_url)
# }, error = function(e) {
#   stop("Failed to fetch release data from GitHub API. Check your internet connection or API rate limits.")
# })

# # 3. Extract the download URL for the .tar.gz asset
# assets <- release_data$assets
# tar_asset <- assets[grepl("webr-.*\\.tar\\.gz$", assets$name), ]

# if (nrow(tar_asset) == 0) {
#   stop("Could not find a .tar.gz file in the latest release assets.")
# }

# download_url <- tar_asset$browser_download_url[1]
# tar_filename <- tar_asset$name[1]

# # 4. Set up an isolated temporary workspace
# tmp_dir <- tempdir()
# dest_file <- file.path(tmp_dir, tar_filename)
# extract_dir <- file.path(tmp_dir, "webr_extracted")

# # Clean up previous extraction attempts in temp if they exist
# if (dir.exists(extract_dir)) unlink(extract_dir, recursive = TRUE)
# dir.create(extract_dir)

# # 5. Download the release asset
# message(sprintf("Downloading %s...", tar_filename))
# download.file(url = download_url, destfile = dest_file, mode = "wb", quiet = FALSE)

# # 6. Extract the archive
# message("Extracting archive...")
# untar(tarfile = dest_file, exdir = extract_dir)

# # 7. Locate the extracted contents
# # webR usually extracts into a subfolder named "webr" inside the exdir
# extracted_subdirs <- list.dirs(extract_dir, recursive = FALSE)
# webr_folder <- extracted_subdirs[grepl("webr", basename(extracted_subdirs))]

# # Fallback in case it extracted everything directly into the root of extract_dir
# if (length(webr_folder) == 0) {
#   webr_folder <- extract_dir
# } else {
#   webr_folder <- webr_folder[1]
# }

# # 8. Copy files over to your project directory
# message(sprintf("Syncing files to %s...", target_dir))
# files_to_copy <- list.files(webr_folder, full.names = TRUE)

# # Copy each item (files and directories) into the target directory
# for (item in files_to_copy) {
#   file.copy(
#     from = item, 
#     to = target_dir, 
#     recursive = TRUE, 
#     overwrite = TRUE
#   )
# }

# # 9. Clean up
# message("Cleaning up temporary files...")
# unlink(dest_file)
# unlink(extract_dir, recursive = TRUE)

# message("Success! The latest version of webR has been deployed.")

# httpuv::runStaticServer("./wasm_build/", headers = list(
#   "Cross-Origin-Opener-Policy" = "same-origin",
#   "Cross-Origin-Embedder-Policy" = "require-corp"
# )) #host = "0.0.0.0"

# P10-A7: serving is opt-in (--serve-local or CN_SERVE=1); by default the build ends here and prints the command,
# so `source("buildWASM.R")` / CI return instead of blocking on a static server.
if (isTRUE(BUILD$serve_local)) {
  httpuv::runStaticServer("./wasm_build/", headers = list()) #host = "0.0.0.0"
} else {
  message("buildWASM: done -> ", normalizePath("wasm_build"), "\n  serve it with: Rscript -e 'httpuv::runStaticServer(\"wasm_build\")'",
          "  (or rerun with --serve-local / CN_SERVE=1)")
}

# # httpuv::runServer(
# #   host = "127.0.0.1", port = 7446,
# #   app = list(
# #     staticPaths = list(
# #       "/" = httpuv::staticPath(
# #         "./wasm_build/",
# #         headers = list(
# #           "Cross-Origin-Opener-Policy" = "same-origin",
# #           "Cross-Origin-Embedder-Policy" = "require-corp"
# #         )
# #       )
# #     ),
# #     call = function(req) {
# #       list(
# #         status = 404L, 
# #         headers = list("Content-Type" = "text/plain"), 
# #         body = paste(req, "404 - Not Found")
# #       )
# #     }
# #   )
# # )
