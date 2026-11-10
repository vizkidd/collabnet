# require(shinylive)
# require(httpuv)
# # install.packages("pak")
# # pak::pak("r-wasm/rwasm")
# require(rwasm)
# # Update the R package
# pak::pak("posit-dev/r-shinylive")
# # Force a fresh download of the WebAssembly assets
# shinylive::assets_download()

# E2 CLI:  Rscript buildWASM.R [--target=ghpages|workers|pages|both] [--dry-run] [--serve] [--base-path=/|/collabnet/]
#          [--gate-api=https://collabnet.<account>.workers.dev]   (E3c: the ONE donation Worker; see DEPLOY_CLOUDFLARE.md)
#                               [--project-name=collabnet] [--paid-plan] [--skip-export]
# Default --target=ghpages = the previous behaviour (CI runs plain `Rscript buildWASM.R`). Cloudflare targets stage
# wasm_build/ via tools/cf_deploy.mjs (Node >= 18, pinned wrangler; see DEPLOY_CLOUDFLARE.md). --skip-export reuses
# an existing wasm_build/ (or --build-dir=DIR) instead of running shinylive::export.
cn_build_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  o <- list(target = "ghpages", dry_run = FALSE, serve = FALSE, base_path = "/", project_name = "collabnet",
            paid_plan = FALSE, skip_export = FALSE, build_dir = "wasm_build", gate_api = NULL)
  for (a in args) {
    kv <- regmatches(a, regexec("^--([a-z-]+)(=(.*))?$", a))[[1]]
    if (!length(kv)) stop("buildWASM: unexpected argument ", a, call. = FALSE)
    k <- kv[2]; v <- kv[4]
    switch(k,
      "target" = { if (!v %in% c("ghpages", "workers", "pages", "both")) stop("buildWASM: --target must be ghpages|workers|pages|both", call. = FALSE); o$target <- v },
      "dry-run" = o$dry_run <- TRUE, "serve" = o$serve <- TRUE, "paid-plan" = o$paid_plan <- TRUE, "skip-export" = o$skip_export <- TRUE,
      "base-path" = { if (!grepl("^/?([A-Za-z0-9._-]+/?)*$", v)) stop("buildWASM: bad --base-path ", v, call. = FALSE)
                      o$base_path <- if (nzchar(gsub("/", "", v))) paste0("/", gsub("^/+|/+$", "", v), "/") else "/" },
      "project-name" = o$project_name <- v, "build-dir" = o$build_dir <- v, "gate-api" = o$gate_api <- v,
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
BUILD <- cn_build_args()
# E3c: gate API baked into the bundle: --gate-api, else same-origin for Cloudflare targets (the Worker serves the app),
# else (ghpages) the committed default in gate_config.json (placeholder -> no Worker -> honour button).
cn_gate_env <- new.env(); sys.source("GScholarLENS-Gate.R", envir = cn_gate_env)
BUILD$gate_api_resolved <- cn_gate_env$cn_gate_api_normalise(
  if (!is.null(BUILD$gate_api)) BUILD$gate_api else if (BUILD$target != "ghpages") "same-origin" else { f <- cn_gate_env$cn_gate_api_config_file(); if (is.null(f)) "" else f })
message("buildWASM: gate API = ", if (nzchar(BUILD$gate_api_resolved)) BUILD$gate_api_resolved else "(none: honour button only)",
        if (!is.null(BUILD$gate_api)) " [--gate-api]" else if (BUILD$target != "ghpages") " [default for Cloudflare targets]" else " [gate_config.json]")
if (BUILD$skip_export && !is.null(BUILD$gate_api)) warning("buildWASM: --gate-api is baked at export time; ignored with --skip-export", call. = FALSE)
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
  "GScholarLENS-Fixes.R", "GScholarLENS-MCP.R",
  "GScholarLENS-Console.R", "GScholarLENS-Parsers.R", "GScholarLENS-ShWeights.R", "GScholarLENS-CSP.R",
  "GScholarLENS-Keys.R", "GScholarLENS-Gate.R"   # E1: export gate
)

# cat("WASM <- TRUE", file="staging_app/app.R")
# # Read and concatenate helper scripts and app files
# worker_js_content <- if (file.exists(worker_js_path)) paste(readLines(worker_js_path, warn = FALSE), collapse = "\n") else ""

# P1-5: build provenance, embedded in the bundled app (shown in About, written to .cnet and the methods text).
.bi_commit <- Sys.getenv("GITHUB_SHA", tryCatch(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)[1], error = function(e) NA_character_))
.bi_assets <- tryCatch(shinylive::assets_info(quiet = TRUE), error = function(e) NULL)
collabnet_build_info <- list(
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
  lines <- grep(pattern = "^\\s*source\\s*\\(", x = lines, value = TRUE, invert = TRUE)   # E-23: anchored (was any line containing "source(...)")
  # lines <- grep(pattern = "require\\(httr2\\)", x = lines, value = TRUE, invert = TRUE)
  # if (nzchar(worker_js_content)) {
  #   worker_js_json <- jsonlite::toJSON(worker_js_content, auto_unbox = TRUE)
  #                 sprintf('tags$script(HTML(%s))', worker_js_json), lines)
  # }
  return(lines)
}))

gate_api_line <- paste0("collabnet_gate_api_baked <- ", deparse(BUILD$gate_api_resolved))   # E3c
cat(c(build_info_line, gate_api_line, app_content_lines), file="staging_app/app.R", sep="\n")

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
if (!file.exists("tools/csp_inject.mjs")) stop("buildWASM: tools/csp_inject.mjs not found (P3-6/E2b)", call. = FALSE)
csp_status <- system2("node", c("tools/csp_inject.mjs", "wasm_build/index.html",
                               if (!is.null(cn_gate_env$cn_gate_api_origin(BUILD$gate_api_resolved))) c("--extra-connect", cn_gate_env$cn_gate_api_origin(BUILD$gate_api_resolved))))   # E3c
if (!identical(csp_status, 0L)) stop("buildWASM: CSP injection (tools/csp_inject.mjs) failed; refusing to ship wasm_build/index.html without the CSP meta", call. = FALSE)
# E1/E2/E3: donation policy + retired success/failure notices (deploy_static/README.md) next to the app:
# wasm_build/success/, failure/, refund-policy/ -> https://vizkidd.github.io/collabnet/success/ etc.
if (!fs::dir_exists("deploy_static")) stop("buildWASM: deploy_static/ not found (E1/E3 static pages)")
for (f in setdiff(list.files("deploy_static", all.files = FALSE), "README.md")) {
  src <- file.path("deploy_static", f)
  if (fs::dir_exists(src)) fs::dir_copy(src, file.path("wasm_build", f), overwrite = TRUE) else fs::file_copy(src, file.path("wasm_build", f), overwrite = TRUE)
}
# D3-6/7: tab title "CollabNET" + favicon links/files in the outer shell (the app itself runs in an iframe).
# E2: Node port tools/html_head_patch.mjs (byte-identical to html_head_patch.py, kept one more release).
if (!file.exists("tools/html_head_patch.mjs")) stop("buildWASM: tools/html_head_patch.mjs not found (D3-6/E2)")
head_status <- system2("node", c("tools/html_head_patch.mjs", "wasm_build/index.html", "--title", "CollabNET", "--icons-from", "www"))
if (!identical(head_status, 0L)) stop("buildWASM: html_head_patch.mjs failed (no <head> in wasm_build/index.html?)")
# D1-7 / D2-A1: guard the Shinylive service worker against 'only-if-cached' requests (see sw_patch.py).
# Fails the build loudly (instead of warning) if node is missing (cn_node_check above) or the fetch-handler
# pattern is not found, e.g. after a Shinylive upgrade changed the service worker shape.
# E2: Node port tools/sw_patch.mjs (byte-identical to sw_patch.py, kept one more release).
if (!file.exists("tools/sw_patch.mjs")) stop("buildWASM: tools/sw_patch.mjs not found (D2-A1/E2)")
sw_status <- system2("node", c("tools/sw_patch.mjs", "wasm_build/shinylive-sw.js"))
if (!identical(sw_status, 0L)) stop("buildWASM: sw_patch.mjs failed; the Shinylive fetch-handler pattern was not found in wasm_build/shinylive-sw.js")

# E2: Cloudflare targets (ghpages: nothing more to do, CI uploads wasm_build/ as before).
cf_targets <- switch(BUILD$target, ghpages = character(0), both = c("workers", "pages"), BUILD$target)
for (t in cf_targets) {
  st <- system2("node", c("tools/cf_deploy.mjs", paste0("--target=", t), "--src=wasm_build", "--out=cf_build",
                          paste0("--base-path=", BUILD$base_path), paste0("--project-name=", BUILD$project_name),
                          if (BUILD$paid_plan) "--paid-plan", if (BUILD$dry_run) "--dry-run", if (BUILD$serve) "--serve"))
  if (!identical(st, 0L)) stop("buildWASM: Cloudflare ", t, " step failed (exit ", st, "); see messages above", call. = FALSE)
}

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

httpuv::runStaticServer("./wasm_build/", headers = list()) #host = "0.0.0.0"

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