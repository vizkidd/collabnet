# =============================================================================
# A-6 (audit High): stable machine identifier + non-lossy key unlocking.
#
# The local private key (keys/private.key.signed) is encrypted with a key
# derived from a machine identifier. Previously, without
# /var/lib/dbus/machine-id (or on macOS) that identifier was a fresh random UUID
# on every run, so decryption failed on the next start and the stored API keys
# were silently lost. These helpers:
#   * derive a stable identifier (OS id files, macOS IOPlatformUUID, Windows
#     BIOS serial, then a random id persisted ONCE in
#     tools::R_user_dir("collabnet", "config") with 0700/0600 permissions);
#   * report decryption failures as an explicit status instead of NULL/silence;
#   * never delete stored ciphertext (it is backed up before regeneration).
# All file / system access is wrapped in tryCatch so the code is webR-safe.
# =============================================================================

# Read the first line of an id file; return NA unless it looks like an id.
cn_read_id_file <- function(path) {
  tryCatch({
    if (!file.exists(path)) return(NA_character_)
    x <- trimws(readLines(path, n = 1L, warn = FALSE))
    if (length(x) == 1L && nchar(x) >= 8L && grepl("^[A-Za-z0-9-]+$", x)) x else NA_character_
  }, error = function(e) NA_character_, warning = function(w) NA_character_)
}

cn_random_id <- function() {
  id <- tryCatch(uuid::UUIDgenerate(), error = function(e) NA_character_)
  if (is.na(id) || !nzchar(id))
    id <- paste(sample(c(0:9, letters[1:6]), 32L, replace = TRUE), collapse = "")
  id
}

cn_default_config_dir <- function() {
  tryCatch(tools::R_user_dir("collabnet", which = "config"), error = function(e) NA_character_)
}

# Persisted fallback id: created once (dir 0700, file 0600), then re-used.
cn_persisted_machine_id <- function(config_dir = cn_default_config_dir()) {
  if (is.null(config_dir) || is.na(config_dir) || !nzchar(config_dir)) return(NA_character_)
  path <- file.path(config_dir, "machine-id")
  id <- cn_read_id_file(path)
  if (!is.na(id)) return(id)
  tryCatch({
    if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE, mode = "0700", showWarnings = FALSE)
    suppressWarnings(Sys.chmod(config_dir, mode = "0700", use_umask = FALSE))
    old <- Sys.umask("077"); on.exit(Sys.umask(old), add = TRUE)
    tmp <- tempfile("machine-id-", tmpdir = config_dir)
    suppressWarnings(writeLines(cn_random_id(), tmp))
    suppressWarnings(Sys.chmod(tmp, mode = "0600", use_umask = FALSE))
    # Do not clobber an id written concurrently by another process.
    if (!file.exists(path)) file.rename(tmp, path) else unlink(tmp)
    cn_read_id_file(path)
  }, error = function(e) NA_character_)
}

# Returns list(id, source, stable). `stable = FALSE` only for the last-resort
# per-session id (e.g. webR / unwritable config dir).
cn_machine_id <- function(config_dir = cn_default_config_dir(),
                          os_files = c("/var/lib/dbus/machine-id", "/etc/machine-id"),
                          os = NULL) {
  if (is.null(os)) os <- tryCatch({
    if (grepl("wasm", Sys.info()[["machine"]])) "wasm"
    else if (.Platform$OS.type == "windows") "windows"
    else if (Sys.info()[["sysname"]] == "Darwin") "macos" else "unix"
  }, error = function(e) "unknown")
  ok <- function(x) length(x) == 1L && !is.na(x) && nchar(x) >= 8L
  if (os == "windows") {
    id <- tryCatch(trimws(system("wmic bios get serialnumber", intern = TRUE, ignore.stderr = TRUE)[2]), error = function(e) NA_character_,
                   warning = function(w) NA_character_)
    if (ok(id) && !grepl("O\\.E\\.M|default string|^0+$", id, ignore.case = TRUE))
      return(list(id = id, source = "windows-bios", stable = TRUE))
  } else if (os == "macos") {
    id <- tryCatch({
      x <- system("ioreg -rd1 -c IOPlatformExpertDevice", intern = TRUE, ignore.stderr = TRUE)
      l <- grep("IOPlatformUUID", x, value = TRUE)[1]
      sub('.*"IOPlatformUUID"\\s*=\\s*"([^"]+)".*', "\\1", l)
    }, error = function(e) NA_character_, warning = function(w) NA_character_)
    if (ok(id)) return(list(id = id, source = "macos-ioplatformuuid", stable = TRUE))
  }
  if (os %in% c("unix", "macos", "unknown")) {
    for (f in os_files) {
      id <- cn_read_id_file(f)
      if (ok(id)) return(list(id = id, source = f, stable = TRUE))
    }
  }
  id <- cn_persisted_machine_id(config_dir)
  if (ok(id)) return(list(id = id, source = "persisted", stable = TRUE))
  list(id = cn_random_id(), source = "session", stable = FALSE)
}

# Explicit-status decryption. Never returns NULL silently.
# status: "ok" | "missing" | "decrypt_failed" | "mismatch"
cn_decrypt_privkey <- function(ciphertext, bios_id, expected = NULL) {
  if (is.null(ciphertext)) return(list(status = "missing", key = NULL, message = "No stored private key."))
  dec <- tryCatch(sodium::data_decrypt(ciphertext, key = sodium::sha256(charToRaw(bios_id))),
                  error = function(e) e)
  if (inherits(dec, "error"))
    return(list(status = "decrypt_failed", key = NULL,
                message = paste("Stored private key could not be decrypted with this machine's identifier:", conditionMessage(dec))))
  if (!is.null(expected) && !identical(dec, expected))
    return(list(status = "mismatch", key = NULL, message = "Decrypted private key does not match the stored key file."))
  list(status = "ok", key = dec, message = "")
}

cn_new_privkey <- function(bios_id) {
  charToRaw(openssl::sha512(openssl::base64_encode(cn_random_id()), key = bios_id))
}

# Copy (never move/delete) all key ciphertext to keys/unreadable-<stamp>/.
cn_backup_key_files <- function(keys_dir = "keys") {
  tryCatch({
    files <- list.files(keys_dir, pattern = "\\.(key|signed)$", full.names = TRUE)
    if (!length(files)) return(NA_character_)
    dest <- file.path(keys_dir, paste0("unreadable-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-", sample.int(1e6, 1)))
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    if (!all(file.copy(files, dest, copy.date = TRUE))) return(NA_character_)
    dest
  }, error = function(e) NA_character_)
}

# Initialise the local (sodium) private key.
# Returns list(status, privkey, privkey_final, privkey_dec, message, backup).
# status: "created" | "ok" | "regenerated" (stable id, old key unreadable: old
# files backed up, fresh key persisted; stored API keys must be re-entered) |
# "session_only" (unstable id: nothing on disk is touched, in-memory key only).
cn_init_privkey <- function(keys_dir = "keys", bios_id, id_stable = TRUE) {
  f_plain <- file.path(keys_dir, "private.key"); f_enc <- file.path(keys_dir, "private.key.signed")
  make_new <- function() {
    pk <- cn_new_privkey(bios_id)
    list(privkey = pk, privkey_final = sodium::data_encrypt(pk, key = sodium::sha256(charToRaw(bios_id))))
  }
  persist <- function(k) tryCatch({
    dir.create(keys_dir, showWarnings = FALSE, recursive = TRUE)
    saveRDS(k$privkey, file = f_plain); saveRDS(k$privkey_final, file = f_enc); TRUE
  }, error = function(e) FALSE)
  if (!file.exists(f_plain) || !file.exists(f_enc)) {
    k <- make_new()
    if (!id_stable) return(c(list(status = "session_only", privkey_dec = k$privkey, backup = NA_character_,
                                  message = "No stable machine identifier; API keys will only be kept for this session."), k))
    persist(k)
    return(c(list(status = "created", privkey_dec = k$privkey, backup = NA_character_, message = ""), k))
  }
  stored <- tryCatch(list(privkey = readRDS(f_plain), privkey_final = readRDS(f_enc)), error = function(e) NULL)
  res <- if (is.null(stored)) list(status = "decrypt_failed", key = NULL, message = "Stored key files are unreadable.")
         else cn_decrypt_privkey(stored$privkey_final, bios_id, expected = stored$privkey)
  if (identical(res$status, "ok"))
    return(c(list(status = "ok", privkey_dec = res$key, backup = NA_character_, message = ""), stored))
  k <- make_new()
  if (!id_stable)
    return(c(list(status = "session_only", privkey_dec = k$privkey, backup = NA_character_,
                  message = paste(res$message, "Stored keys were left untouched; please re-enter API keys for this session.")), k))
  bk <- cn_backup_key_files(keys_dir)
  if (is.na(bk))   # could not back up -> do not overwrite anything on disk
    return(c(list(status = "session_only", privkey_dec = k$privkey, backup = NA_character_,
                  message = paste(res$message, "Backup failed, so stored keys were left untouched; please re-enter API keys for this session.")), k))
  persist(k)
  c(list(status = "regenerated", privkey_dec = k$privkey, backup = bk,
         message = paste0(res$message, " Previous key files were preserved in '", bk, "'. Please re-enter your API keys.")), k)
}

# Decrypt a stored API key file. Returns list(status, value):
# "ok" | "missing" | "decrypt_failed".
cn_read_api_key <- function(path, privkey_dec) {
  if (!tryCatch(file.exists(path), error = function(e) FALSE)) return(list(status = "missing", value = NULL))
  tryCatch({
    if (is.null(privkey_dec)) stop("no private key available")
    dec <- sodium::data_decrypt(readRDS(path), key = openssl::sha256(privkey_dec))
    list(status = "ok", value = trimws(rawToChar(dec)))
  }, error = function(e) list(status = "decrypt_failed", value = NULL, message = conditionMessage(e)))
}
