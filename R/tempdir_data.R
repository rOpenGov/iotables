#' @title Deprecated: Retrieve or download Eurostat dataset from temporary cache
#'
#' @description
#' ❌ **Deprecated** — This internal helper is retained only for backward
#' compatibility. It has been replaced by unified cache management inside
#' [iotables_download()] and [airpol_get()].
#'
#' The new caching system uses a session-local Eurostat cache directory
#' (`file.path(tempdir(), "eurostat")`) with persistent `.rds` storage for
#' downloaded tables.
#'
#' @param id Character. The Eurostat dataset identifier (e.g., `"naio_10_cp1700"`).
#' @param force_download Logical. If `TRUE`, forces a re-download and ignores
#' cached copies. Defaults to `FALSE`.
#'
#' @return A raw Eurostat data frame.
#' @keywords internal
#' @seealso [iotables_download()], [airpol_get()]
#' @importFrom eurostat get_eurostat clean_eurostat_cache
#' @export
tempdir_data <- function(id, force_download = FALSE) {
  deprecate_warn(
    # see utils.R
    old = "tempdir_data",
    new = "iotables_download",
    version = "0.9.5",
    details = "Use iotables_download() instead, which now manages Eurostat cache directly."
  )
  
  # Legacy behaviour
  tmpdir <- tempdir()
  processed_file <- file.path(tmpdir, paste0(id, "_processed.rds"))
  safe_read <- function(path) tryCatch(readRDS(path), error = function(e) NULL)
  
  if (!force_download && file.exists(processed_file)) {
    if (interactive()) message("Using cached processed file in tempdir().")
    data <- safe_read(processed_file)
    if (!is.null(data)) return(data)
  }
  
  cache_dir <- file.path(tmpdir, "eurostat")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  eurostat_files <- list.files(cache_dir, pattern = id, full.names = TRUE)
  
  if (!force_download && length(eurostat_files) >= 1) {
    if (interactive()) message("Using cached Eurostat file from ", cache_dir)
    data <- safe_read(eurostat_files[[1L]])
    if (!is.null(data)) return(data)
  }
  
  if (interactive()) {
    message("Downloading Eurostat dataset ", id,
            " (force_download = ", force_download, ").")
  }
  
  if (force_download) {
    unlink(processed_file, force = TRUE)
    if (length(eurostat_files)) unlink(eurostat_files, force = TRUE)
    suppressWarnings(eurostat::clean_eurostat_cache(cache_dir = cache_dir))
  }
  
  downloaded <- tryCatch(
    eurostat::get_eurostat(id, cache = !force_download, cache_dir = cache_dir),
    error = function(e) {
      message("Eurostat download failed for ", id, ": ", conditionMessage(e))
      NULL
    }
  )
  
  if (is.null(downloaded) || !is.data.frame(downloaded)) {
    stop("Download of ", id, " failed. Check Eurostat availability or identifier.")
  }
  
  downloaded
}

