#' @title Extract metadata from a downloaded IO table
#'
#' @description
#' Returns only the metadata (e.g. country, year, unit, flow) from a nested
#' input–output (IO) table created by [iotables_download()]. Removes the
#' heavy `data` list-column, leaving just metadata rows.
#'
#' @param dat Optional. A nested tibble as returned by [iotables_download()].
#'   If `NULL`, the function attempts to load it from `tempdir()` using the
#'   same cache naming convention (`"<source>_processed.rds"`).
#' @param source Character. A valid data source code (see Details).
#'
#' @return
#' A tibble with metadata columns only (no `data` list-column).
#'
#' @importFrom assertthat assert_that
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @export
iotables_metadata_get <- function(dat = NULL,
                                  source = "naio_10_cp1700") {
  validate_source(source)

  # Load cached file if dat not provided
  if (is.null(dat)) {
    cache_file <- file.path(tempdir(), paste0(source, "_processed.rds"))
    if (!file.exists(cache_file)) {
      message(glue(
        "No cached dataset found for source='{source}' in tempdir():\n{cache_file}\n",
        "Try running iotables_download(source = '{source}') first."
      ))
      return(invisible(NULL))
    }

    dat <- tryCatch(
      readRDS(cache_file),
      error = function(e) {
        message("Failed to read cached data: ", conditionMessage(e))
        NULL
      }
    )
  }

  # Validate structure
  if (is.null(dat) || !"data" %in% names(dat)) {
    message(glue(
      "No valid nested data found for source='{source}'.",
      " Did you run iotables_download() first?"
    ))
    return(invisible(NULL))
  }

  # Drop heavy list-column
  metadata <- dat[, setdiff(names(dat), "data"), drop = FALSE]

  # Return a clean tibble
  tibble::as_tibble(metadata)
}
