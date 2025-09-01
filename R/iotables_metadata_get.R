#' Extract metadata from a downloaded IO table
#'
#' @description
#' Return only the metadata information from a nested input–output (IO) table
#' (or related table) created by [iotables_download()]. The `data` list-column
#' is removed, leaving only metadata rows.
#'
#' @details
#' If `dat` is `NULL`, the function tries to load the file corresponding to
#' `source` from the current session's `tempdir()`.
#'
#' @section Sources:
#' Supported Eurostat/ONS products include:
#'
#' - `"naio_10_cp1700"` — Symmetric IO table, basic prices (product × product)
#' - `"naio_10_pyp1700"` — Symmetric IO table, basic prices (product × product), previous years’ prices
#' - `"naio_10_cp1750"` — Symmetric IO table, basic prices (industry × industry)
#' - `"naio_10_pyp1750"` — Symmetric IO table, basic prices (industry × industry), previous years’ prices
#' - `"naio_10_cp15"` — Supply table at basic prices incl. margins/taxes
#' - `"naio_10_cp16"` — Use table at purchasers’ prices
#' - `"naio_10_cp1610"` — Use table at basic prices
#' - `"naio_10_pyp1610"` — Use table at basic prices (previous years’ prices)
#' - `"naio_10_cp1620"` / `"naio_10_pyp1620"` — Trade & transport margins
#' - `"naio_10_cp1630"` / `"naio_10_pyp1630"` — Taxes less subsidies on products
#' - `"uk_2010_siot"` — United Kingdom IO Analytical Tables
#'
#' @param dat A nested tibble created by [iotables_download()]. Defaults to
#'   `NULL`, in which case the function attempts to read the file from
#'   `tempdir()`.
#' @param source Character. A valid data source code (see **Sources**).
#'
#' @return
#' A tibble with only metadata columns. The `data` list-column is removed
#' and unnested.
#'
#' @importFrom tidyr unnest
#' @family import functions
#'
#' @examples
#' \donttest{
#' # Download data into tempdir()
#' iotables_download(source = "naio_10_pyp1750")
#'
#' # Extract metadata only
#' iotables_metadata_get(source = "naio_10_pyp1750")
#' }
#'
#' @export
iotables_metadata_get <- function(dat = NULL,
                                  source = "naio_10_cp1700") {
  if (is.null(dat)) {
    validate_source(source)
    dat <- iotables_read_tempdir(source)
  }

  if (!is.null(dat)) {
    metadata <- dat[, !names(dat) %in% c("data")]
    tidyr::unnest(metadata, cols = c())
  } else {
    message(
      "The temporary file for source='", source,
      "' is not found in\ntempdir='",
      tempdir(), "'"
    )
  }
}
