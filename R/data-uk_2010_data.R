#' United Kingdom Input–Output Analytical Tables, 2010
#'
#' @description
#' Official **Input–Output Analytical Tables** for the United Kingdom (2010),
#' published by the Office for National Statistics (ONS).
#'
#' These replication data are harmonized to the *iotables* package format
#' and can be accessed via [iotable_get()] by setting the `source` parameter
#' to one of the available tables.
#'
#' @details
#' The dataset includes five analytical table types from the 2010 UK Input–Output release:
#'
#' - `uk_2010_siot` — Input–Output table (domestic use, basic prices, product × product)
#' - `uk_2010_use` — Domestic use table at basic prices (product × industry)
#' - `uk_2010_imports` — Imports use table at basic prices (product × product)
#' - `uk_2010_coeff` — Matrix of coefficients (product × product)
#' - `uk_2010_inverse` — Leontief inverse (product × product)
#'
#' These correspond to the Excel sheets from the official ONS publication.
#'
#' @usage
#' data(uk_2010_data)
#'
#' @format
#' A [tibble][tibble::tibble] with **10 variables**:
#'
#' \describe{
#'   \item{uk_row}{Row identifier (dots and `&` converted to `-`).}
#'   \item{uk_row_lab}{Original ONS row label.}
#'   \item{uk_col}{Column identifier (dots and `&` converted to `-`).}
#'   \item{uk_col_lab}{Original ONS column label.}
#'   \item{geo}{Eurostat-style geocode (constant: `"UK"`).}
#'   \item{geo_lab}{Human-readable country label (constant: `"United Kingdom"`).}
#'   \item{indicator}{Indicator or table name, matching Excel sheet title.}
#'   \item{unit}{Eurostat-style unit code (e.g. `"MIO_NAC"`).}
#'   \item{unit_lab}{Human-readable unit label (e.g. `"Million national currency"`).}
#'   \item{values}{Numeric values for each indicator.}
#'   \item{year}{Constant value: `2010`.}
#' }
#'
#' @source
#' Office for National Statistics (2010). *United Kingdom Input–Output Analytical Tables, 2010.*
#' Retrieved from the [UK National Archives (ONS Web Archive)](https://webarchive.nationalarchives.gov.uk/20160114044923/https://www.ons.gov.uk/ons/rel/input-output/input-output-analytical-tables/2010/index.html).
#'
#' @seealso
#' - [iotable_get()] for accessing and filtering IO tables.
#' - [iotables_download()] for downloading current Eurostat and UK datasets.
#'
#' @family validation datasets
#' @keywords datasets
#'
"uk_2010_data"
