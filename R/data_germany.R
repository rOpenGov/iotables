#' Germany 1995 input–output table (domestic, basic prices; Version A)
#'
#' @description
#' Replication of Table 15.4 in the Eurostat manual: *Input–output table
#' of domestic output at basic prices (Version A)*. The table is
#' reformatted into the same long, tidy structure used by Eurostat's
#' warehouse SIOTs so it can be used interchangeably with real downloads
#' in examples, vignettes and unit tests.
#'
#' @details
#' This dataset underpins package examples and tests that verify analytical
#' results against the manual’s published numbers. Variable names follow
#' the Eurostat convention used for symmetric product × product tables:
#' rows are coded in `prod_na`, columns in `induse`, with a stock/flow
#' flag in `stk_flow` identifying domestic use.
#'
#' Terminology in the manual predates ESA 2010. For compatibility with
#' current Eurostat series, a companion dataset
#' [`germany_1995_2010`][germany_1995_2010] provides the same table
#' using ESA 2010 labels/codes (e.g. `D21X31` instead of older forms).
#'
#' @format
#' A data frame with the following columns:
#' \describe{
#'   \item{prod_na}{Row codes (products; CPA aggregates) as in Eurostat
#'     SIOTs.}
#'   \item{induse}{Column codes (products; CPA aggregates) as in Eurostat
#'     SIOTs.}
#'   \item{values}{Cell values, million euro. Numeric.}
#'   \item{stk_flow}{Stock/flow flag. Always `"DOM"` for domestic use.}
#'   \item{unit}{Measurement unit. Always `"MIO_EUR"`.}
#'   \item{unit_lab}{Unit label, e.g. `"Million euro"`.}
#'   \item{geo}{Country code, `"DE"`.}
#'   \item{geo_lab}{Country name, `"Germany"`.}
#'   \item{time}{Reference date (e.g. `"1995-01-01"`).}
#' }
#'
#' Row/column *labels* used by printing helpers can be joined at runtime
#' via the package metadata (see `?metadata`), producing `prod_na_lab`,
#' `induse_lab`, `iotables_row`, and `iotables_col` as needed.
#'
#' @source
#' Eurostat (2008). *Eurostat Manual of Supply, Use and Input–Output
#' Tables* (2008 edition), Table 15.4.
#'
#' @references
#' Eurostat (2008). *Eurostat Manual of Supply, Use and Input–Output
#' Tables*. Luxembourg: Office for Official Publications of the
#' European Communities.
#'
#' United Nations (2018). *Handbook on Supply and Use Tables and
#' Input–Output Tables with Extensions and Applications (White cover
#' version)*. New York: UN Statistics Division.
#'
#' @seealso
#' [germany_1995_2010], [iotable_get()], [iotables_download()],
#' [metadata]
#'
#' @examples
#' data(germany_1995)
#' head(germany_1995)
#'
#' # Use the example as if it were a Eurostat download:
#' x <- iotable_get(source = "germany_1995",
#'                  geo = "DE", year = 1995,
#'                  unit = "MIO_EUR",
#'                  stk_flow = "DOM",
#'                  labelling = "iotables")
"germany_1995"

