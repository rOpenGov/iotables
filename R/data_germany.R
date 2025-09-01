#' Germany 1995 symmetric input–output table (ESA 2010 codes)
#'
#' @description
#' Reproduction of Table 15.4 in the Eurostat Manual
#' (*Input–output table of domestic output at basic prices, Version A*).
#' This is a small, well-documented benchmark dataset that accompanies the
#' \pkg{iotables} package. It is reformatted into the same long structure
#' as Eurostat warehouse SIOTs, so that functions and tests can work
#' identically on this example and on real Eurostat downloads.
#'
#' @details
#' The values come from Beutel (2008), *Eurostat Manual of Supply, Use and
#' Input–Output Tables*, Table 15.4. Labels and codes follow ESA 2010
#' conventions (e.g. \code{CPA_A}, \code{CPA_B-E}, \code{P3_S14}),
#' allowing direct comparison with modern Eurostat releases.
#'
#' This dataset underpins many unit tests in \pkg{iotables}: multipliers,
#' coefficients, and linkage indices are validated against the published
#' benchmark. Because it is small (247 rows), it is also used in vignettes
#' and examples to demonstrate workflows.
#'
#' @format A data frame with 247 rows and 11 columns:
#' \describe{
#'   \item{prod_na}{Row code (ESA 2010 / CPA aggregate).}
#'   \item{prod_na_lab}{Row label, long description.}
#'   \item{iotables_row}{Row identifier used internally.}
#'   \item{iotables_col}{Column identifier (factor with 13 levels).}
#'   \item{values}{Cell value, in millions of euros (integer).}
#'   \item{induse}{Column code (ESA 2010 / CPA aggregate or national accounts
#'     item).}
#'   \item{geo}{Country code, always \code{"DE"}.}
#'   \item{geo_lab}{Country name, \code{"Germany"}.}
#'   \item{time}{Reference year, as a Date (\code{"1995-01-01"}).}
#'   \item{unit}{Unit code, \code{"MIO_EUR"}.}
#'   \item{unit_lab}{Unit label, \code{"Million euro"}.}
#' }
#'
#' @source
#' Beutel, J. (2008). *Eurostat Manual of Supply, Use and Input–Output
#' Tables*, Table 15.4. Luxembourg: Office for Official Publications of the
#' European Communities.
#'
#' @seealso
#' [iotable_get()] for extracting comparable tables from Eurostat.
#'
#' @examples
#' data(germany_1995)
#' head(germany_1995)
#' # Verify against the Eurostat manual:
#' subset(germany_1995, prod_na == "CPA_A" & iotables_col == "agriculture_group")
"germany_1995"
