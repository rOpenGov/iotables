#' @title Air pollution table for Germany, 1995
#'
#' @description
#' Air pollution values for validation and cross-checking with the Eurostat
#' Manual.
#'
#' @details
#' This dataset is provided for testing purposes. Labels were slightly adjusted
#' to reflect the transition from ESA95 to ESA2010 vocabulary since the
#' publication of the Eurostat Manual.
#'
#' @source
#' Eurostat (2008). *Eurostat Manual of Supply, Use and Input–Output Tables*,
#' p. 482.  
#' [PDF](https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/
#' b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0)
#'
#' @format A data frame with 72 observations and 4 variables:
#' \describe{
#'   \item{airpol}{Abbreviation of the air pollutant.}
#'   \item{induse}{Column labels, following Eurostat conventions with minor
#'     differences.}
#'   \item{iotables_col}{Column labels using `iotables` abbreviations.}
#'   \item{value}{Values in thousand tons.}
#' }
#'
#' @family validation datasets

"germany_airpol"
