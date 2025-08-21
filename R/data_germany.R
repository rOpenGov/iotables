#' @title Simple input-output table for Germany, 1995
#'
#' @description
#' Replication dataset from the **Eurostat Manual**, Table 15.4:  
#' *Input-output table of domestic output at basic prices (Version A)*.
#'
#' @details
#' This dataset is used for testing and documentation purposes. It is a
#' well-documented example taken from the Eurostat Manual. The table has
#' been reformatted into the same long-form tidy structure as the Eurostat
#' database SIOTs.  
#'
#' The labels were slightly adjusted to reflect the transition from the
#' ESA95 vocabulary to ESA2010 since the publication of the Manual.
#'
#' @source
#' [Eurostat Manual of Supply, Use and Input-Output Tables, p. 482](
#' https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/
#' b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0)
#'
#' @format A data frame with 228 observations and 11 variables:
#' \describe{
#'   \item{prod_na}{Technology codes in row names, following Eurostat
#'     conventions.}
#'   \item{prod_na_lab}{Longer labels corresponding to `t_rows2`.}
#'   \item{induse}{Column labels, following Eurostat conventions with
#'     minor differences.}
#'   \item{iotables_row}{Row labels (to be used in key columns), using
#'     `iotables` abbreviations.}
#'   \item{iotables_col}{Column labels using `iotables` abbreviations.}
#'   \item{values}{Table values in million euros.}
#'   \item{unit}{Unit identifier. Always `"MIO_EUR"`.}
#'   \item{unit_lab}{Label for the unit: "million euros". Eurostat usually
#'     provides both euro and national currency values.}
#'   \item{geo}{Country code for Germany (`"DE"`).}
#'   \item{geo_lab}{Country name (`"Germany"`).}
#'   \item{time}{Reference year of the SIOT.}
#' }
#'
#' @family validation datasets
"germany_1995"

