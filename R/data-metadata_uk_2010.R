#' Multipliers and Effects (Product) for Testing
#'
#' A reference dataset derived from the **United Kingdom Input–Output
#' Analytical Tables, 2010**. This version was imported from Excel and
#' reformatted for internal testing.
#'
#' @format A data frame with 10 variables:
#' \describe{
#'   \item{variable}{A constant used by [iotable_get()].}
#'   \item{uk_row}{Row identifiers from the UK tables. Dots and `&` were
#'     converted to `-`.}
#'   \item{uk_col}{Column identifiers from the UK tables. Dots and `&` were
#'     converted to `-`.}
#'   \item{uk_row_label}{Original UK row labels.}
#'   \item{uk_col_label}{Original UK column labels.}
#'   \item{eu_prod_na}{Eurostat vocabulary equivalent of `uk_row`.}
#'   \item{row_order}{Ordering key for rows.}
#'   \item{col_order}{Ordering key for columns.}
#'   \item{prod_na}{Eurostat-like key values for rows.}
#'   \item{induse}{Eurostat-like key values for columns.}
#' }
#'
#' @details
#' This dataset provides a mapping between the UK 2010 analytical input–output
#' tables and Eurostat-compatible codes, intended mainly for testing and
#' validation.
#'
#' @family metadata datasets
"metadata_uk_2010"
