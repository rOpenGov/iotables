#' @title Multipliers and effects (product) for testing
#'
#' @description
#' A metadata dataset from the **United Kingdom Input-Output Analytical
#' Tables, 2010**. This version was imported from Excel and reformatted
#' for testing.
#'
#' @format A data frame with 10 variables:
#' \describe{
#'   \item{variable}{A constant used by `iotable_get()`.}
#'   \item{uk_row}{Row identifiers from the UK tables. Dots and `&`
#'     converted to `-`.}
#'   \item{uk_col}{Column identifiers from the UK tables. Dots and `&`
#'     converted to `-`.}
#'   \item{uk_row_label}{Original UK row labels.}
#'   \item{uk_col_label}{Original UK column labels.}
#'   \item{eu_prod_na}{Eurostat vocabulary equivalent of `uk_row`.}
#'   \item{row_order}{Ordering variable for rows.}
#'   \item{col_order}{Ordering variable for columns.}
#'   \item{prod_na}{Eurostat-like key values for rows.}
#'   \item{induse}{Eurostat-like key values for columns.}
#' }
#'
#' @family metadata datasets
"metadata_uk_2010"


