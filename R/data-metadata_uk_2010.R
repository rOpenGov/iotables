#' Multipliers and effects (product) for testing
#' from the United Kingdom Input-Output Analytical Tables, 2010												
#'
#' The Excel-imported UK data.
#' @usage data(uk_2010_data)
#' @format A data frame with 10 variables.
#'\describe{
#'   \item{variable}{Constant for the iotable_get function.} 
#'   \item{uk_row}{The UK row identifier. Dots and '&' converted to '-'.} 
#'   \item{uk_col}{The UK row identifier. Dots and '&' converted to '-'.} 
#'   \item{uk_row_label}{The original UK row labels.}
#'   \item{uk_col_label}{The original UK column labels.}
#'   \item{eu_prod_na}{The Eurostat vocabulary equivalent of uk_row}
#'   \item{row_order}{Ordering variable for rows.}
#'   \item{col_order}{Ordering variable for columns.}
#'   \item{prod_na}{The Eurostat-like key values for rows.}
#'   \item{induse}{The Eurostat-like column names}
#' }
#' @family Metadata datasets
"metadata_uk_2010"