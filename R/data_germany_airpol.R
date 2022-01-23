#' @title Air Pollution Table for Germany, 1995.
#' 
#' @description Air pollution values for validation. 
#'
#' @details For testing purposes and cross-checking with the Eurostat manual.
#' The labels were slightly alterred to reflect the transition from the vocabulary
#' of ESA95 to ESA2010 since the publication of the Manual.
#' @source \href{https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0}{Eurostat Manual of Supply, Use and Input-Output Tables} p 482.
#' @usage data(germany_airpol)
#' @format A data frame with 72 observations and 4 variables.
#'\describe{
#'   \item{airpol}{The abbreviation of the air pollutant.}
#'   \item{induse}{Column labels, following the Eurostat convention with differences.}
#'   \item{iotables_col}{Column labels for iotables package abbreviations.}
#'   \item{value}{The actual values of the table in thousand tons.}  
#' }
#' @family Validation datasets
"germany_airpol"

