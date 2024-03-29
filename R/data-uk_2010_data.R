#' @title United Kingdom Input-Output Analytical Tables, 2010												
#'
#' @description Replication data exported from the Office of National Statistics.
#' 
#' @details You can retrieve the data with \code{\link{iotable_get}}, setting the 
#' \code{source} parameter as follows:
#' \describe{
##'  \item{\code{uk_2010_siot}}{ Input-Output table (domestic use, basic prices, product by product) }
##'  \item{\code{uk_2010_use}}{ Domestic use table at basic prices (product by industry) }
##'  \item{\code{uk_2010_imports}}{ Imports use table at basic prices (product by product) }
##'  \item{\code{uk_2010_coeff}}{ Matrix of coefficients (product by product) }
##'  \item{\code{uk_2010_inverse}}{ Leontief Inverse (product by product) }
##' }  
#' @usage data(uk_2010_data)
#' @source \href{https://webarchive.nationalarchives.gov.uk/20160114044923/https://www.ons.gov.uk/ons/rel/input-output/input-output-analytical-tables/2010/index.html}{United Kingdom Input-Output Analytical Tables 2010}
#' @format A data frame with 10 variables.
#'\describe{
#'   \item{uk_row}{The UK row identifier. Dots and '&' converted to '-'.} 
#'   \item{uk_row_lab}{The original UK row labels.}
#'   \item{uk_col}{The UK row identifier. Dots and '&' converted to '-'.} 
#'   \item{uk_col_lab}{The original UK column labels.}
#'   \item{geo}{Eurostat-style geocode, i.e. UK }
#'   \item{geo_lab}{United Kingdom}
#'   \item{indicator}{The name of the indicator, i.e. Excel sheet.}
#'   \item{unit}{Eurostat label equivalents units, i.e. MIO_NAC.}
#'   \item{unit_lab}{Eurostat label equivalents, i.e. millions of national currency unit.}
#'   \item{values}{The numeric values of the variable}
#'   \item{year}{Constant = 2010.}
#' }
#' @family Validation datasets

"uk_2010_data"