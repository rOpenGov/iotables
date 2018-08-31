#' Simple input-output table for Germany, 1990.
#'
#' For testing purposes a well documented example data set is used from the Eurostat manual.
#' The table in the Eurostat manual is brought to the format used by the Eurostat database.
#' It is a small dataset for examples, but it is also instructive to understand how Eurostat
#' stores the highly structured SIOTs in long-form tidy datasets. 
#' The third and fourth quadrant labelling follows the current Eurostat labels.
#' @source \href{http://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0}{Eurostat Manual of Supply, Use and Input-Output Tables} p 492
#' @usage data(germany_1990)
#' @format A data frame with 228 observations and 10 variables.
#'\describe{
#'   \item{t_rows2}{Technology codes in row names, following the Eurostat convention.}
#'   \item{t_rows2_lab}{Longer labels for t_rows2}
#'   \item{t_cols2}{Column labels, following the Eurostat convention with differences.}
#'   \item{t_cols2_lab}{Longer labels for t_cols2}
#'   \item{values}{The actual values of the table in million euros}  
#'   \item{unit}{MIO_EUR, the same as Eurostat}
#'   \item{unit_lab}{Million euros. Eurostat usually has euro and national currency unit values, too.}
#'   \item{geo}{ISO / Eurostat country code for Germany, i.e. DE}
#'   \item{geo_lab}{ISO / Eurostat country name, Germany}
#'   \item{time}{Date of the SIOT}     
#' }
#' #' @keywords data, datasets, input-output table, Germany
"germany_1990"

