#' Input-output table for Croatia, 2010.
#'
#' 1700 - Symmetric input-output table at basic prices (product x product)
#' In thousand kunas (T_NAC)
#' @source   \href{https://dzs.gov.hr/}{Državni zavod za statistiku}.
#' @usage data(croatia_2010_1700)
#' @format A data frame with 13 variables. 
#'\describe{
#'   \item{t_rows2}{Technology codes in row names, following the Eurostat convention.}
#'   \item{t_rows2_lab}{Longer labels for t_rows2}
#'   \item{t_cols2}{Technology codes in column names, following the Eurostat convention.}
#'   \item{t_cols2_lab}{Longer labels for t_cols2}
#'   \item{iotables_col}{The standardized iotables column labelling for easier reading.}
#'   \item{col_order}{The column ordering to keep the matrix legible.}
#'   \item{row_order}{The row ordering to keep the matrix legible.}
#'   \item{iotables_row}{The standardized iotables row labelling for easier reading.}
#'   \item{unit}{Different from Eurostat tables, in thousand national currency units.}
#'   \item{geo}{ISO / Eurostat country code for Croatia}
#'   \item{geo_lab}{ISO / Eurostat country name, Croatia.}
#'   \item{time}{Date of the SIOT}                    
#'   \item{values}{The actual values of the table in thousand kunas}  
#' }
#' @family Croatia 2010 datasets
"croatia_2010_1700"