#' Input-output table for Croatia, 2010.
#'
#' 1800 - Symmetric input-output table for domestic production (product x product)
#' In thousand kunas (T_NAC)
#' @source   \href{https://www.dzs.hr/Hrv_Eng/publication/2015/12-01-04_01_2015.xlsx}{Eurostat Manual of Supply, Use and Input-Output Tables} Updated 17 December 2015.
#' @usage data(croatia_2010_1800)
#' @format A data frame with 13 variables. 
#'\describe{
#'   \item{t_rows2}{Technology codes in row names, following the Eurostat convention.}
#'   \item{t_rows2_lab}{Longer labels for t_rows2}
#'   \item{values}{The actual values of the table in thousand kunas}  
#'   \item{t_cols2}{Column labels, following the Eurostat convention with differences. CPA_ suffix added to original DZS column names.}
#'   \item{t_cols2_lab}{Longer labels for t_cols2}
#'   \item{iotables_col}{The standardized iotables column labelling for easier reading.}
#'   \item{col_order}{The column ordering to keep the matrix legible.}
#'   \item{t_rows2_lab}{Longer labels for t_cols2}
#'   \item{iotables_row}{The standardized iotables row labelling for easier reading.}
#'   \item{row_order}{The row ordering to keep the matrix legible.}
#'   \item{unit}{Different from Eurostat tables, in thousand national currency units.}
#'   \item{geo}{ISO / Eurostat country code for Croatia}
#'   \item{geo_lab}{ISO / Eurostat country name, Croatia.}
#'   \item{time}{Date of the SIOT}  
#'   }                  
#'#' @keywords data, datasets, input-output table, Croatia

"croatia_2010_1800"