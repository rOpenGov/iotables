#' @title Input-output table for Croatia, 2010 (domestic production)
#'
#' @description
#' Symmetric input-output table (SIOT) for domestic production (product ×
#' product), code **1800**.  
#'  
#' Values are expressed in thousand kunas (T_NAC).
#'
#' @format A data frame with 13 variables:
#' \describe{
#'   \item{t_rows2}{Technology codes in row names, following Eurostat
#'     conventions.}
#'   \item{t_rows2_lab}{Longer labels for `t_rows2`.}
#'   \item{values}{Actual values of the table in thousand kunas.}
#'   \item{t_cols2}{Column labels, following Eurostat conventions. A `CPA_`
#'     suffix was added to original DZS column names.}
#'   \item{t_cols2_lab}{Longer labels for `t_cols2`.}
#'   \item{iotables_col}{Standardized `iotables` column labels for easier
#'     reading.}
#'   \item{col_order}{Column ordering to keep the matrix legible.}
#'   \item{iotables_row}{Standardized `iotables` row labels for easier reading.}
#'   \item{row_order}{Row ordering to keep the matrix legible.}
#'   \item{unit}{Different from Eurostat tables, in thousand national currency
#'     units.}
#'   \item{geo}{ISO/Eurostat country code for Croatia.}
#'   \item{geo_lab}{ISO/Eurostat country name, Croatia.}
#'   \item{time}{Date of the SIOT.}
#' }
#'
#' @source [Državni zavod za statistiku](https://dzs.gov.hr/)
#'
#' @family Croatia 2010 datasets
"croatia_2010_1800"