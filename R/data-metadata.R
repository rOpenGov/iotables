#' Metadata
#'
#' An arrangement of the Eurostat national accounts vocabulary, used to correctly
#' order wide format rows and columns from bulk long-form tables. 
#' @usage data(metadata)
#' @format A data frame with 8 variables.
#'\describe{
#'   \item{variable}{Eurostat vocabulary source, i.e. t_rows, t_cols, prod_na, induse}
#'   \item{group}{ Informal labelling for macroeconomic groups }
#'   \item{code}{Eurostat labels}
#'   \item{label}{Eurostat label descriptions}
#'   \item{quadrant}{Where to place the data from a long-form raw data file}  
#'   \item{account_group}{Different from Eurostat tables, in thousand national currency units.}
#'   \item{numeric_label}{ ordering from quadrant, account_group, digit_1, digit_2}     
#'   \item{iotables_label}{Custom, machine_readable snake format variable names}                     
#' }
#' @keywords data, datasets, input-output table, metadata, vocabulary

"metadata"