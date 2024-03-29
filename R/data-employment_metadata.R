#' @title Employment metadata
#'
#' @description An arrangement of the Eurostat national accounts vocabulary to match with 
#' employment statistics data. 
#' @usage data(employment_metadata)
#' @format A data frame with 6 variables.  
#'\describe{
#'   \item{emp_code}{code used in the employment statistics}
#'   \item{code}{Eurostat labels for SIOTs corresponding to emp_code}
#'   \item{label}{Eurostat label descriptions for SIOTs corresponding to emp_code}
#'   \item{variable}{Eurostat vocabulary source, i.e. t_rows, t_cols, prod_na, induse}
#'   \item{group}{Different from Eurostat tables, in thousand national currency units.}
#'   \item{iotables_label}{Custom, machine_readable snake format variable names}                     
#' }
#' @family Metadata datasets

"employment_metadata"