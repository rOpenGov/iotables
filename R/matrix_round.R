#' @title Round all matrix values to required number of digits.
#' 
#' @description For comparison with results created with other software or published with rounding,
#' systematically round the values of an input-output table, a use, supply, tax or margins table. 
#'  
#' @param data_table A symmetric input output table, a use, supply, tax or margins table.
#' @param digits An integer number, defaults to \code{0}.
#' @return The matrix, with the intact key column and the numeric columns rounded.
#' @family iotables processing functions
#' @importFrom dplyr mutate across

matrix_round <- function(data_table, digits = 0) {
  
  data_table %>% mutate ( across(where(is.numeric), round, digits) )

}
