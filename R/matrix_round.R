#' @title Round all matrix values to required number of digits.
#'  
#' @param data_table A symmetric input output table, a use table or a supply 
#' table.
#' @param digits An integer number, defaults to \code{0}.
#' @return The matrix, with the intact key column and the numeric columns rounded.
#' @importFrom dplyr mutate across

matrix_round <- function(data_table, digits = 0) {
  
  data_table %>% dplyr::mutate ( across(where(is.numeric), round, digits) )

}
