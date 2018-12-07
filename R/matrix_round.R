#' Round all matrix values to required number of digits.
#'  
#' @param data_table A symmetric input output table, a use table or a supply 
#' table.
#' @param digits An integer number, defaults to \code{0}.
#' @return The matrix, with the intact key column and the numeric columns rounded.
#' @importFrom dplyr mutate_if
#' @examples 
#' matrix_round( iotable_get ( source = 'germany_1990') )

matrix_round <- function(data_table, digits = 0) {
  
  data_table %>% dplyr::mutate_if ( is.numeric, round, digits )

}
