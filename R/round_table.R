#' @title Systematically round values in a table
#' 
#' @description This is an internal function to do the rounding on all numeric elements 
#' of the table for comparability with external sources that use rounding.
#' @param data_table A symmetric input-output table, or a use table, or a 
#' supply table, or a margin or a tax table.
#' @param digits Number of digits for rounding.
#' @return A tibble with a key row and a symmetric matrix, 
#' after removing all empty columns and rows at the same time. 
#' @importFrom dplyr mutate across
#' @keywords internal

round_table <- function ( data_table, 
                          digits = NULL) {

  if (!is.null(digits)) { # Rounding digits must be numeric, if given
    if (! inherits(digits, "numeric") ) {
      warning ("Error: rounding digits are not given as a numeric input, 
               no rounding took place.") 
      return(data_table) }
  }
   
  round_eps <- function ( x, digits ) {
    ifelse (x == 1e-06, x, round (x, digits))
  }
  data_table %>%  mutate (across(where(is.numeric), round_eps, digits))
}