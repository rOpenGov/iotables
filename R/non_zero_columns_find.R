#' Find non-zero columns
#' 
#' This is an internal function to help finding empty columns and rows in 
#' symmetric tables.
#' @param data_table A symmetric input output table, a use table or a supply 
#' table.
#' @return A vector of \code{TRUE} and \code{FALSE} values for the table.
#' @examples 
#' non_zero_columns ( x = c(0,0,0))

non_zero_columns_find <- function(x) {
  if ( class ( x ) %in% c("factor", "character") ) return ( TRUE )
  ifelse (  all ( as.numeric ( unlist (x) ) == 0) , FALSE, TRUE )
}
