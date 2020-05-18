#' Find Non-zero Columns
#' 
#' This is an internal function to help finding empty columns and rows in 
#' symmetric tables.
#' @param data_table A symmetric input output table, a use table or a supply 
#' table.
#' @return A vector of \code{TRUE} and \code{FALSE} values for the table.
#' @keywords internal

non_zero_columns_find <- function(data_table) {
  if ( class ( data_table ) %in% c("factor", "character") ) return ( TRUE )
  ifelse (  all ( as.numeric ( unlist (data_table) ) == 0) , FALSE, TRUE )
}
