#' @title Symmetrically remove empty rows and columns
#' 
#' @description Symmetrically remove columns with only zero values or with missing values.
#' 
#' @param data_table A symmetric input-output table, or a symmetric part of a
#' use table or a supply table.
#' @return A tibble/data.frame with a key row and a symmetric matrix, 
#' after removing all empty columns and rows at the same time. 
#' @examples 
#' test_table <- input_coefficient_matrix_create(iotable_get(source = "germany_1990"))
#' test_table[, 2] <- 0
#' empty_remove (test_table)
#' @export

empty_remove <- function ( data_table ) {
  
  ###Find non-zero cols and rows and remove them---- 
  non_zero_cols <- vapply ( data_table[, seq_len(ncol(data_table))], 
                            non_zero_columns_find, logical (1) )
  non_zero_rows <- as.logical (non_zero_cols[-1] ) 
  
  remove_cols <- names (data_table)[which(is.na(non_zero_cols) | non_zero_cols == FALSE)]
  
  siot_rows <- as.character ( unlist ( data_table[,1]) )
  
  if ( length(remove_cols) > 0 ) {
    message ("Columns and rows of ", paste(remove_cols, collapse =', '), " are all zeros and will be removed.")
  }
  
  data_table [! siot_rows %in% remove_cols , 
              ! names ( data_table ) %in% remove_cols  ]
}



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