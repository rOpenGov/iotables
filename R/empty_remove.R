#' Symmetrically remove empty rows and columns
#' 
#' This is an internal function to determine where to separate quadrants if
#' necessary.
#' @param data_table A symmetric input-output table, or a symmetric part of a
#' use table or a supply table.
#' @return A tibble/data.frame with a key row and a symmetric matrix, 
#' after removing all empty columns and rows at the same time. 
#' @keywords internal

empty_remove <- function ( data_table ) {
  
  ###Find non-zero cols and rows and remove them---- 
  non_zero_cols <- vapply ( data_table[, 1:ncol(data_table)], 
                            non_zero_columns_find, logical (1) )
  non_zero_rows <- as.logical (non_zero_cols[-1] ) 
  remove_cols <- names (data_table )[! non_zero_cols]
  
  siot_rows <- as.character ( unlist ( data_table[,1]) )
  
  if ( length( remove_cols) > 0 ) {
    message ("Columns and rows of ", paste(remove_cols, collapse =', '), " are all zeros and will be removed.")
  }

  data_table [! siot_rows %in% remove_cols , 
            ! names ( data_table ) %in% remove_cols  ]
  
}
