#' Symmetrically remove empty rows and columns
#' 
#' This is an internal function to determine where to separate quadrants if
#' necessary.
#' @param siot A symmetric input-output table, or a symmetric part of a
#' use table or a supply table.
#' @return A tibble/data.frame with a key row and a symmetric matix, 
#' after removing all empty columns and rows at the same time. 
#' @examples 
#' empty_remove( iotable_get ( source = "germany_1990") )

empty_remove <- function ( siot ) {
  
  ###Find non-zero cols and rows and remove them---- 
  non_zero_cols <- vapply ( siot[, 1:ncol(siot)], 
                            iotables:::non_zero_columns_find, logical (1) )
  non_zero_rows <- as.logical (non_zero_cols[-1] ) 
  remove_cols <- names (siot )[! non_zero_cols]
  
  siot_rows <- as.character ( unlist ( siot[,1]) )
  
  if ( length( remove_cols) > 0 ) {
    message ("Columns and rows of ", paste(remove_cols, collapse =', '), " are all zeros and will be removed.")
  }

  siot [! siot_rows %in% remove_cols , 
            ! names ( siot ) %in% remove_cols  ]
  
}