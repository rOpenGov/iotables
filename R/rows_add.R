#' @title Add conforming row(s)
#'
#' @description Add a conforming row, or elements of a conforming row to a names matrix.
#' 
#' @details If you want to add a single row manually, you can input a named numeric vector or a 
#' data frame with a single row. For multiple rows, input them as wide form data frame (see examples.)
#' 
#' @param data_table A symmetric input-output table, a use table, 
#' a margins or tax table retrieved by the  \code{\link{iotable_get}}
#' function. 
#' @param rows_to_add A data frame or a named numeric vector.
#' @param row_names An optional name or vector of names for the key column. Defaults to \code{NULL}.
#' @param empty_to_fill What should happen with missing column values? Defaults to \code{0}. If you want
#' to avoid division by zero, you may consider a very small value such as 0.000001.
#' @return An extended \code{data_table} with the new row(s) binded.
#' @family iotables processing functions
#' @importFrom dplyr bind_rows bind_cols
#' @examples 
#' rows_to_add <- data.frame(iotables_row      = "CO2_emission", 
#'                           agriculture_group =  10448, 
#'                           industry_group    =  558327, # -> construction is omitted
#'                           trade_group       =  11194)
#' 
#' rows_add (iotable_get(), rows_to_add = rows_to_add)
#' 
#' rows_add (iotable_get(), 
#'           rows_to_add = c(industry_group    =  1534, 
#'                           trade_group       =  4),
#'          row_names    = "CH4_emission" )
#' @export
 
rows_add <- function(data_table, rows_to_add, row_names = NULL, empty_fill = 0 ) {
  
  if ( is.numeric(rows_to_add) ) {
    rows_to_add <- as.data.frame(t(rows_to_add))
  }
  
  if ( is.null(row_names)) { 
    if( !is.numeric(rows_to_add[,1]) ) {
      key_column <- key_column_create(key_column_name = names(data_table)[1],
                                      key_column_values = unlist(rows_to_add[,1]))
    } else  {
      key_column <- key_column_create(key_column_name = names(data_table)[1], 
                                      key_column_values = paste0("new_row_", 1:nrow(rows_to_add))
                                      )
    }} else {
      if ( nrow(rows_to_add) != length(row_names) ) {
        stop( "The number of rows to add and the number of row_names do not match.")
      } else {
        key_column <- key_column_create(key_column_name = names(data_table)[1], 
                                        key_column_values = row_names )
      }
    }
  
  columns_required <- ifelse(is_key_column_present(data_table),
                             (ncol(data_table)-1), 
                             ncol(data_table))
  
  if(is_key_column_present(data_table)) {
    numeric_names <- names(data_table)[-1]
  } else {numeric_names <- names(data_table) }

  numeric_names
  
  empty_values <- as.data.frame(matrix(rep(empty_fill, columns_required*nrow(rows_to_add)), ncol = columns_required ))
  names(empty_values) <- numeric_names
  empty_values <- bind_cols (key_column, empty_values) 
  empty_values
  
  if ( !is.numeric(rows_to_add[,1])) {
    completed <- rows_to_add[,-1]
  } else {
    completed <- rows_to_add
  }

  to_complete_rows <- empty_values[,! names(empty_values) %in% names(completed)]
  
  
  data_table  %>%
    bind_rows(bind_cols(completed, to_complete_rows ) )
  
}
