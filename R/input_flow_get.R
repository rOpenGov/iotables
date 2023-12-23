#' @title Create an inter-industry or input flow matrix
#' 
#' @description Select the first quadrant of the symmetric input-output table.
#' 
#' @details The first quadrant is called the input flow matrix, or the input requirements matrix, 
#' or the inter-industry matrix in different contexts.
#' @param data_table A symmetric input-output table or use table 
#' retrieved by the \code{\link{iotable_get}} function. 
#' @param households Defaults to \code{FALSE}. If \code{TRUE}, the 
#' final household expenditure is added to the input flow table.
#' @param empty_remove Defaults to \code{TRUE}. If you want to keep empty 
#' primary input rows, choose \code{FALSE}. Empty product/industry rows are always 
#' removed to avoid division by zero error in the analytic functions.
#' @importFrom dplyr mutate across left_join select where
#' @autoglobal
#' @return A data flow matrix (a symmetric use table) with a key column.
#' @family analytic object functions
#' @examples 
#' input_flow <- input_flow_get(data_table = iotable_get(), 
#'                              empty_remove = FALSE,
#'                              households = TRUE)
#' 
#' @export 

input_flow_get <- function ( data_table,
                             empty_remove = FALSE,
                             households = TRUE ) {  
  
  data_table <- mutate(data_table, across(where(is.factor), as.character))
  
  #Remove empty columns and rows
  if ( empty_remove ) siot <- empty_remove(data_table)
  
  last_column <- quadrant_separator_find(data_table)
  
  ## Adding households, if requested----------------------------------------  
  if (households == TRUE) {
    household_column <- household_column_get( data_table )
    quadrant <- data_table [, 1:last_column]
    input_flow_table <- left_join (
            quadrant, household_column, 
            by = names(quadrant)[1]
            )
  }  else {
    input_flow_table <- select (data_table, 1:last_column)
  }
  
  key_column <- tolower(as.character(unlist(input_flow_table[,1])))
  
  last_row <- which(key_column %in% c("total", "cpa_total"))
  
  input_flow_table[1:last_row,]
}


