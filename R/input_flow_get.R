#' Create a use (input flow) matrix
#' 
#' Select the use table from a symmetric input-output table.
#' @param data_table A symmetric input-output table or use table retrieved by the  
#' \code{\link{iotable_get}} function. 
#' @param households Defaults to \code{FALSE}. If \code{TRUE}, the 
#' final household expenditure is added to the input flow table.
#' @param empty_remove Defaults to \code{TRUE}. If you want to keep empty 
#' primary input rows, choose \code{FALSE}. Empty product/industry rows are always 
#' removed to avoid division by zero error in the analytical functions.
#' @importFrom dplyr mutate_if left_join select
#' @examples 
#' data_table <- iotable_get()
#' input_flow <- input_flow_get( data_table = data_table, 
#'                          empty_remove = FALSE,
#'                          households = TRUE)
#' 
#' @export 

input_flow_get <- function ( data_table,
                             empty_remove = FALSE,
                             households = TRUE ) {  
  
  ##Initialize variables ------------
   . <-  NULL #non-standard evaluation creates a varning in build. 
  
  data_table <- dplyr::mutate_if (data_table, is.factor, as.character )
  
  #Remove empty columns and rows
  if ( empty_remove ) siot <- empty_remove ( data_table )
  
  last_column <- quadrant_separator_find ( data_table )
  
  ###Addding households, if requested----------------------------------------  
  if (households == TRUE) {
    household_column <- household_column_get( data_table )
    quadrant <- data_table [, 1:last_column]
    input_flow_table <- dplyr::left_join ( quadrant, household_column, 
                               by = names(quadrant)[1])
  }  else {
    input_flow_table <- dplyr::select ( data_table, 1:last_column)
  }
  
  key_column <- tolower(as.character(unlist(input_flow_table[,1])))
  
  last_row <- which(key_column %in% c("total", "cpa_total"))
  
  input_flow_table[1:last_row,]
  
}


