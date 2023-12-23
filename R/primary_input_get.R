#' @title Get primary inputs
#' @description This function will retrieve any primary input from the input-output 
#' table. 
#' @param data_table A symmetric input-output table, a use table, or a 
#' supply table retrieved by the  \code{\link{iotable_get}} function. 
#' @param primary_input The primary input to be returned from the table.
#' @importFrom dplyr select mutate across where
#' @return A data frame with the vector of multipliers and the an 
#' auxiliary metadata column (for joining with other matrixes.)
#' @family iotables processing functions
#' @autoglobal
#' @examples
#' comp_employees_de <- primary_input_get(
#'   data_table = iotable_get(), 
#'   primary_input = "compensation_employees") 
#' @export

primary_input_get <- function ( data_table,
                                primary_input = "compensation_employees") {
  
  if ( is.null(data_table)) { 
    stop ( "No input-output table was given as an input")
    }
  
  last_column <- quadrant_separator_find ( data_table )
  
  data_table <- data_table %>% 
    mutate(across(where(is.factor), as.character)) %>%
    select(1:last_column)
  
  if ( primary_input %in% data_table[[1]] ) {
    input_row <- which ( data_table[[1]] == primary_input )
  } else {
    stop("The input is not found in this data source.")
  }

  data_table[input_row, ]
}


