#' Get an output vector
#' 
#' This is a wrapper function around the \code{\link{primary_input_get}} 
#' function. 
#' @param data_table A symmetric input-output table or use table retrieved by the  
#' \code{\link{iotable_get}} function. 
#' @examples 
#' output_get ( data_table = iotable_get () )
#' @export 

output_get <- function ( data_table ) {  
  
  key_column <- as.character(unlist(data_table[,1]))
  
  possible_names <- c('output', 'output_bp', 'p1', 'P1')
  
  primary_input_get ( data_table = data_table,
                      primary_input = possible_names[
                        possible_names %in% key_column]
  )
}


