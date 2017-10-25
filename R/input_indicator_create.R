#' Create input indicator(s)
#' 
#' The function creates the input indicators from the inputs and the outputs.
#' @param input_matrix A named (primary) input(s) vector or matrix created by \code{\link{primary_input_get}}
#' @param output_vector A named output vector created by \code{\link{output_get}}.  
#' @param digits Rounding digits, if omitted, no rounding takes place.  
#' @examples  
#' de_output <- output_get ( source = "germany_1990", geo = "DE",
#'                           year = 1990, unit = "MIO_EUR", 
#'                           households = FALSE, labelling = "iotables")
#' 
#' de_emp <- primary_input_get ( input = "emp",
#'                               source = "germany_1990", geo = "DE",
#'                               year = 1990, unit = "MIO_EUR", 
#'                               households = FALSE, labelling = "iotables")
#' @export 

input_indicator_create <- function ( input_matrix, output_vector,
                                     digits = NULL ) { 
  if (! is.null(digits)) {
    if (digits<0) digits <- NULL
  }
  if (! all.equal(names(input_matrix), names(output_vector))) {
    stop ( "Non-conforming inputs and outputs are given, or the column names are not matching.")
  }
  
  #Not elegant
  for ( j in 2:ncol(input_matrix)) {
    if ( as.numeric(output_vector[j])==0) {
      output_vector[j] <- 0.000001  #avoid division by zero 
      warning ("Warning: Zero output is changed to 0.000001.")
      }
    if (is.null(digits)) {
      input_matrix[,j] <- input_matrix[,j] / as.numeric(output_vector[j])  #tibble to numeric conversion
    } else {
      input_matrix[,j] <- round(input_matrix[,j] / as.numeric(output_vector[j]), digits = digits )
    }
   
  }
  input_matrix[,1] <- as.character (input_matrix[,1])
  input_matrix[1,1] <- paste0(as.character(input_matrix[1,1]), "_indicator")
  
  return(input_matrix)
}
