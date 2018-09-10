#' Output multipliers
#' 
#' Output multipliers as defined by the Eurostat Manual of Supply, 
#' Use and Input-Output Tables on p500.
#' @param input_coefficient_matrix  A Leontieff inverse matrix created by the
#' \code{\link{input_coefficient_matrix_create}} function. 
#' @examples 
#' de_use    <- use_table_get ( source = "germany_1990", geo = "DE",
#'                              year = 1990, unit = "MIO_EUR", 
#'                              households = FALSE, labelling = "iotables")
#' 
#' de_output <- output_get ( source = "germany_1990", geo = "DE",
#'                           year = 1990, unit = "MIO_EUR",
#'                           households = FALSE, labelling = "iotables")
#'                           
#' de_input_coeff <- input_coefficient_matrix_create( 
#'                            de_use, de_output, digits = 4)
#'                            
#' output_multiplier_create ( de_input_coeff )
#' @export 

output_multiplier_create <- function ( input_coefficient_matrix ) { 
  L = leontieff_matrix_create( input_coefficient_matrix )
  I = leontieff_inverse_create( L )
  output_multipliers <- I[1, ]
  output_multipliers[, 2:ncol(output_multipliers)] <- colSums(I[, 2:ncol(input_coefficient_matrix)])
  output_multipliers[, 1] <- "output_multipliers"
  output_multipliers
  }