#' @title Create output multipliers
#' 
#' @description Create a data frame of output multipliers.
#' 
#' @details Output multipliers as defined by the Eurostat Manual of Supply, 
#' Use and Input-Output Tables on p500.
#' 
#' @param input_coefficient_matrix  A Leontief inverse matrix created by the
#' \code{\link{input_coefficient_matrix_create}} function. 
#' @return A data frame with a key column and the output multipliers of the industries.
#' @examples                            
#' de_input_coeff <- input_coefficient_matrix_create( 
#'                           iotable_get(), 
#'                           digits = 4)
#'                            
#' output_multiplier_create (de_input_coeff)
#' @export 

output_multiplier_create <- function (input_coefficient_matrix) { 
  
  I <- leontief_inverse_create( input_coefficient_matrix )
  output_multipliers <- I[1, ]
  output_multipliers[, 2:ncol(output_multipliers)] <- colSums(I[, 2:ncol(input_coefficient_matrix)])
  output_multipliers[, 1] <- "output_multipliers"
  output_multipliers
  }