#' Create the inverse of a Leontieff-matrix.
#' 
#' The inversion takes place after the basic properties of the Leontieff matrix. 
#' @param leontieff_matrix A Leontieff matrix created by the \code{\link{leontieff_matrix_create}} function. 
#' @importFrom dplyr mutate_at
#' @examples 
#' de_use <- use_table_get ( source = "germany_1990", geo = "DE",
#'                year = 1990, unit = "MIO_EUR", 
#'                households = FALSE, labelling = "iotables")
#' 
#' de_output <- output_get ( source = "germany_1990", geo = "DE",
#'                year = 1990, unit = "MIO_EUR",
#'                households = FALSE, labelling = "iotables")
#' 
#' de_coeff <- input_coefficient_matrix_create( de_use, de_output, digits = 4)
#' 
#' L <- iotables::leontieff_matrix_create( technology_coefficients_matrix = de_coeff )
#' I <- leontieff_inverse_create (L)
#' @export 

leontieff_inverse_create <- function ( leontieff_matrix ) {
  . = NULL ; funs = NULL ; vars = NULL
  
  Lm <- as.matrix(leontieff_matrix[,2:ncol(leontieff_matrix)])
  
  inverse <- solve( Lm )

  if ( sum(sapply(inverse, function(x) sum(is.nan(x)))) > 0) {
    stop ("Error: Could not invert the Leontieff-matrix.")
  }
  
  named_inverse <- cbind(as.data.frame(leontieff_matrix [,1]),
                     as.data.frame(inverse))
  names ( named_inverse ) <- names (leontieff_matrix)
  row.names ( named_inverse ) <- 1:nrow(named_inverse)
  return(named_inverse)
}
