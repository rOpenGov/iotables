#' Create a Leontieff matrix
#' 
#' Create a Leontieff matrix from technology matrix after some basic error 
#' handling. Most likely you will need this function as a step to invoke the 
#' function to create its inverse: \code{\link{leontieff_inverse_create}}.
#' @param technology_coefficients_matrix A technology coefficient matrix created by the \code{\link{input_coefficient_matrix_create}}.
#' @importFrom dplyr mutate_at mutate_if
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
#' @export 

leontieff_matrix_create <- function ( technology_coefficients_matrix ) { 
  . = NULL ; funs = NULL ; vars = NULL
  
  Tm <- as.matrix (technology_coefficients_matrix[,2:ncol(technology_coefficients_matrix )])
  
  if ( nrow(Tm) != ncol(Tm)) stop("Error: the input matrix is not symmetric.")
  
  
   IminusA <- diag( nrow(Tm) ) - Tm
  
  if ( sum(sapply(IminusA, function(x) sum(is.nan(x)))) > 0) {
    warning ("Warning: There are invalid elements in the Leontieff-matrix.")
  }
 
 Leontieff <- cbind(as.data.frame(technology_coefficients_matrix [,1]),
                    as.data.frame(IminusA))

 names ( Leontieff)[1] <- names (technology_coefficients_matrix)[1]
 Leontieff[,1] <- as.character(Leontieff[,1])
 return (Leontieff)
}
