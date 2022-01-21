#' @title Create the inverse of a Ghosh-matrix.
#' 
#' The inversion takes place after the basic properties of the Leontieff matrix. 
#' 
#' @param output_coefficients_matrix A technology coefficient matrix created
#' by the \code{\link{output_coefficient_matrix_create}}.
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @importFrom dplyr mutate across
#' @family analytic object functions
#' @examples 
#' om <- output_coefficient_matrix_create( 
#'   data_table = iotable_get(), 
#'   households = FALSE)
#' ghosh_inverse_create( output_coefficients_matrix = om )
#' @export 

ghosh_inverse_create <- function ( output_coefficients_matrix, 
                                  digits=NULL ) {

  ghosh_matrix <- leontieff_matrix_create( 
    # The ghosh matrix contains output_coefficients, not input coefficients
       technology_coefficients_matrix = output_coefficients_matrix 
       )
  
  Gm <- as.matrix(ghosh_matrix[,2:ncol(ghosh_matrix)])
  
  inverse <- solve(Gm)
  
  if ( sum(vapply(inverse,  function(x) sum(is.nan(x)), numeric(1))) > 0) {
    stop ("Error: Could not invert the Leontieff-matrix.")
  }
  
  named_inverse <- cbind(
        as.data.frame(ghosh_matrix[,1]),
        as.data.frame(inverse)
        ) %>%
    mutate(across(where(is.factor), as.character))
  
  names (named_inverse)     <- names (ghosh_matrix)
  row.names (named_inverse) <- seq_len(nrow(named_inverse))
  
 if ( is.null(digits) ) return (named_inverse)
  
  round_table ( named_inverse, digits = digits  )
}
