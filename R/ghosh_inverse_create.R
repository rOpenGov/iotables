#' @title Create the inverse of a Ghosh-matrix.
#' 
#' Create the Ghosh-inverse from the output coefficients.
#' 
#' @param output_coefficients_matrix A technology coefficient matrix created
#' by the \code{\link{output_coefficient_matrix_create}}.
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @importFrom dplyr mutate across
#' @family analytic object functions
#' @examples 
#' om <- output_coefficient_matrix_create( 
#'   data_table = iotable_get()
#'   )
#'   
#' ghosh_inverse_create( output_coefficients_matrix = om )
#' @export 

ghosh_inverse_create <- function ( output_coefficients_matrix, 
                                  digits=NULL ) {

  # The Ghosh-model is a dual pair of the Leontief-model, so we can use the same 
  # functions, but with different inputs (output coefficients instead of input coefficients.)
  
  ghosh_matrix <- leontief_matrix_create( 
       technology_coefficients_matrix = output_coefficients_matrix 
       )
  
  Gm <- as.matrix(ghosh_matrix[,2:ncol(ghosh_matrix)])
  
  inverse <- solve(Gm)
  
  if ( sum(vapply(inverse,  function(x) sum(is.nan(x)), numeric(1))) > 0) {
    stop ("Error: Could not invert the Ghosh-matrix.")
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
