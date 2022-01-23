#' @title Create the inverse of a Leontief-matrix
#' 
#' @description Create the Leontief inverse from the technology coefficient matrix.
#' @details  The Leontief-inverse is \deqn{L = (I-A)^-1}
#' where B is the input coefficient matrix
#' created by \code{\link{input_coefficient_matrix_create}}. For the similar inverse 
#' created from output coefficients, see the 
#' \code{\link{ghosh_inverse_create}} function. 
#' 
#' @param technology_coefficients_matrix A technology coefficient matrix created
#' by the \code{\link{input_coefficient_matrix_create}}.
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @importFrom dplyr mutate across
#' @family analytic object functions
#' @examples 
#' tm <- input_flow_get ( 
#'   data_table = iotable_get(), 
#'   households = FALSE)
#' I <- leontief_inverse_create( technology_coefficients_matrix = tm )
#' @export 

leontief_inverse_create <- function ( technology_coefficients_matrix, 
                                      digits=NULL ) {

  leontief_matrix<- leontief_matrix_create( 
       technology_coefficients_matrix = technology_coefficients_matrix 
       )
  
  Lm <- as.matrix(leontief_matrix[,2:ncol(leontief_matrix)])
  
  inverse <- solve( Lm )
  
  if ( sum(vapply(inverse,  function(x) sum(is.nan(x)), numeric(1))) > 0) {
    stop ("Error: Could not invert the Leontief-matrix.")
  }
  
  named_inverse <- cbind(
        as.data.frame(leontief_matrix [,1]),
        as.data.frame(inverse)
        ) %>%
    mutate(across(where(is.factor), as.character))
  
  names (named_inverse)     <- names (leontief_matrix)
  row.names (named_inverse) <- seq_len(nrow(named_inverse))
  
 if ( is.null(digits) ) return (named_inverse)
}

#' @rdname leontief_inverse_create
#' @export
leontieff_inverse_create <- function (technology_coefficients_matrix, 
                                      digits=NULL) {
  
  .Deprecated(new = leontief_inverse_create(technology_coefficients_matrix, 
                                            digits=NULL), 
              msg = "leontieff_inverse_create() is spelled correctly as leontief_inverse_create()")
  
  leontief_inverse_create(technology_coefficients_matrix, 
                          digits=NULL)
  
}
