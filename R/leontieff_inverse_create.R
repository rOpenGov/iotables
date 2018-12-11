#' Create the inverse of a Leontieff-matrix.
#' 
#' The inversion takes place after the basic properties of the Leontieff matrix. 
#' @param technology_coefficients_matrix A technology coefficient matrix created
#' by the \code{\link{input_coefficient_matrix_create}} or 
#' \code{\link{output_coefficient_matrix_create}}.
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @importFrom dplyr mutate_at mutate_if
#' @examples 
#' tm <- input_flow_get ( 
#'   data_table = iotable_get(), 
#'   households = FALSE)
#' I <- leontieff_inverse_create( technology_coefficients_matrix = tm )
#' @export 

leontieff_inverse_create <- function ( technology_coefficients_matrix, 
                                       digits=NULL) {
  . = NULL ; 
  
  leontieff_matrix<- leontieff_matrix_create( 
       technology_coefficients_matrix = technology_coefficients_matrix )
  Lm <- as.matrix(leontieff_matrix[,2:ncol(leontieff_matrix)])
  
  inverse <- solve( Lm )

  if ( sum(sapply(inverse, function(x) sum(is.nan(x)))) > 0) {
    stop ("Error: Could not invert the Leontieff-matrix.")
  }
  
  named_inverse <- cbind(as.data.frame(leontieff_matrix [,1]),
                     as.data.frame(inverse)) %>%
    dplyr::mutate_if (is.factor, as.character)
  
  names ( named_inverse ) <- names (leontieff_matrix)
  row.names ( named_inverse ) <- 1:nrow(named_inverse)
  
 if ( is.null(digits) ) return (named_inverse)
  
  round_table ( named_inverse, digits = digits  )
}
