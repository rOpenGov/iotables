#' Forward linkages
#' 
#' Forward linkeages as defined by the Eurostat Manual of Supply, Use and Input-Output
#' Tables (see page 506.)
#' @param Im A Leontieff inverse matrix created by the \code{\link{leontieff_inverse_create}} function. 
#' @importFrom dplyr mutate_if
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
#' forward_linkages ( Im )
#' 
#' @export 

forward_linkages <- function ( Im ) {
  . = NULL ; funs = NULL ; vars = NULL
  
 Im <- dplyr::mutate_if (Im, is.factor, as.character )
 
 forward_link <- Im
  
 forward_link$total <- rowSums( Im[, -1] )
   
 forward_link
  
}
