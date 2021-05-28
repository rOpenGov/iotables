#' Forward linkages
#' 
#' Forward linkages as defined by the Eurostat Manual of Supply, Use and
#' Input-Output Tables (see p506-507.)
#' @param output_coefficient_matrix An output coefficient matrix created 
#' with the \code{\link{output_coefficient_matrix_create}} function. 
#' @param digits Number of decimals for rounding, defaults to \code{NULL}.
#' @importFrom dplyr mutate_if
#' @return The vector of industry (product) forward linkages in a 
#' long-form data.frame, containing the metadata column of the the row
#' names from the \code{output_coefficient_matrix}.
#' @family interindustrial linkage functions
#' @examples 
#' data_table = iotable_get()
#' 
#' de_out <- output_coefficient_matrix_create ( 
#'  data_table, "tfu", digits = 4
#'  )
#'
#' forward_linkages ( output_coefficient_matrix = de_out, 
#'                    digits = 4 )
#' @export 


forward_linkages <- function ( output_coefficient_matrix, 
                               digits  = NULL) {

 output_coefficient_matrix <- dplyr::mutate_if (output_coefficient_matrix, 
                                                is.factor, as.character )
  
  first_col <- output_coefficient_matrix [, 1]
  Ocm <- as.matrix(output_coefficient_matrix [, -1])

  I <- leontieff_inverse_create( output_coefficient_matrix )
  FLm <- I
  FLm$forward_linkages <- rowSums(I[, 2:ncol(I)])
  
  if ( !is.null(digits)) {
    FLm[, 2:ncol(FLm)] <- round ( FLm[, 2:ncol(FLm)], digits )
  }
  FLm[, c(1, ncol(FLm))]
}
