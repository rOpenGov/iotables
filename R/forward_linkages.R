#' @title Forward linkages
#' 
#' @description The increased output of a sector indicates that additional
#' amounts of products are available to be used as inputs by other sectors which can increase their 
#' production, which is captured in this indicator vector.
#' @details Forward linkages as defined by the Eurostat Manual of Supply, Use and
#' Input-Output Tables (pp. 506--507) and the United Nations 
#' Handbook on Supply and Use Tables and Input-Output Tables with Extensions and Applications p637.
#' @param output_coefficient_matrix An output coefficient matrix created 
#' with the \code{\link{output_coefficient_matrix_create}} function. 
#' @param digits Number of decimals for rounding, defaults to \code{NULL}.
#' @importFrom dplyr mutate across 
#' @return The vector of industry (product) forward linkages in a 
#' long-form data.frame, containing the metadata column of the the row
#' names from the \code{output_coefficient_matrix}.
#' @family linkage functions
#' @examples 
#' data_table = iotable_get()
#' 
#' de_out <- output_coefficient_matrix_create ( 
#'  data_table, "tfu", digits = 4
#'  )
#'
#' forward_linkages(output_coefficient_matrix = de_out, 
#'                  digits = 4 )
#' @export 

forward_linkages <- function ( output_coefficient_matrix, 
                               digits  = NULL) {

  output_coefficient_matrix <- mutate (output_coefficient_matrix, 
                                       across(where(is.factor), as.character) )
  first_col <- output_coefficient_matrix [, 1]
  G <- ghosh_inverse_create( output_coefficient_matrix )
  FLm <- G
  FLm$forward_linkages <- rowSums(G[, -1])
  
  if ( !is.null(digits)) {
    FLm[, 2:ncol(FLm)] <- round ( FLm[, 2:ncol(FLm)], digits )
  }
  FLm[, c(1, ncol(FLm))]
}
