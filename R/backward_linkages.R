#' Backward linkages
#' 
#' Backward linkages as defined by the Eurostat Manual of Supply, Use and
#' Input-Output Tables (see p506-507.)
#' @param Im A Leontieff inverse matrix created by the
#' \code{\link{leontieff_inverse_create}} function. 
#' @return The vector of industry (product) backward linkages in a wide
#' data.frame class, following the column names of the Leontieff 
#' inverse matrix. 
#' @importFrom dplyr mutate_if
#' @examples 
#' de_coeff <- input_coefficient_matrix_create( iotable_get(), 
#'                                              digits = 4 )
#' I <- leontieff_inverse_create (de_coeff)
#' backward_linkages (I)
#' @export 

backward_linkages <- function ( Im ) {
  . = NULL ; funs = NULL ; vars = NULL
  
 Im <- dplyr::mutate_if (Im, is.factor, as.character )
  
  total_row <- data.frame ( 
    name = "backward linkages"
    )
  
  names ( total_row )[1] <- names ( Im[1])
  
  total_row <- cbind ( total_row, 
                       t(colSums(Im[,2:ncol(Im)]))
                       )
  
  total_row
  
}
