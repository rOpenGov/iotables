#' @title Backward linkages
#' 
#' @description Indicate the interconnection of a particular sector to other sectors
#' from which it purchases inputs (demand side). When a sector increases its output, 
#' it will increase the total (intermediate) demand on all other sectors, which is measured
#' by backward linkages. 
#' 
#' @details Backward linkages are defined as the column sum of the Leontief inverse, in line with 
#' the Eurostat Manual of Supply, Use and
#' Input-Output Tables (see p506-507.) and the 
#' Handbook on Supply and Use Tables and Input-Output Tables with Extensions and Applications of 
#' the United Nations (see p636,)
#' @param Im A Leontief inverse matrix created by the
#' \code{\link{leontief_inverse_create}} function. 
#' @return The vector of industry (product) backward linkages in a wide
#' data.frame class, following the column names of the Leontief 
#' inverse matrix. 
#' @importFrom dplyr mutate across
#' @family linkage functions
#' @examples 
#' de_coeff <- input_coefficient_matrix_create( iotable_get(), 
#'                                              digits = 4 )
#' I <- leontief_inverse_create (de_coeff)
#' backward_linkages (I)
#' @export 

backward_linkages <- function ( Im ) {

 Im <- mutate (Im, across(where(is.factor), as.character))
  
  total_row <- data.frame ( 
    name = "backward linkages"
    )
  
  names(total_row)[1] <- names(Im[1])
  
  total_row <- cbind ( total_row, 
                       t(colSums(Im[,2:ncol(Im)]))
                       )
  
  total_row
  
}
