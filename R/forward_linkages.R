#' Forwardlinkages
#' 
#' Forwardlinkeages as defined by the Eurostat Manual of Supply, Use and Input-Output
#' Tables (see p506-507.)
#' @param Im A Leontieff inverse matrix created by the \code{\link{leontieff_inverse_create}} function. 
#' @importFrom dplyr mutate_if
#' @examples 
#' #You need a table that has a total column and either the total 
#' #intermediate use or final use
#' #This is usually the case with Eurostat tables, but with the Germany data
#' #file total must be added.
#' 
#' io_table <- iotable_get () 
#' io_table <- io_table [1:which(tolower(io_table[,1]) =="total" ), ]

#' output_bp <- dplyr::select ( io_table, output_bp )
#' io_table <- io_table [, 1:7] 
#' io_table$total <- rowSums(io_table[, 2:7])
#' io_table <- cbind (io_table, output_bp)
#' 
#' de_out <- output_coefficient_matrix_create ( io_table = io_table, 
#'                                     type = 'final_demand',
#'                                     digits = 4)
#'
#' forward_linkages ( output_coefficient_matrix = de_out, 
#'                    digits = 4 )
#' 
#' @export 

forward_linkages <- function ( output_coefficient_matrix, 
                               digits  = NULL) {
  . = NULL ; funs = NULL ; vars = NULL
  
 output_coefficient_matrix <- dplyr::mutate_if (output_coefficient_matrix, 
                                                is.factor, as.character )
  
  first_col <- output_coefficient_matrix [, 1]
  Ocm <- as.matrix(output_coefficient_matrix [, -1])
  
  L  <- leontieff_matrix_create( output_coefficient_matrix  )
  I <- leontieff_inverse_create(L)
  FLm <- I
  FLm$forward = rowSums(I[, 2:ncol(I)])
  
  if ( !is.null(digits)) {
    FLm[, 2:ncol(FLm)] <- round ( FLm[, 2:ncol(FLm)], digits )
  }
  FLm[, c(1, ncol(FLm))]
}