#' @title Create an input coefficient matrix
#' 
#' @description Create an input coefficient matrix from the input flow matrix and the
#' output vector. The two input vectors must have consistent labelling, i.e
#' the same column names must be  found in the use table (input flow) and the
#' output vector.
#' 
#' @details The terminology follows the 
#' \href{https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0}{Eurostat Manual of Supply, Use and Input-Output Tables}.
#' Input-Output Multipliers Specification Sheet and Supporting Material, Spicosa Project Report, 
#' which cannot be linked due to a malformatted url, but can be found with a search engine.
#' this matrix is called 'technological coefficients'. The results of the function are 
#' tested on both sources.This is a wrapper function around  \code{\link{coefficient_matrix_create}}.
#' 
#' @param data_table A symmetric input-output table, a use table, 
#' a margins or tax table retrieved by the \code{\link{iotable_get}}
#'  function. 
#' @param households Defaults to \code{NULL}. Household column can be added 
#' with \code{TRUE}.
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @return A data frame that contains the matrix of first quadrant of the use table as
#' \code{input_flow} divided by \code{output} supported by a key column of 
#' product or industries, with a key column. 
#' Optionally the results are rounded to given \code{digits}. 
#' @return An input coefficient matrix of data.frame class. 
#' The column names are ordered, and the row names are in the 
#' first, auxiliary metadata column.
#' @examples 
#' input_coefficient_matrix_create ( 
#'                            iotable_get(), 
#'                            digits = 4 )
#'                            
#' #This is a wrapper function and equivalent to                           
#' 
#' coefficient_matrix_create( iotable_get(), 
#'                            total = "total", 
#'                            return = "products")
#' @export 

input_coefficient_matrix_create <- function ( data_table,
                                              households = FALSE,
                                              digits     = NULL) {
  
  
  cm <- coefficient_matrix_create(
                             data_table = data_table, 
                             total = "output", 
                             return_part = "products", 
                             households = households,
                             digits = digits)
  
  potential_total_names <- c( "cpa_total", "total")
  
  #TOTAL rows and columns must be removed
  key_column <- tolower(as.character(unlist(cm[,1])))
  remove_col <- which(potential_total_names %in% names(cm))
  remove_row <- which (key_column %in% potential_total_names)
  
  if ( length(remove_row) > 0 ) {
    cm <-  cm[-remove_row, ]  
  }
  
  if ( length(remove_col) > 0 ) {
    cm <-  cm[,-remove_col ]  
  }

  cm
  
}
