#' @title Transpose a vector
#' @description Many vectors (indicators, multipliers) are create in the wide form to confom matrixes in 
#' analytical functions.  For printing it is more useful to have them in long form. 
#' @param data_table A matrix or vector that should have a key column.
#' @param names_to Defaults to \code{'nace_r2'}. 
#' @param values_to Defaults to \code{'value'}.
#' @param .keep Keep key column? Defaults to \code{FALSE}. 
#' @return A logical variable of length 1, \code{TRUE} or \code{FALSE}.
#' @importFrom tidyr pivot_longer 
#' @importFrom dplyr any_of
#' @examples 
#' vector_transpose (
#'   data.frame(indicator = "my_inidcator", 
#'              agriculture = 0.0123,
#'              manufacturing = 0.1436,
#'              trade = 0.0921)
#' )
#' @export  

vector_transpose <- function( data_table, 
                              names_to = "nace_r2", 
                              values_to = "value", 
                              .keep = FALSE ) {
  
  is_key_column_present(data_table)
  key_column <- names(data_table)[1]
  
  return_df <- data_table %>% tidyr::pivot_longer(
     -any_of(key_column), 
     names_to  = names_to, 
     values_to = values_to
   )

  if (.keep) return_df else return_df[,-1]
    
}
