#' @title Transpose a vector
#' @description Many vectors (indicators, multipliers) are create in the wide form to confom matrixes in 
#' analytical functions.  For printing it is more useful to have them in long form. 
#' @details This is a wrapper around \code{\link[tidyr]{pivot_longer}} so you do not necessarily need to
#' import or load the entire \emph{tidyr} package.
#' @param data_table A matrix or vector that should have a key column.
#' @param names_to Defaults to \code{'nace_r2'}. 
#' @param values_to Defaults to \code{'value'}.
#' @param .keep Keep the indicator identifier column? Defaults to \code{FALSE}. 
#' @return A long form vector with a key column, and optionally the identifier of the indicator in
#' the first column.
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
