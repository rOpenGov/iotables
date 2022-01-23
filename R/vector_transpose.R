#' @title Transpose a vector to a long form
#' @description Many vectors (indicators, multipliers) are create in the wide form to confom matrixes in 
#' analytical functions.  For printing it is more useful to have them in long form. 
#' @details This is a wrapper around \code{\link[tidyr]{pivot_longer}} so you do not necessarily need to
#' import or load the entire \emph{tidyr} package.
#' @param data_table A matrix or vector that should have a key column.
#' @param names_to Defaults to \code{'nace_r2'}. 
#' @param values_to Defaults to \code{'value'}.
#' @param key_column_name The name of the first column. Defaults to \code{NULL} when it is not changed.
#' It should usually match the key column of the matrix or vector you would like to join the new 
#' vector created with \code{vector_transpose_longer}.
#' @param .keep Keep the indicator identifier column? Defaults to \code{FALSE}. 
#' @return A long form vector with a key column, and optionally the identifier of the indicator in
#' the first column.
#' @family iotables processing functions
#' @importFrom tidyr pivot_longer 
#' @importFrom dplyr any_of
#' @examples 
#' vector_transpose_longer(
#'   data.frame(indicator = "my_inidcator", 
#'              agriculture = 0.0123,
#'              manufacturing = 0.1436,
#'              trade = 0.0921)
#' )
#' @export  

vector_transpose_longer <- function( data_table, 
                                     names_to = "nace_r2", 
                                     values_to = "value", 
                                     key_column_name = NULL,
                                     .keep = FALSE ) {
  
  is_key_column_present(data_table)
  key_column <- names(data_table)[1]
  
  return_df <- data_table %>%
    tidyr::pivot_longer(
     -any_of(key_column), 
     names_to  = names_to, 
     values_to = values_to
   )

  if (.keep) return_df else return_df[,-1]
    
}

#' @rdname vector_transpose_longer
vector_transpose <- function( data_table, 
                              names_to = "nace_r2", 
                              values_to = "value", 
                              key_column_name = NULL,
                              .keep = FALSE ) { 
  
  .Deprecated(new= "vector_transpose_longer")
  
  vector_transpose_longer(data_table, names_to, values_to, key_column_name, .keep)
}


#' @title Transpose a vector to wider format
#' @description Many vectors (indicators, multipliers) are create in the wide form to confom matrixes in 
#' analytical functions.  For binding it is more useful to have them in wide format.
#' @details This is a wrapper around \code{\link[tidyr]{pivot_wider}} so you do not necessarily need to
#' import or load the entire \emph{tidyr} package.
#' @inheritParams key_column_create 
#' @param names_from,values_from A pair of
#'   arguments describing which column (or columns) to get the name of the
#'   output column (`names_from`), and which column (or columns) to get the
#'   cell values from (`values_from`).
#' @param key_column_values You can explicitly supply key column values. Defaults to \code{NULL} when the
#' key column values will be created from the long data. 
#' @family iotables processing functions
#' @examples 
#' vector_transpose_wider (data_table =  germany_airpol[, -2],
#'                         names_from = 'induse',
#'                         values_from = 'value')
#'                         
#' vector_transpose_wider (data_table =  germany_airpol[1:8, 3:4],
#'                         names_from = 'induse',
#'                         values_from = 'value', 
#'                         key_column_values = "CO2_emission" )
#' @export 
vector_transpose_wider <- function ( data_table, 
                                     names_from, 
                                     values_from,
                                     key_column_name = NULL, 
                                     key_column_values = NULL) {
  
  if (is.null(key_column_name)) key_column_name <- names(data_table)[1]
  if ( ncol(data_table)>2 & is_key_column_present(data_table) ) {
    pivot_wider ( data_table, 
                  names_from = names_from, 
                  values_from = values_from )
  } else {
    bind_cols (
      key_column_create(key_column_name, key_column_values), 
      pivot_wider ( data_table, 
                    names_from = names_from, 
                    values_from = values_from ))
  }
}

#' @title Create a key columnn
#' @description Create a key column for matching the dimensions of matrixes.
#' @details This function will likely be used with the creation of coefficients that need to be matched with
#' a matrix that has a key column.
#' @param key_column_name The name of the key column.
#' @param key_column_values The value(s) of the key column
#' @return A tibble with one column, named \code{key_column_name} and with values \code{key_column_values}.
#' @examples 
#' key_column_create ("iotables_row", c("CO2_multiplier", "CH4_multiplier"))
#' @importFrom tibble tibble 
#' @importFrom rlang set_names
#' @family iotables processing functions
#' @export

key_column_create <- function(key_column_name, 
                              key_column_values = NULL ) {
  
  tibble ( names = as.character(key_column_values) ) %>%
    rlang::set_names (key_column_name)
}
