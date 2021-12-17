#' @title Create an empty conforming vector
#' 
#' @description This helper function creates you a named vector that conforms your
#' analytical objects, such as the use table, the Leontieff-matrix, 
#' etc. With 60x60 matrixes it is easy to make mistakes with manual 
#' definition. The empty effects vector can be used in .csv format
#' as a sample to import scenarios from a spreadsheet application.
#'  
#' @param data_table A use table, Leontieff-matrix, Leontieff-inverse,
#' a coefficient matrix or other named matrix / vector.
#' @return A wide-format conforming vector of data frame class, 
#' with column names matching the metadata of the \code{data_table}.
#' @examples
#' de_input_flow <- input_flow_get(data_table = iotable_get())
#' 
#' conforming_vector_create (data_table = de_input_flow)
#' @family iotables processing functions
#' @export 

conforming_vector_create <- function (data_table) {
  
  conforming_vector <- data_table[1,]
  
  conforming_vector[] <- apply ( conforming_vector, 1, function(x) x = 0 )
  
  conforming_vector 
}
