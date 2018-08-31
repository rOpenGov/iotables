#' Create an empty conforming vector
#' 
#' This helper function creates you a named vector that conforms your
#' analytical objects, such as the use table, the Leontieff-matrix, 
#' etc. With 60x60 matrixes it is easy to make mistakes with manual 
#' definition. The empty effects vector can be used in .csv format
#' as a sample to import scenearios from a spreadsheet application.
#'  
#' @param dat A use table, Leontieff-matrix, Leontieff-inverse, a coefficient matrix 
#' or other named matrix / vector.
#' @examples
#' de_use <- use_table_get ( source = "germany_1990", geo = "DE",
#'                          year = 1990, unit = "MIO_EUR", 
#'                          households = FALSE, labelling = "iotables")
#'
#' conforming_vector_create (de_use)
#' @export 

conforming_vector_create <- function ( dat ) {
  
  conforming_vector <- dat[1,]
  
  conforming_vector [] <- apply ( conforming_vector, 1, function(x) x = 0 )
  
  conforming_vector 
}
