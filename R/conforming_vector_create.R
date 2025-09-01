#' Create an Empty Conforming Vector
#'
#' Create a named vector (in wide format) that conforms to the structure of a
#' given analytical object, such as a use table, coefficient matrix, or
#' Leontief matrix. This helps avoid mistakes when manually defining large
#' vectors (e.g., for 60 × 60 matrices).
#'
#' The empty conforming vector can also be exported to `.csv` format and used
#' as a template for importing scenarios from a spreadsheet application.
#'
#' @param data_table A use table, coefficient matrix, Leontief matrix, or other
#'   named matrix or data frame.
#'
#' @return A one-row `data.frame` with the same column names as `data_table`,
#'   but with all values set to zero.
#'
#' @family iotables processing functions
#'
#' @examples
#' de_input_flow <- input_flow_get(data_table = iotable_get())
#' conforming_vector_create(de_input_flow)
#'
#' @export
conforming_vector_create <- function(data_table) {
  conforming_vector <- data_table[1, , drop = FALSE]
  conforming_vector[] <- 0
  conforming_vector
}
