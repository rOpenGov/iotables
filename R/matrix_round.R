#' Round Matrix Values
#'
#' Round all numeric values in an input–output style table to a specified number
#' of digits. The key column (first column) is preserved unchanged.
#'
#' @param data_table A symmetric input–output table, use table, supply table,
#'   tax table, or margins table.
#' @param digits Integer number of decimal places to round to. Defaults to `0`.
#'
#' @return A `data.frame` (or tibble) with the key column intact and all other
#'   numeric columns rounded to the given precision.
#'
#' @details
#' This is useful for comparing results across software or publications that
#' present rounded tables.
#'
#' @family iotables processing functions
#'
#' @examples
#' de_coeff <- input_coefficient_matrix_create(iotable_get())
#' head(matrix_round(de_coeff, digits = 2))
#'
#' @importFrom dplyr mutate across
#' @export
matrix_round <- function(data_table, digits = 0) {
  data_table %>%
    mutate(across(where(is.numeric), function(x) round(x, digits)))
}
