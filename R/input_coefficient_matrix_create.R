#' @title Create an input coefficient matrix
#'
#' @description
#' Create an input coefficient matrix from the input flow matrix and the
#' output vector. This is a thin wrapper around
#' [coefficient_matrix_create()], with `total = "output"` and
#' `return_part = "products"`.
#'
#' @details
#' The input coefficients of production activities may be interpreted as the
#' corresponding cost shares for products and primary inputs in total output.
#' Our terminology follows the
#' \href{https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF}{Eurostat
#' Manual of Supply, Use and Input–Output Tables}. In some sources this
#' is called the *technological coefficients matrix*.
#'
#' @param data_table A symmetric input–output table, a use table,
#'   or margins/tax table retrieved by [iotable_get()].
#' @param households Logical; include the households column if available.
#'   Default `FALSE`.
#' @param digits Optional integer precision to round the resulting matrix.
#'   Default `NULL` (no rounding).
#'
#' @return
#' A `data.frame` containing the input coefficient matrix (products ×
#' products), with the key (row label) retained as the first column.
#' Any TOTAL rows/columns (`"cpa_total"`, `"total"`) are removed.
#'
#' @examples
#' cm <- input_coefficient_matrix_create(
#'   iotable_get(source = "germany_1995"),
#'   digits = 4
#' )
#' head(cm)
#'
#' # Equivalent direct call:
#' cm2 <- coefficient_matrix_create(
#'   iotable_get(source = "germany_1995"),
#'   total = "output",
#'   return_part = "products",
#'   digits = 4
#' )
#'
#' @export
input_coefficient_matrix_create <- function(data_table,
                                            households = FALSE,
                                            digits = NULL) {
  cm <- coefficient_matrix_create(
    data_table = data_table,
    total = "output",
    return_part = "products",
    households = households,
    digits = digits
  )

  potential_total_names <- c("cpa_total", "total")

  # Remove TOTAL rows and columns
  key_column <- tolower(as.character(unlist(cm[, 1])))
  remove_col <- which(potential_total_names %in% names(cm))
  remove_row <- which(key_column %in% potential_total_names)

  if (length(remove_row) > 0) cm <- cm[-remove_row, ]
  if (length(remove_col) > 0) cm <- cm[, -remove_col]

  cm
}
