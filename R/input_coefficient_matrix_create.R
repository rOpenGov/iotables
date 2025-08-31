#' Create an input coefficient matrix
#'
#' @description
#' Create an input coefficient matrix from the input flow matrix and the
#' output vector. The two inputs must have consistent labelling, i.e.
#' the same product/industry column names must be found in the use table
#' (input flow) and in the output vector.
#'
#' @details
#' The input coefficients of production activities may be interpreted as the
#' corresponding cost shares for products and primary inputs in total output.
#' Our terminology follows the
#' \href{https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0}{Eurostat Manual of Supply, Use and Input-Output Tables}.
#'
#' In some sources this matrix is called the *technological coefficients* matrix.
#' This is a thin wrapper around [coefficient_matrix_create()].
#'
#' @param data_table A symmetric input–output table, a use table,
#' a margins or tax table retrieved by [iotable_get()].
#' @param households Logical; include the households column if available. Default `FALSE`.
#' @param digits Optional integer precision to round the resulting matrix. Default `NULL`
#'   (no rounding).
#'
#' @return
#' A `data.frame` containing the input coefficient matrix (first quadrant / products),
#' with the key (row label) retained as the first column. Any TOTAL rows/columns
#' named like `"cpa_total"` or `"total"` (case-insensitive) are removed. If
#' `digits` is provided, results are rounded.
#'
#' @examples
#' # Basic usage with the package-provided demo table
#' cm <- input_coefficient_matrix_create(
#'   iotable_get(),
#'   total = "output",
#'   digits = 4
#' )
#' head(cm)
#'
#' # Equivalent direct call via the underlying helper:
#' cm2 <- coefficient_matrix_create(
#'   data_table = iotable_get(),
#'   total = "output",
#'   return_part = "products",
#'   households = FALSE,
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

  # TOTAL rows and columns must be removed
  key_column <- tolower(as.character(unlist(cm[, 1])))
  remove_col <- which(potential_total_names %in% names(cm))
  remove_row <- which(key_column %in% potential_total_names)

  if (length(remove_row) > 0) {
    cm <- cm[-remove_row, ]
  }

  if (length(remove_col) > 0) {
    cm <- cm[, -remove_col]
  }

  cm
}
