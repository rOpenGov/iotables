#' @title Create the Leontief matrix (I − A)
#'
#' @description
#' Builds the **Leontief matrix** \eqn{(I - A)} from a technology (input)
#' coefficients matrix \eqn{A}. This is the intermediate step used before
#' computing the Leontief inverse with [leontief_inverse_create()].
#'
#' @details
#' In Eurostat terminology (*Manual of Supply, Use and Input–Output Tables*),
#' the technology coefficients matrix \eqn{A} is obtained by dividing each column
#' of the inter-industry flows by the total output of that industry.
#' The Leontief matrix is then \eqn{I - A}, also called the *matrix of
#' intermediate coefficients subtracted from identity*.
#'
#' This function removes any TOTAL rows/columns (e.g. `"total"`, `"cpa_total"`)
#' before forming \eqn{I - A}. It returns a `data.frame` with the original key
#' column followed by the numeric block of \eqn{I - A}.
#'
#' @param technology_coefficients_matrix A technology coefficients matrix created
#'   by [input_coefficient_matrix_create()] or [output_coefficient_matrix_create()].
#'   The first column must be a key; remaining columns must be numeric.
#'
#' @return
#' A `data.frame` whose first column is the key and whose remaining columns
#' contain the Leontief matrix \eqn{(I - A)}.
#'
#' @family analytic object functions
#'
#' @references
#' Beutel, J. (2008). *Eurostat Manual of Supply, Use and Input–Output Tables.*
#' Luxembourg: Office for Official Publications of the European Communities.
#'
#' Validation examples:
#' – Table 15.6 (p. 485): Direct requirements (input coefficients A)
#' – **Table 15.9 (p. 487): Leontief matrix (I − A)**
#' – Table 15.10 (p. 488): Leontief inverse (total requirements L = (I − A)⁻¹)
#'
#' Results reproduced by `input_coefficient_matrix_create()`,
#' `leontief_matrix_create()`, and `leontief_inverse_create()` using the built-in
#' dataset `iotable_get(source = "germany_1995")`.
#'
#' @examples
#' # From input coefficients (usual case)
#' tm <- input_coefficient_matrix_create(iotable_get(), households = FALSE)
#' L <- leontief_matrix_create(tm)
#'
#' @importFrom dplyr mutate across
#' @export

leontief_matrix_create <- function(technology_coefficients_matrix) {
  key_column <- as.character(
    unlist(technology_coefficients_matrix[, 1])
  )

  total_row <- which(c("total", "cpa_total") %in% tolower(key_column))
  total_col <- which(c("total", "cpa_total") %in% tolower(names(technology_coefficients_matrix)))

  if (length(total_row) > 0) {
    technology_coefficients_matrix <- technology_coefficients_matrix[-total_row, ]
  }

  if (length(total_col) > 0) {
    technology_coefficients_matrix <- technology_coefficients_matrix[, -total_col]
  }

  Tm <- as.matrix(technology_coefficients_matrix[, 2:ncol(technology_coefficients_matrix)])

  if (nrow(Tm) != ncol(Tm)) stop("Error: the input matrix is not symmetric.")

  IminusA <- diag(nrow(Tm)) - Tm

  if (sum(vapply(IminusA, function(x) sum(is.nan(x)), numeric(1))) > 0) {
    warning("Warning: There are invalid elements in the Leontief-matrix.")
  }

  Leontief <- cbind(
    as.data.frame(technology_coefficients_matrix[, 1]),
    as.data.frame(IminusA)
  )

  names(Leontief)[1] <- names(technology_coefficients_matrix)[1]
  Leontief[, 1] <- as.character(Leontief[, 1])

  Leontief
}

#' @rdname leontief_matrix_create
#' @export
leontieff_matrix_create <- function(technology_coefficients_matrix) {
  .Deprecated(
    new = leontief_matrix_create(technology_coefficients_matrix),
    msg = "leontieff_matrix_create() is spelled correctly as leontief_matrix_create()"
  )

  leontief_matrix_create(technology_coefficients_matrix)
}
