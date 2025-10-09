#' @title Create the Leontief inverse
#'
#' @description Computes the **Leontief inverse**, which measures the total
#' (direct + indirect) requirements of all products or industries for one unit
#' of final demand.
#'
#' @details The Leontief inverse is defined as \deqn{L = (I - A)^{-1}} where
#' \eqn{A} is the input-coefficient (or technology-coefficient) matrix created
#' by [input_coefficient_matrix_create()].
#'
#' Each element \eqn{l_{ij}} of \eqn{L} shows the total production of product
#' *i* required—both directly and indirectly—to satisfy one unit of final demand
#' for product *j*.
#'
#' This formulation follows the *Eurostat Manual of Supply, Use and Input–Output
#' Tables* (Beutel 2008, Chapter 15; see equations (19), (43)) and the *UN
#' Handbook on Supply and Use Tables and Input–Output Tables with Extensions and
#' Applications* (2018 Rev. 1, pp. 619–621). The Leontief inverse is also called
#' the **total requirements matrix** in both manuals.
#'
#' For the analogous inverse based on output coefficients, see
#' [ghosh_inverse_create()].
#'
#' @param technology_coefficients_matrix A technology-coefficient matrix created
#'   by [input_coefficient_matrix_create()].
#' @param digits Optional integer specifying rounding precision. Default `NULL`
#'   (no rounding).
#'
#' @return A `data.frame` with the original key column and the Leontief inverse
#' in the remaining columns. If `digits` is supplied, values are rounded.
#'
#' @importFrom dplyr mutate across
#' @family analytic object functions
#'
#' @references Beutel, J. (2008). *Eurostat Manual of Supply, Use and
#' Input–Output Tables.* Luxembourg: Office for Official Publications of the
#' European Communities.
#'
#' Validation examples: – Table 15.6 (p. 485): Direct requirements (input
#' coefficients) – Table 15.10 (p. 488): Total requirements (Leontief inverse)
#'
#' Results reproduced by `input_coefficient_matrix_create()` and
#' `leontief_inverse_create()` using the built-in dataset `iotable_get(source =
#' "germany_1995")`.
#'
#' @examples
#' # --- Minimal hand-checkable 2×2 system ---
#' minimal_matrix <- data.frame(
#'   sector = c("A", "B"),
#'   A = c(0.2, 0.4),
#'   B = c(0.1, 0.2)
#' )
#' leontief_inverse_create(minimal_matrix, digits = 3)
#'
#' # --- Full Eurostat example (Germany 1995) ---
#' cm_de <- input_coefficient_matrix_create(
#'   data_table = iotable_get(source = "germany_1995")
#' )
#' leontief_inverse_create(cm_de)
#'
leontief_inverse_create <- function(technology_coefficients_matrix,
                                    digits = NULL) {
  leontief_matrix <- leontief_matrix_create(
    technology_coefficients_matrix = technology_coefficients_matrix
  )

  Lm <- as.matrix(leontief_matrix[, 2:ncol(leontief_matrix)])

  inverse <- solve(Lm)

  if (sum(vapply(inverse, function(x) sum(is.nan(x)), numeric(1))) > 0) {
    stop("Error: Could not invert the Leontief-matrix.")
  }

  named_inverse <- cbind(
    as.data.frame(leontief_matrix[, 1]),
    as.data.frame(inverse)
  ) %>%
    mutate(across(where(is.factor), as.character))

  names(named_inverse) <- names(leontief_matrix)
  row.names(named_inverse) <- seq_len(nrow(named_inverse))

  if (is.null(digits)) {
    return(named_inverse)
  } else {
    round_table(named_inverse, digits = digits)
  }
}

#' @rdname leontief_inverse_create
#' @export
leontieff_inverse_create <- function(technology_coefficients_matrix,
                                     digits = NULL) {
  .Deprecated(
    new = leontief_inverse_create(technology_coefficients_matrix,
      digits = NULL
    ),
    msg = "leontieff_inverse_create() is spelled correctly as leontief_inverse_create()"
  )

  leontief_inverse_create(technology_coefficients_matrix,
    digits = NULL
  )
}
