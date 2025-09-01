#' @title Create Ghosh inverse from output coefficients
#'
#' @description
#' Compute the Ghosh inverse from an output-coefficient matrix.
#'
#' @details
#' The Ghosh inverse is defined as \eqn{G = (I - B)^{-1}}, where \eqn{B} is
#' the output-coefficient matrix created by
#' [output_coefficient_matrix_create()].
#'
#' See the United Nations *Handbook on Supply and Use Tables and Input–Output
#' Tables with Extensions and Applications* (2018, Rev. 1), pp. 622–639
#' (\href{https://unstats.un.org/unsd/nationalaccount/docs/SUT_IOT_HB_Final_Cover.pdf}{PDF}).
#'
#' For the analogous inverse based on input coefficients, see
#' [leontief_inverse_create()].
#'
#' @param output_coefficients_matrix A technology-coefficient matrix created by
#'   [output_coefficient_matrix_create()].
#' @param digits Optional integer precision for rounding. Default `NULL`
#'   (no rounding).
#'
#' @return
#' A `data.frame` with the original key column and the Ghosh inverse in the
#' remaining columns. If `digits` is provided, values are rounded.
#'
#' @importFrom dplyr mutate across where
#' @family analytic object functions
#'
#' @examples
#' # Minimal example
#' om <- output_coefficient_matrix_create(iotable_get())
#' ghosh_inverse_create(om)
#'
#' # Using the Germany 1995 benchmark table (Eurostat manual)
#' # data(germany_1995)
#' # om_de <- output_coefficient_matrix_create(germany_1995)
#' # ghosh_inverse_create(om_de)
#'
#' @export


ghosh_inverse_create <- function(output_coefficients_matrix,
                                 digits = NULL) {
  # The Ghosh-model is a dual pair of the Leontief-model, so we can use
  # the same functions, but with different inputs (output coefficients
  # instead of input coefficients.)

  ghosh_matrix <- leontief_matrix_create(
    technology_coefficients_matrix = output_coefficients_matrix
  )

  Gm <- as.matrix(ghosh_matrix[, 2:ncol(ghosh_matrix)])

  inverse <- solve(Gm)

  if (sum(vapply(inverse, function(x) sum(is.nan(x)), numeric(1))) > 0) {
    stop("Error: Could not invert the Ghosh-matrix.")
  }

  named_inverse <- cbind(
    as.data.frame(ghosh_matrix[, 1]),
    as.data.frame(inverse)
  ) %>%
    dplyr::mutate(dplyr::across(
      where(is.factor),
      function(x) as.character(x)
    ))

  names(named_inverse) <- names(ghosh_matrix)
  row.names(named_inverse) <- seq_len(nrow(named_inverse))

  if (is.null(digits)) {
    return(named_inverse)
  }

  round_table(named_inverse, digits = digits)
}
