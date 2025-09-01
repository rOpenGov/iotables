#' Create output multipliers
#'
#' @description
#' Compute output multipliers from a Leontief inverse matrix.
#'
#' @details
#' The output multipliers are defined as the **column sums** of the
#' Leontief inverse \eqn{(I - A)^{-1}}, where \eqn{A} is the input
#' coefficient matrix. They measure the total direct and indirect
#' output generated in each industry per unit increase in final demand.
#'
#' See Eurostat (2008), *Manual of Supply, Use and Input–Output Tables*,
#' p. 500; UN (2018), *Handbook on Supply and Use Tables and Input–Output
#' Tables with Extensions and Applications*, §15.35.
#'
#' @param input_coefficient_matrix A technology–coefficient matrix as
#'   returned by [input_coefficient_matrix_create()].
#'
#' @return A one-row `data.frame` (or tibble) with:
#' - The first column a label `"output_multipliers"`.
#' - Remaining columns the multipliers for each industry.
#'
#' @family multiplier functions

#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#'
#' @examples
#' de_input_coeff <- input_coefficient_matrix_create(
#'   iotable_get(),
#'   digits = 4
#' )
#'
#' output_multiplier_create(de_input_coeff)
#'
#' @export

output_multiplier_create <- function(input_coefficient_matrix) {
  inv <- leontief_inverse_create(input_coefficient_matrix)
  multipliers <- colSums(inv[, -1, drop = FALSE])
  key_col <- names(inv)[1]
  
  # build a named list in base R
  row <- c(list(output_multipliers = "output_multipliers"), 
           as.list(multipliers))
  names(row)[1] <- key_col
  
  tibble::as_tibble(row)
}

