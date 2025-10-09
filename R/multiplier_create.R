#' Create multipliers
#'
#' Wrapper around [equation_solve()] that computes total multipliers by
#' post-multiplying an input indicator vector with a Leontief inverse and
#' adds a key column carrying the multiplier name for consistent joins.
#'
#' In the Eurostat IO framework, multipliers measure *total* effects per
#' unit of **final demand**, by product or industry (via the Leontief
#' inverse \eqn{(I - A)^{-1}}). This contrasts with *direct effects*,
#' which reflect only the immediate (first-round) impact.
#'
#' @param input_vector A named numeric vector (or 1-column matrix)
#'   created by [input_indicator_create()] whose names match the ordering
#'   of the Leontief inverse columns.
#' @param Im A Leontief inverse matrix created by
#'   [leontief_inverse_create()]. Column names must correspond to products
#'   or industries consistent with `input_vector`.
#' @param multiplier_name A string used for the key column that labels the
#'   returned multipliers. Default is `"multiplier"`.
#' @param digits Optional integer. If supplied and non-negative, round the
#'   resulting multipliers to this number of decimal places. Negative
#'   values are ignored (no rounding).
#'
#' @return A data frame with:
#' * a first key column (character) named as the first column of
#'   `input_vector` and filled with `multiplier_name`, and
#' * one numeric column per product/industry containing the multipliers.
#'
#' @details
#' The function delegates the numerical solve to [equation_solve()] and
#' then formats the result for tidy joining with other IO tables. Ensure
#' that the dimension ordering and names of `input_vector` and `Im`
#' correspond; otherwise results will be misaligned.
#'
#' @seealso [equation_solve()], [input_indicator_create()],
#'   [leontief_inverse_create()]
#'
#' @family multiplier functions
#'
#'  @references
#' Beutel, J. (2008).
#' *Eurostat Manual of Supply, Use and Input–Output Tables.*
#' Luxembourg: Office for Official Publications of the European
#' Communities.
#'
#' Validation examples:
#' – Table 15.16 (pp. 503–504): Total multipliers (Germany 1995)
#'
#' Results reproduced by `multiplier_create()` using
#' `iotable_get(source = "germany_1995")`.
#'
#' @examples
#' # Minimal workflow -----------------------------------------------
#' data_table <- iotable_get()
#'
#' coeff_de <- input_coefficient_matrix_create(data_table)
#'
#' de_gva_indicator <- input_indicator_create(
#'   data_table = data_table,
#'   input = "gva"
#' )
#'
#' I_de <- leontief_inverse_create(coeff_de)
#'
#' de_gva_multipliers <- multiplier_create(
#'   input_vector = de_gva_indicator,
#'   Im = I_de,
#'   multiplier_name = "employment_multiplier",
#'   digits = 4
#' )
#'
#' @export

multiplier_create <- function(input_vector,
                              Im,
                              multiplier_name = "multiplier",
                              digits = NULL) {
  if (!is.null(digits) && digits < 0) digits <- NULL

  multipliers <- equation_solve(
    LHS = input_vector,
    Im = Im
  )

  if (!is.null(digits)) {
    multipliers <- round(multipliers, digits)
  }
  names(multipliers) <- names(Im)[2:ncol(Im)]
  row_name <- as.data.frame(multiplier_name)
  names(row_name)[1] <- names(input_vector)[1]

  named_multipliers <- cbind(row_name, multipliers)
  named_multipliers[, 1] <- as.character(named_multipliers[, 1])

  if (!is.null(digits)) matrix_round(named_multipliers, digits) else named_multipliers
}
