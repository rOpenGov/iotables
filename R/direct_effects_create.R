#' @title Create direct effects
#'
#' @description The function creates the effects.
#' @param input_requirements A matrix or vector created by
#' \code{\link{input_indicator_create}}
#' @param inverse A Leontief-inverse created by
#' \code{\link{leontief_inverse_create}}.
#' @param digits Rounding digits, defaults to \code{NULL}, in which case
#' no rounding takes place.
#' @importFrom dplyr select mutate all_of
#' @return A data.frame containing the direct effects and the necessary
#' metadata to sort them or join them with other matrixes.
#' @family indicator functions
#' @examples
#' nl <- netherlands_2000
#'
#' input_coeff_nl <- input_coefficient_matrix_create(
#'   data_table = netherlands_2000,
#'   households = FALSE
#' )
#'
#' compensation_indicator <- input_indicator_create(netherlands_2000, "compensation_employees")
#'
#' I_nl <- leontief_inverse_create(input_coeff_nl)
#'
#' direct_effects_create(
#'   input_requirements = compensation_indicator,
#'   inverse = I_nl
#' )
#' @export

direct_effects_create <- function(input_requirements,
                                  inverse,
                                  digits = NULL) {
  names_direct <- names(input_requirements)
  key_column <- names(input_requirements)[1]
  replace_dot <- function(x) gsub(pattern = "_indicator", "", x)
  extend <- function(x) paste0(x, "_effect")

  new_key_column <- input_requirements %>%
    select(1:2) %>%
    mutate(across(all_of(key_column), replace_dot)) %>%
    mutate(across(all_of(key_column), extend))

  if (all(names(inverse) %in% names(input_requirements))) {
    input_requirements <- select(
      input_requirements,
      all_of(names(inverse))
    )
  }

  col_n <- ncol(input_requirements)

  # columns of the left matrix must be the same as the number of rows of
  # the right matrix
  # Remove key column------
  key_column <- subset(input_requirements, select = 1)
  input_requirements_matrix <- input_requirements[, -1]
  inverse <- inverse[, -1]

  inverse <- as.matrix(inverse)
  input_requirements_matrix <- as.matrix(input_requirements_matrix)


  effects <- input_requirements_matrix %*% inverse

  if (!is.null(digits)) {
    if (digits >= 0) {
      effects <- round(effects, digits)
    }
  }

  cbind(new_key_column[, 1], effects)
}
