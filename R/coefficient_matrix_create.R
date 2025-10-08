#' @title Create a coefficient matrix
#'
#' @description
#' Compute a coefficient matrix from a symmetric input–output table (SIOT),
#' use table, or similar. By default, coefficients are related to output,
#' but you can use other totals if present.
#'
#' @details
#' The coefficient matrix \eqn{A} is formed by dividing each row of the
#' inter-industry flows by an output or supply total. By default, the
#' denominator is `"output"` (equivalent to `"P1"` or `"output_bp"`).
#' Alternative totals can be supplied via the `total` argument.
#'
#' @param data_table A symmetric input–output table, use table, margins or
#'   tax table retrieved by [iotable_get()].
#' @param total Character. Row label to use as denominator. Defaults to
#'   `"output"`. Accepts `"P1"`, `"output_bp"`, `"total"`, `"cpa_total"`.
#' @param digits Optional integer. Number of digits for rounding. Default
#'   `NULL` (no rounding).
#' @param return_part Optional. `"products"`, `"industries"`, or
#'   `"primary_inputs"` to select a subset of the matrix. Default `NULL`
#'   returns the full matrix.
#' @param remove_empty Logical. Defaults to `TRUE`. If `FALSE`, empty
#'   primary-input rows are kept. Empty product/industry rows are always
#'   removed.
#' @param households Logical. If `TRUE`, include household column. Default
#'   `FALSE`.
#' @param ... Optional extra arguments for future extensions, ignored by
#'   default.
#'
#' @return A data.frame with:
#' - The key column from `data_table`
#' - Numeric columns containing input coefficients
#'
#' @family indicator functions
#'
#' @examples
#' cm <- coefficient_matrix_create(
#'   data_table = iotable_get(source = "germany_1995"),
#'   total = "output",
#'   digits = 4
#' )
#'
#' @export
coefficient_matrix_create <- function(
    data_table,
    total = "output",
    digits = NULL,
    remove_empty = TRUE,
    households = FALSE,
    return_part = NULL,
    ...) {
  data_table <- data_table %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character))

  if (remove_empty) data_table <- empty_remove(data_table)

  last_column <- quadrant_separator_find(data_table, include_total = FALSE)

  if (!is.null(households) && households) {
    household_column <- household_column_get(data_table)
    quadrant <- data_table[, 1:last_column]
    data_table <- dplyr::left_join(quadrant, household_column,
      by = names(quadrant)[1]
    )
    last_column <- last_column + 1
  } else {
    data_table <- data_table[, 1:last_column]
  }

  key_column <- tolower(as.character(unlist(data_table[, 1])))

  # find denominator row
  if (total %in% c("output", "p1", "output_bp")) {
    total_row <- data_table[which(key_column %in%
      c("output", "p1", "output_bp", "total output"))[1], ]
    if (nrow(total_row) == 0) stop("Output row not found.")
  } else if (total %in% c("total", "cpa_total")) {
    total_row <- data_table[which(key_column %in% c("total", "cpa_total"))[1], ]
  } else {
    total_row <- data_table[which(tolower(key_column) %in% tolower(total)[1]), ]
    if (nrow(total_row) == 0) stop("Requested total row not found.")
  }

  # avoid division by zero
  null_to_eps <- function(x) ifelse(x == 0, 1e-6, x)
  total_row <- dplyr::mutate(
    total_row,
    dplyr::across(where(is.numeric), null_to_eps)
  )

  coeff_matrix <- data_table %>%
    dplyr::mutate(dplyr::across(where(is.numeric), as.numeric))

  for (i in seq_len(nrow(data_table))) {
    coeff_matrix[i, 2:last_column] <-
      coeff_matrix[i, 2:last_column] / as.numeric(total_row[2:last_column])
  }

  # return_part filter
  if (!is.null(return_part)) {
    last_row <- which(tolower(unlist(data_table[, 1])) %in%
      c("cpa_total", "total", "total output"))
    if (return_part == "primary_inputs") {
      coeff_matrix <- coeff_matrix[last_row:nrow(coeff_matrix), ]
    } else if (return_part %in% c("products", "industries")) {
      coeff_matrix <- coeff_matrix[1:last_row, ]
    }
  }

  if (is.null(digits)) coeff_matrix else round_table(coeff_matrix, digits)
}
