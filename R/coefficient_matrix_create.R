#' @title Create an input (technical) coefficient matrix
#'
#' @description
#' Computes the **input (technical) coefficient matrix** from a symmetric
#' input–output table (SIOT), use table, or supply table. Each element
#' represents the amount of a product used as input per unit of output
#' (usually at basic prices).
#'
#' @details
#' The coefficient matrix \eqn{A}—also called the *technological* or
#' *direct requirements* matrix—is obtained by dividing each element of
#' the inter-industry (intermediate use) block \eqn{Z} by the corresponding
#' column total of output \eqn{x}:
#'
#' \deqn{A_{ij} = Z_{ij} / x_j}
#'
#' where \eqn{Z_{ij}} denotes the use of product *i* by industry (or product)
#' *j*, and \eqn{x_j} is total output at basic prices.
#'
#' This formulation follows the *Eurostat Manual of Supply, Use and
#' Input–Output Tables* (Beutel, 2008, §15.1–15.3) and the *System of
#' National Accounts 2008* (§14.90–14.91).
#'
#' By default, the denominator row is `"output"` (equivalent to ESA 2010
#' transaction `"P1"` or `"output_bp"`). Alternative totals such as
#' `"total"` or `"cpa_total"` may be used through the `total` argument.
#'
#' @param data_table A symmetric input–output table, use table, margins or
#'   tax table retrieved by [iotable_get()].
#' @param total Character. Label of the row used as denominator.
#'   Defaults to `"output"`. Accepts `"P1"`, `"output_bp"`, `"total"`,
#'   or `"cpa_total"`.
#' @param digits Optional integer. Number of digits for rounding.
#'   Default `NULL` (no rounding).
#' @param return_part Optional. `"products"`, `"industries"`, or
#'   `"primary_inputs"` to select part of the matrix. Default `NULL`
#'   returns the full matrix.
#' @param remove_empty Logical. Should empty rows be removed?
#'   Defaults to `TRUE`.
#' @param households Logical. If `TRUE`, include the household column.
#'   Default `FALSE`.
#' @param ... Reserved for future extensions.
#'
#' @return
#' A `data.frame` containing:
#' - the key (row label) column from `data_table`
#' - numeric columns with input (technical) coefficients
#'
#' @references
#' Beutel, J. (2008). *Eurostat Manual of Supply, Use and Input–Output
#' Tables*. Luxembourg: Publications Office of the European Union, ch. 15.
#' United Nations et al. (2009). *System of National Accounts 2008*,
#' §§14.90–14.91.
#'
#' @family indicator functions
#'
#' @examples
#' cm <- coefficient_matrix_create(
#'   data_table = iotable_get(source = "germany_1995"),
#'   total = "output",
#'   digits = 4
#' )
#' head(cm)
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

  key_column <- as.character(unlist(data_table[, 1]))

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
