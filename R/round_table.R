#' Systematically round numeric values in a table
#'
#' @description
#' Utility function to round all numeric columns in an input-output style
#' table. It is mainly intended for reproducibility and comparability with
#' external sources that report rounded values. Non-numeric columns are left
#' unchanged.
#'
#' Special cases:
#' - If `digits = NULL` (default), the function returns the input unchanged
#'   (no rounding).
#' - Values exactly equal to `1e-06` are preserved to avoid suppressing small
#'   "epsilon" entries that occur in published IOTs and SUTs.
#'
#' @param data_table A symmetric input–output table, a use table, a supply
#'   table, or a margins/taxes table. Must be a data frame or tibble.
#' @param digits Integer scalar giving the number of digits for rounding.
#'   If `NULL`, no rounding is performed. If not numeric, a warning is
#'   issued and the table is returned unchanged.
#'
#' @return A tibble (if input was a tibble) or data frame with numeric
#'   columns rounded according to `digits`. Non-numeric columns are
#'   unchanged. If `digits = NULL` or invalid, the table is returned
#'   unchanged.
#'
#' @details
#' Rounding conventions in published tables differ across sources:
#' - Eurostat (2008, *Manual of Supply, Use and Input-Output Tables*),
#'   presents benchmark IOTs rounded to integers (millions of EUR).
#' - UN (2018, *Handbook on SUTs and IOTs*), notes that examples may not
#'   sum exactly because of rounding (p. 15).
#'
#' This function allows the user to replicate such rounded presentations
#' while keeping analytic pipelines consistent. Internally, rounding should
#' be used with care: repeated rounding in intermediate steps may accumulate
#' error. For modelling, keep `digits = NULL` and apply rounding only when
#' reproducing published sources.
#'
#' @importFrom dplyr mutate across where if_else
#' @keywords internal

round_table <- function(data_table, digits = NULL) {
  # No rounding requested
  if (is.null(digits)) {
    return(data_table)
  }

  # Validate digits
  if (!is.numeric(digits) || length(digits) != 1L || !is.finite(digits)) {
    warning("Error: rounding digits must be numeric (finite scalar). No rounding took place.")
    return(data_table)
  }

  round_eps <- function(x, digits) {
    dplyr::if_else(is.na(x) | x == 1e-06, x, round(x, digits))
  }

  dplyr::mutate(
    data_table,
    dplyr::across(dplyr::where(is.numeric), ~ round_eps(.x, digits))
  )
}
