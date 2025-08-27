#' Create an inter-industry (input flow) matrix
#'
#' @description
#' Select the first quadrant of a symmetric input–output table (inter-industry
#' flows). Optionally include the final household consumption column.
#'
#' @details
#' The first quadrant is also called the *input flow matrix*, *input
#' requirements matrix*, or *inter-industry matrix*. See the Eurostat *Manual
#' of Supply, Use and Input-Output Tables* for layout conventions (e.g., the
#' first quadrant and the position of final household consumption; cf. the
#' package tests referencing p. 461).
#'
#' @param data_table A symmetric input–output table or use table retrieved by
#'   [iotable_get()].
#' @param households Logical; if `TRUE`, add the final household consumption
#'   column to the returned table. Default `TRUE`.
#' @param empty_remove Logical; if `TRUE`, drop empty rows/columns to avoid
#'   downstream division-by-zero issues. Product/industry rows that are empty
#'   are always removed internally by analytic functions. Default `FALSE`.
#'
#' @return
#' A data frame containing the key column and the first-quadrant block; if
#' requested, the household column is appended.
#'
#' @family analytic object functions
#'
#' @examples
#' input_flow <- input_flow_get(
#'   data_table = iotable_get(),
#'   empty_remove = FALSE,
#'   households = TRUE
#' )
#'
#' @importFrom dplyr mutate across left_join select where
#' @export


input_flow_get <- function(data_table,
                           empty_remove = FALSE,
                           households = TRUE) {
  data_table <- mutate(data_table, across(where(is.factor), as.character))

  # Remove empty columns and rows
  if (empty_remove) siot <- empty_remove(data_table)

  last_column <- quadrant_separator_find(data_table)

  ## Adding households, if requested----------------------------------------
  if (households == TRUE) {
    household_column <- household_column_get(data_table)
    quadrant <- data_table[, 1:last_column]
    input_flow_table <- left_join(
      quadrant, household_column,
      by = names(quadrant)[1]
    )
  } else {
    input_flow_table <- select(data_table, 1:last_column)
  }

  key_column <- tolower(as.character(unlist(input_flow_table[, 1])))

  last_row <- which(key_column %in% c("total", "cpa_total"))

  input_flow_table[1:last_row, ]
}
