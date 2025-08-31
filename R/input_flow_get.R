#' Create an inter-industry (intermediate-use) matrix
#'
#' @description
#' Return the Quadrant I block (intermediate consumption) of a symmetric
#' input–output table. Optionally append the final household consumption
#' column for Type-II modelling.
#'
#' @details
#' In the Eurostat framework, the Quadrant I block shows *intermediate
#' consumption* by industry (columns) and product (rows), valued at
#' purchasers’ prices. Final household consumption belongs to the final
#' uses block (not Quadrant I); when `households = TRUE`, that column is
#' appended for convenience in Type-II analyses that endogenise private
#' consumption. See the *Eurostat Manual of Supply, Use and
#' Input-Output Tables* for the quadrant layout and definitions.
#'
#' @param data_table A symmetric input–output table (product-by-product
#'   or industry-by-industry) obtained via [iotable_get()].
#' @param households Logical. If `TRUE`, append the
#'   `final_consumption_households` column. Default `TRUE`.
#' @param empty_remove Logical. Reserved; currently ignored (no effect).
#'   Default `FALSE`.
#'
#' @return
#' A data frame with the key column and the Quadrant I block; if
#' `households = TRUE`, the household final consumption column is
#' appended.
#'
#' @seealso [input_coefficient_matrix_create()],
#'   [leontief_inverse_create()]
#'
#' @family analytic object functions
#'
#' @examples
#' # Basic extraction (Quadrant I + households column)
#' x <- input_flow_get(
#'   data_table   = iotable_get(),
#'   empty_remove = FALSE,
#'   households   = TRUE
#' )
#'
#' # Quadrant I only (no households column)
#' y <- input_flow_get(
#'   data_table   = iotable_get(),
#'   empty_remove = FALSE,
#'   households   = FALSE
#' )
#'
#' @importFrom dplyr mutate across left_join
#' @importFrom tidyselect where
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
    quadrant <- data_table[, seq_len(last_column)]
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
