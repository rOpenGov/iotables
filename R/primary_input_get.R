#' Get a primary input row
#'
#' Retrieve a named primary-input row from a symmetric input–output table,
#' a use table, or a supply table (as returned by [iotable_get()]).
#'
#' @details
#' In I–O accounting, *primary inputs* (e.g., compensation of employees,
#' consumption of fixed capital, taxes on production/subsidies, operating
#' surplus/mixed income, and—when relevant—imports used for domestic
#' production) are shown in the value-added block (third quadrant).
#'
#' @param data_table A symmetric I–O table, use table, or supply table as
#'   returned by [iotable_get()].
#' @param primary_input Character. The primary input to return. Accepts common
#'   synonyms (e.g., "compensation of employees", "cfc", "taxes on production",
#'   "operating surplus", "imports").
#'
#' @importFrom dplyr select mutate across where any_of
#' @return A data frame containing the key column and the matching primary-
#'   input row.
#'
#' @references
#' Eurostat (2008). *Eurostat Manual of Supply, Use and Input–Output Tables*,
#' ch. 13.
#' United Nations (2018). *Handbook on Supply and Use Tables and Input–Output
#' Tables with Extensions and Applications (Rev. 1, “white cover”)*, ch. 10.
#'
#' @family iotables processing functions
#'
#' @examples
#' # Get the Germany 1995 demo SIOT with default labelling
#' de_iot <- iotable_get(source = "germany_1995")
#'
#' # Select compensation of employees (row code: "compensation_employees")
#' primary_input_get(de_iot, "compensation_employees")
#'
#' # Get the same table with Eurostat short labelling
#' de_iot_short <- iotable_get(source = "germany_1995", labelling = "short")
#'
#' # Consumption of fixed capital (row code: "K1")
#' primary_input_get(de_iot_short, "K1")
#'
#' # Operating surplus and mixed income, net (row code: "B2A3N")
#' primary_input_get(de_iot_short, "B2A3N")
#' @export
primary_input_get <- function(data_table,
                              primary_input = "compensation_employees") {
  if (is.null(data_table)) {
    stop("No input-output table was given as input.")
  }
  if (!is.data.frame(data_table)) {
    stop("`data_table` must be a data.frame (or tibble).")
  }
  if (!is.character(primary_input) || length(primary_input) != 1L) {
    stop("`primary_input` must be a single character string.")
  }

  data_table <- dplyr::mutate(
    data_table,
    dplyr::across(dplyr::where(is.factor), as.character)
  )

  # Select all until the last column in the quadrant
  last_column <- quadrant_separator_find(data_table)
  if (!is.numeric(last_column) || length(last_column) != 1L ||
    is.na(last_column) || last_column < 2L ||
    last_column > ncol(data_table)) {
    stop("Quadrant separator is invalid for this table.")
  }

  # Limit to the economic block (by position, preserving current behavior)
  data_table <- data_table[, seq_len(last_column), drop = FALSE]

  # First column is the label/key column
  labels <- data_table[[1L]]

  hits <- which(labels == primary_input)
  if (length(hits) == 0L) {
    stop("The requested primary input was not found in the first column.")
  }
  if (length(hits) > 1L) {
    stop("Multiple rows match `primary_input`; labels must be unique.")
  }

  data_table[hits, , drop = FALSE]
}
