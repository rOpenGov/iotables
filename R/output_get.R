#' Get the output (P1) vector
#'
#' @description
#' Convenience wrapper around [primary_input_get()] that returns the row
#' labelled **Output (P1)** from a symmetric input–output table (SIOT) or
#' from a use table retrieved by [iotable_get()].
#'
#' @details
#' In the Eurostat framework, **Output** is transaction **P1**, usually
#' recorded at **basic prices** (often labelled "output" or "output_bp").
#' It is a balancing item of the use table / SIOT, **not** a “primary
#' input” (primary inputs are value added components and imports, shown
#' in the third quadrant). This helper merely selects the row labelled
#' `"output"`, `"output_bp"`, `"P1"` or `"p1"` if present.
#'
#' @param data_table A symmetric input–output table or use table
#'   retrieved by [iotable_get()].
#'
#' @return
#' A one-row data frame: the first column is the key column; remaining
#' columns give output (P1) by product/industry.
#'
#' @seealso [primary_input_get()], [iotable_get()]
#' @family iotables processing functions
#'
#' @examples
#' # Output (P1) from the package demo table
#' iot_germany <- iotable_get()
#' output_get(data_table = iot_germany)
#'
#' @export


output_get <- function(data_table) {
  key_column <- as.character(unlist(data_table[, 1]))

  possible_names <- c("output", "output_bp", "p1", "P1")

  primary_input_get(
    data_table = data_table,
    primary_input = possible_names[
      possible_names %in% key_column
    ]
  )
}
