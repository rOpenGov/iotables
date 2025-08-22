#' Remove Empty Rows and Columns Symmetrically
#'
#' Remove columns and their corresponding rows if they contain only zeros or
#' missing values. This ensures that the resulting table remains symmetric (same
#' dimensions in rows and columns).
#'
#' @param data_table A symmetric input–output table, or the symmetric quadrant
#'   of a use or supply table.
#'
#' @return A `data.frame` (or tibble) with a key column and symmetric matrix,
#'   after removing all-zero (or all-missing) columns and their corresponding
#'   rows.
#'
#' @details The function first identifies columns that contain only zeros or
#'   missing values, then removes both those columns and the rows with matching
#'   labels in the first (key) column. A message is printed listing the removed
#'   columns.
#'
#' @examples
#' # Example using the built-in demo table
#' test_table <- input_coefficient_matrix_create(
#'   iotable_get(source = "germany_1995")
#' )
#'
#' # Set one column to zero, then remove it
#' test_table[, 2] <- 0
#' empty_remove(test_table)
#'
#' @family iotables processing functions
#' @export

empty_remove <- function(data_table) {
  ### Find non-zero cols and rows and remove them----
  non_zero_cols <- vapply(
    data_table[, seq_len(ncol(data_table))],
    non_zero_columns_find, logical(1)
  )
  non_zero_rows <- as.logical(non_zero_cols[-1])

  remove_cols <- names(data_table)[which(is.na(non_zero_cols) | non_zero_cols == FALSE)]

  siot_rows <- as.character(unlist(data_table[, 1]))

  if (length(remove_cols) > 0) {
    message("Columns and rows of ", paste(remove_cols, collapse = ", "), " are all zeros and will be removed.")
  }

  data_table[
    !siot_rows %in% remove_cols,
    !names(data_table) %in% remove_cols
  ]
}

#' Find Non-zero Columns
#'
#' Internal helper to detect empty columns (or rows) in symmetric input–output
#' style tables.
#'
#' @param data_table A column (vector) from a symmetric input–output table,
#'   a use table, or a supply table. May also be a factor or character vector.
#'
#' @return A logical value: `TRUE` if the column contains at least one non-zero
#'   numeric entry, or if the input is a factor/character column; `FALSE`
#'   otherwise.
#'
#' @keywords internal

non_zero_columns_find <- function(data_table) {
  if (class(data_table) %in% c("factor", "character")) {
    return(TRUE)
  }
  ifelse(all(as.numeric(unlist(data_table)) == 0), FALSE, TRUE)
}
