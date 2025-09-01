#' @title Return Final Household Expenditure
#'
#' @description Extracts the column of final household expenditure from a
#' symmetric input-output table, a use table, or a supply table. If no
#' household expenditure column is detected, returns `NULL`.
#'
#' @param data_table A symmetric input-output table, a use table, or a
#' supply table.
#'
#' @return A tibble/data frame with the key column and the household
#' expenditure column. Returns `NULL` if no household column is found.
#'
#' @family iotables processing functions
#' @examples
#' household_column_get(iotable_get(source = "germany_1995"))
#' @export

household_column_get <- function(data_table) {
  household_column <- household_column_find(data_table)

  if (!is.null(household_column)) {
    data_table %>%
      dplyr::select(1, all_of(household_column)) %>%
      dplyr::mutate(across(2, fn_na_to_null))
  } else {
    NULL
  }
}
