#' Find Household Expenditure Column
#'
#' Identify the column position corresponding to final household expenditure in
#' a symmetric input–output table or related table.
#'
#' @param data_table A symmetric input–output table, a use table, or a supply
#'   table.
#'
#' @return An integer vector giving the position(s) of household expenditure
#'   columns. Returns `NULL` if none are found.
#'
#' @details
#' The function searches column names case-insensitively. It first looks for
#' exact matches among the following alternatives:
#'
#' * `"households"`
#' * `"p3_s14"`
#' * `"final_consumption_households"`
#' * `"final_consumption_household"`
#' * `"consumption_expenditure_household"`
#' * `"consumption_expenditure_households"`
#'
#' If none of these are found, it falls back to any column name that contains
#' `"households"`.
#'
#' @examples
#' # German SIOT includes a household final consumption column
#' household_column_find(iotable_get(source = "germany_1995"))
#'
#' # Custom example
#' df <- data.frame(
#'   sector = c("A", "B"),
#'   households = c(100, 200)
#' )
#' household_column_find(df)
#'
#' @family iotables processing functions
#' @export


household_column_find <- function(data_table) {
  household_column <- NULL
  if (any(c(
    "households", "p3_s14", "final_consumption_households",
    "final_consumption_household",
    "consumption_expenditure_household", "consumption_expenditure_households"
  ) %in%
    tolower(names(data_table)))) {
    household_column <- which(tolower(names(data_table)) %in%
      c(
        "households", "p3_s14", "final_consumption_households", "final_consumption_household",
        "consumption_expenditure_household", "consumption_expenditure_households"
      ))
  } else if (any(grepl("households", tolower(names(data_table))))) {
    household_column <- which(grepl("households", tolower(names(data_table))))
  }
  household_column
}
