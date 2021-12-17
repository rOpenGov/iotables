#' Return Final Household Expenditure
#'  
#' @param data_table A symmetric input output table, a use table or
#' a supply table.
#' @return The column containing final household expenditure. If not found
#' \code{NULL} is returned.
#' @importFrom dplyr mutate across
#' @family iotables processing functions
#' @examples 
#' household_column_get(iotable_get (source = 'germany_1990'))
#' @export

household_column_get <- function(data_table) {
 
 household_column <- household_column_find(data_table)
 fn_na_to_null <- function(x) ifelse (is.na(x),0,x)
 
 if ( ! is.null( household_column) ) {
    data_table %>% 
       select ( 1, household_column ) %>%
       mutate ( across (2, fn_na_to_null) )
 } else { return(NULL) }
 
}
