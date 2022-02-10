#' Return the position of final household expenditure
#'  
#' @param data_table A symmetric input output table, a use table or a 
#' supply table.
#' @return An integer value with the final household expenditure. Returns 
#' \code{NULL} if not found.
#' @examples 
#' household_column_find( iotable_get ( source = 'germany_1995') )
#' @export

household_column_find <- function(data_table) {
  household_column <- NULL
  if (  any(c('households', 'p3_s14', 'final_consumption_households',
              'final_consumption_household',
              'consumption_expenditure_household', 'consumption_expenditure_households') %in%
               tolower(names(data_table))) ) {
       household_column <- which(tolower(names(data_table)) %in% 
                              c('households', 'p3_s14', 'final_consumption_households', 'final_consumption_household',
                                'consumption_expenditure_household', 'consumption_expenditure_households'))
     } else if ( any (grepl('households', tolower(names(data_table))))) { 
       household_column <- which(grepl('households', tolower(names(data_table))))
       }
  household_column
}
