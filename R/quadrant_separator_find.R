#' Determine the end of Quadrant I and III.
#' 
#' This is an internal function to determine where to separate quadrants if
#' necessary.
#' @param data_table A symmetric input output table, a use table or a supply 
#' table.
#' @return An integer value with the last column of Quadrant I and III. If
#' the last column is not found, \code{2} is returned with a warning to avoid
#' stopping a pipeline.
#' @examples 
#' quadrant_separator_find( iotable_get ( source = "germany_1990") )
#' @export

quadrant_separator_find <- function(data_table, 
                                    households = FALSE) {
  last_column <- 2
   if ( households == TRUE ) { 
     if (  any(c("households", "p13_s14", "final_consumption_households") %in%
               tolower(names(data_table))) ) {
       last_column <- which(tolower(names(data_table)) %in% 
                              c("households", "p13_s14", "final_consumption_households"))
     } else if ( any (grepl("households", tolower(names(data_table))))) { 
       last_column <- which(grepl("households", tolower(names(data_table))))
       }
   } else { 
     if ( any(c("total", "cpa_total") %in% tolower(names(data_table))) ) {
       last_column <- which(tolower(names(data_table))  %in% c("total", "cpa_total") )
     } else if ( any(c("households", "p13_s14") %in% tolower(names(data_table)))) {
       last_column <- which(tolower(names(data_table)) %in% c("households", "p13_s14")-1 )
     } else  if ( 'cpa_u' %in% tolower ( names (data_table)) ) { 
       last_column <- which(tolower(names(data_table)) =='cpa_u')
     } else if ( 'cpa_t' %in% tolower (names(data_table))) { 
       last_column <- which(tolower(names(data_table)) =='cpa_t')
     } else if ( 'cpa_s96' %in% tolower (names(data_table))) { 
       last_column <- which(tolower(names(data_table)) =='cpa_s96')
     } else if ( 'other_services_group' %in% tolower (names(data_table))) { 
       last_column <- which(tolower(names(data_table)) =='other_services_group')
     }
      }
  
  if ( last_column == 2) {
    warning ( "The last column was not found")
  }
  last_column
}
