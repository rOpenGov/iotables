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
#' determine_end_quadrant( iotable_get ( source = "germany_1990") )
#' @export

determine_end_quadrant <- function(data_table) {
  last_column <- 2
  if ( any(c("total", "cpa_total") %in% tolower(names(data_table))) ) {
    last_column <- which(tolower(names(data_table))  %in% c("total", "cpa_total") )
  } else if ( any(c("households", "p13_s14") %in% tolower(names(data_table)))) {
    last_column <- which(tolower(names(data_table)) %in% c("total", "cpa_total")-1 )
  } else  if ( 'cpa_u' %in% tolower ( names (data_table)) ) { 
    last_column <- which(tolower(names(data_table)) =='cpa_u')-1
  } else if ( 'cpa_t' %in% tolower (names(data_table))) { 
    last_column <- which(tolower(names(data_table)) =='cpa_t')-1
  } else if ( 'cpa_s96' %in% tolower (names(data_table))) { 
    last_column <- which(tolower(names(data_table)) =='cpa_s96')-1
  } else if ( 'other_services_group' %in% tolower (names(data_table))) { 
    last_column <- which(tolower(names(data_table)) =='other_services_group')-1
  }
  
  if ( last_column == 2) {
    warning ( "The last column was not found")
  }
  last_column
}
