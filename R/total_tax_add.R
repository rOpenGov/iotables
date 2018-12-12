#' Summarize and add tax data
#'
#' @param data_table A SIOT, a use table, a supply table, or a margins table that 
#' has product and production tax rows in among the primary inputs.
#' @param tax_names Defaults to \code{("d21x31", "d29x39")}, which are the Eurostat 
#' names for taxes. The parameter is not case sensitive.
#' @param total_tax_name Defaults to \code{'TOTAL_TAX'}. The name of the 
#' summarized row. It is case sensitive.
#' @importFrom dplyr select full_join summarise_if mutate_if
#' @examples
#' de_io <- iotable_get ()
#' 
#' total_tax_add ( de_io, 
#'                 tax_names = c("net_tax_products",  "net_tax_production"),
#'                 total_tax_name = "total_tax")
#' @export

total_tax_add <- function ( data_table, 
                            tax_names = c("d21x31", "d29x39"), 
                            total_tax_name = "TOTAL_TAX") {
  
  if(is.null(tax_names)) { 
    stop ( 'Tax names must be set.') 
    } 
  
  key_column <- tolower(as.character(unlist(data_table[,1])) )
  
  if (! all ( tax_names %in% key_column )) {
    stop ( "The tax names ", 
           paste(tax_names, collapse= ', '), 
           ' (not case sensitive) were not found in the data table.')
  }
  
  tax <- data_table [which (key_column %in% tax_names), ]
  
  tax <- dplyr::summarise_if (tax, is.numeric, sum ) %>%
    cbind ( data_table[1,1], .) %>%
    dplyr::mutate_if ( is.factor, as.character)
  
  tax [ 1,1 ] <- total_tax_name
  
  names ( tax)[1] <- names (data_table)[1]
  
  siot_ext   <- dplyr::full_join ( 
    dplyr::mutate_if ( data_table, is.factor, as.character), tax,
    by = names (tax) )
  
  if ( any(c("final_consumption_households", "p3_s14") %in% tolower ( names ( siot_ext)))  ) {
    household_col <- which ( tolower ( names ( siot_ext)) %in% c("final_consumption_households", "p3_s14") )
    new_row <- which ( tolower ( as.character(siot_ext[,1])) %in% tolower(new_key) )
    
    siot_ext[new_row, household_col] <- ifelse ( test = is.na(siot_ext[new_row, household_col]), 
                                                 yes  = 0, 
                                                 no   = siot_ext[new_row, household_col]) 
    } #end of households case 
  
  siot_ext  
}
