#' Add Supplementary Data
#'
#' Download the employment data for a country and arrange it to the 
#' 64x64 SIOTS. Currently works only with product x product tables. 
#' @param data_table A SIOT, a use table, a supply table, or a margins table.  
#' @param supplementary_data Supplementary data to be added. 
#' It must be a data.frame
#' or tibble with a key column containing the indicator's name, 
#' and the column names must match with the \code{data_table}. Can be a 
#' vector or a data frame of several rows. 
#' @param supplementary_names Optional names for the new supplementary rows. 
#' Defaults  to \code{NULL}.
#' @importFrom dplyr select full_join mutate_if
#' @return A symmetric input-output table with supplementary data,  
#' of data.frame class. 
#' The column names are ordered, and the row names are in the 
#' first, auxiliary metadata column.
#' @examples
#' de_io <- iotable_get()
#' CO2 <- c( 0.2379, 0.5172, 0.0456, 0.1320, 0.0127, 0.0530)  
#' names ( CO2) <- c("agriculture_group", "industry_group","construction",
#'                   "trade_group","business_services_group","other_services_group") 
#' CO2 <- cbind ( 
#'   data.frame ( iotables_row = "CO2"),as.data.frame ( t(CO2)))
#' de_coeff <- input_coefficient_matrix_create ( iotable_get() )
#'
#' supplementary_add ( de_io, CO2)
#' @export

supplementary_add <- function ( data_table, 
                                supplementary_data, 
                                supplementary_names = NULL) {
  
  if ( !is.null(supplementary_names)) {
    if ( length(supplementary_names) == nrow(
         as.data.frame(supplementary_data))
         ) {
      new_key <- supplementary_names
      supplementary_data[,1] <- new_key
    } else {
      warning("New names do not match the dimensions of the supplementary data.")
    }
  } else {
    new_key <- as.character(supplementary_data[,1])
  }
 
  
  if ( length(new_key) == 0) new_key <- 'supplementary_row'
  
  key_column <- dplyr::select ( data_table, 1 ) 
  
  names (supplementary_data)[1] <- names (data_table)[1]
  
  siot_ext   <- dplyr::full_join ( 
    dplyr::mutate_if(data_table, is.factor, as.character), 
    dplyr::mutate_if(supplementary_data, is.factor, as.character),
    by = names (supplementary_data) )

  
  if ( any(c("final_consumption_households", "p3_s14") %in% tolower ( names ( siot_ext)))  ) {
    household_col <- which ( tolower ( names ( siot_ext)) %in% c("final_consumption_households", "p3_s14") )
    new_row <- which ( tolower ( as.character(siot_ext[,1])) %in% tolower(new_key) )
    siot_ext[new_row, household_col] <- ifelse ( is.na(siot_ext[new_row, household_col]), 0, siot_ext[new_row, household_col])
  }
  
 siot_ext
}
