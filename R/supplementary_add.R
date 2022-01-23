#' @title Add Supplementary Data
#'
#' @description Add supplementary data to a SIOT, a use, supply or margins table.
#' @details This function is a wrapper around the more general \code{\link{rows_add}} function.
#' @param data_table A SIOT, a use table, a supply table, or a margins 
#' table.  
#' @param supplementary_data Supplementary data to be added. 
#' It must be a data.frame
#' or tibble with a key column containing the indicator's name, 
#' and the column names must match with the \code{data_table}. Can be a 
#' vector or a data frame of several rows. 
#' @param supplementary_names Optional names for the new supplementary rows. 
#' Defaults  to \code{NULL}.
#' @return An extended \code{data_table} with the new row(s) binded.
#' @importFrom dplyr bind_cols
#' @family iotables processing functions
#' @return A symmetric input-output table with supplementary data,  
#' of data.frame class. 
#' The column names are ordered, and the row names are in the 
#' first, auxiliary metadata column.
#' @examples
#' de_io <- iotable_get()
#' CO2_coefficients <- data.frame(agriculture_group = 0.2379,
#'                                industry_group    = 0.5172, 
#'                                construction = 0.0456,
#'                                trade_group = 0.1320, 
#'                                business_services_group = 0.0127,
#'                                other_services_group = 0.0530)
#' CH4_coefficients <- data.frame(agriculture_group = 0.0349,
#'                                industry_group    = 0.0011, 
#'                                construction = 0,
#'                                trade_group = 0, 
#'                                business_services_group = 0,
#'                                other_services_group = 0.0021)
#' CO2 <- cbind (data.frame(iotables_row = "CO2"), 
#'               CO2_coefficients)
#' CH4 <- cbind(data.frame (iotables_row = "CH4_coefficients"),
#'              CH4_coefficients)
       
#' de_coeff <- input_coefficient_matrix_create ( iotable_get() )
#' emissions <- rbind (CO2, CH4)
#' 
#' # Check with the Eurostat Manual page 494:
#' supplementary_add(de_io, emissions)
#' @export

supplementary_add <- function ( data_table, 
                                supplementary_data, 
                                supplementary_names = NULL) {
  
  if ( !is.null(supplementary_names)) {
    if ( length(supplementary_names) != 
           nrow(as.data.frame(supplementary_data))) {
      stop("New names do not match the dimensions of the supplementary data.")
    }
  }
    
  if ( ! is_key_column_present(supplementary_data)) {
    key_column <- key_column_create(names(data_table)[1], 
                                    ifelse (is.null(supplementary_names), 
                                            yes =  paste0("supplementary_row_", 1:nrow(supplementary_data)), 
                                            no = supplementary_names)
    )
    
    supplementary_data <- bind_cols(
      key_column, supplementary_data
    )
  } else {
    key_column <- supplementary_data[,1]
    names(key_column) <- names (data_table)[1]
    }
  
  siot_ext <- rows_add(data_table, supplementary_data)
  
  if ( any(c("final_consumption_households", "p3_s14") %in% tolower ( names (siot_ext)))  ) {
    household_col <- which ( tolower ( names ( siot_ext)) %in% c("final_consumption_households", "p3_s14") )
    new_rows <- which ( tolower ( as.character(siot_ext[,1])) %in% key_column )
    siot_ext[new_rows, household_col] <- ifelse ( is.na(siot_ext[new_rows, household_col]), 0, siot_ext[new_row, household_col])
  }
  
 siot_ext
}
