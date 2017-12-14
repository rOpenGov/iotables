#' Add employment data to IO table
#' 
#' This function adds well-formatted employment data to the IO table from another source. 
#' @param source A data source, for example "naio_10_cp1700".
#' @param io_table An io table.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename left_join arrange mutate_if
#' @importFrom tidyr spread
#' @importFrom forcats fct_reorder
#' @examples 
#' hr_io_1800 <- iotable_get ( source = "croatia_2010_1800", geo = "HR",
#'                             year = 2010, unit = "T_NAC", labelling  = "iotables")
#' hr_io_1800_emp <- employment_add (source = "croatia_2010_1800", io_table = hr_io_1800 )
#' @export 

employment_add <- function ( source = "croatia_2010_1800", io_table = hr_io_1800  ) {
  employment_hr <- NULL
  croatia_files <- c( "croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")
  if ( source %in%  croatia_files) {
    labour_inputs <- iotables::employment_hr
  } else   { stop("No other employment data is available. ") }
  labour_inputs <- io_table [1,]
  labour_inputs[1:length(labour_inputs)] <- NA
  labour_inputs[2:(length(employment_hr$employment)+1)] <- employment_hr$employment
  if ( names (io_table)[2] == 'agriculture') {
    labour_inputs[1,1] <- "labour_inputs"
  } else if ( names (io_table)[2] == "A01" ) {
    labour_inputs[1,1] <- "EMP"  }
  
  return(rbind(io_table, labour_inputs))
}
