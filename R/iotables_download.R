#' Download input-output tables
#'
#' This function downloads standard input-output table files. Currently only Eurostat files are supported.
#' You are not likely to use this function, because \code{\link{iotable_get}} will
#' call this function if necessary and properly filter out an input-output table.
#' The only parameter is the Eurostat code of the table.
#' The data is downloaded in the \code{tempdir()}under the name the statistical product as an
#' rds file. (For example: \code{naio_10_cp1750.rds})
#' The temporary directory is emptied at every normal R session exit.
#' To save the file for further use (which is necessary in analytical work because
#' download times are long) set the  \code{download_directory} [see parameters]. 
#' The function will make a copy of the rds file in this directory.
#'  \itemize{
##'  \item{\code{naio_10_cp1700}}{ Symmetric input-output table at basic prices (product by product)}
##'  \item{\code{naio_10_pyp1700}}{ Symmetric input-output table at basic prices (product by product) (previous years prices)}
##'  \item{\code{naio_10_cp1750}}{ Symmetric input-output table at basic prices (industry by industry)}
##'  \item{\code{naio_10_pyp1750}}{ Symmetric input-output table at basic prices (industry by industry) (previous years prices) }
##'  \item{\code{naio_10_cp1620}}{ Table of trade and transport margins at basic prices}
##'  \item{\code{naio_10_pyp1620}}{ Table of trade and transport margins at previous years' prices}
##'  \item{\code{naio_10_cp1630}}{ Table of taxes less subsidies on products at basic prices}
##'  \item{\code{naio_10_pyp1630}}{Table of taxes less subsidies on products at previous years' prices}
##' } 
#' @param source See the available list of sources above in the Description. 
#' @param data_directory Defaults to \code{NULL}, if a valid directory, it will try to save the pre-processed 
#' data file here with labelling. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing downloaded file
#' in the \code{data_directory} or the temporary directory, if it exists.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join rename group_by
#' @importFrom tidyr nest
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom stats setNames
#' @importFrom lubridate year
#' @examples
#' \dontrun{
#'  io_tables <- iotables_download ( source = "naio_10_cp1700" )
#'  }
#' @export

iotables_download <- function ( source = "naio_10_cp1700", 
                                data_directory = NULL,
                                force_download = TRUE ) {
  t_cols2_lab <- t_rows2_lab <- values_lab <- stk_flow <- NULL
  . <- downloaded <- downloaded_labelled <- fix_duplicated <- NULL
  time_lab <- geo <- geo_lab <- time <- unit <- unit_lab <- NULL
  
  possible_download_sources <- c( "naio_10_cp1700", "naio_10_cp1750", 
                                  "naio_10_pyp1700", "naio_10_pyp1750",
                                  "naio_10_cp1620", "naio_10_pyp1620", 
                                  "naio_10_cp1630", "naio_10_pyp1630" )
  source <- tolower (source)
  if ( ! source %in%  possible_download_sources ) {
    supported_tables <- paste( possible_download_sources, collapse = ", ")
    stop (source, " is not in supported tables [", supported_tables, "]") 
  }
  
  retrieve_from_temp_bulk <-paste0(tempdir(),
                                   "\\eurostat/", source, "_date_code_TF.rds" )
  
  #downloaded <- readRDS("C:/Users/Daniel Antal/OneDrive - Visegrad Investments/2017 Projektek/iotables/data-raw/naio_cp17_r2.rds")
  #only download the Eurostat bulk file if necessary.
  
  if ( !is.null (data_directory)   ) {
    save_file_name <- paste0(data_directory, "/", source, ".rds")
    if ( ! force_download && file.exists(save_file_name) )
      return(readRDS(save_file_name))
  }
  
  if (!file.exists(retrieve_from_temp_bulk) | force_download == TRUE){
    downloaded <- tryCatch(eurostat::get_eurostat (source),
                           error=function(e) message ("No data was found with this identifier."))
  } else {
    message ('The bulk Eurostat file is retrieved from the temporary directory.')
    downloaded <- readRDS( retrieve_from_temp_bulk )
  }
  
  #label the raw Eurostat file, add rename variables with _lab suffix
  downloaded_labelled <- downloaded  %>%
    eurostat::label_eurostat (., fix_duplicated = TRUE) %>%   #add meaningful labels to raw data
    stats::setNames( ., paste0( names (.), "_lab" ) )    %>%  
    dplyr::mutate ( rows = 1:nrow(.) ) %>%  #because long and wide formats are not symmetric
    dplyr::rename ( values = values_lab ) %>%
    dplyr::mutate ( year = lubridate::year( time_lab ))
  
  #join the labelled and the not labelled files, so that both versions are avialable
  
  downloaded <- downloaded  %>%
    dplyr::mutate ( rows = 1:nrow(.)) %>%
    dplyr::left_join (., downloaded_labelled, by = c("rows", "values"))
  #message ("Joined labelled and not labelled data.")
  
  if ( "stk_flow" %in% names ( downloaded )) {
    downloaded <- downloaded %>%
      dplyr::filter ( stk_flow == stk_flow )
    #message ("Type " , stk_flow, " is returned.")
  }
  
  if ( source == "naio_cp17_r2" ){
    
    downloaded$t_cols2 <- plyr::mapvalues(
      downloaded$t_cols2, 
      from = c("CPA_N80-N82", "CPA_R90-R92",  "CPA_E37-E39",
               "CPA_C10-C12", "CPA_C13-C15", 
               "CPA_C31_C32", "CPA_J59_J60", 
               "CPA_J62_J63", "CPA_M69_M70", "CPA_Q87_Q88", 
               "CPA_M74_M75" , "CPA_O84", "CPA_P85", 
               "CPA_D35" ), 
      to = c("CPA_N80-82", "CPA_R90-92", "CPA_E37-39", 
             "CPA_C10-12", "CPA_C13-15", 
             "CPA_C31_32", "CPA_J59_60", 
             "CPA_J62_63", "CPA_M69_70", "CPA_Q87_88", 
             "CPA_M74_75", "CPA_O", "CPA_P", "CPA_D"
      ))
    
    downloaded$t_rows2 <- plyr::mapvalues(
      downloaded$t_rows2, 
      from = c("CPA_N80-N82", "CPA_R90-R92",  "CPA_E37-E39", 
               "CPA_C10-C12", "CPA_C13-C15", 
               "CPA_C31_C32", "CPA_J59_J60", 
               "CPA_J62_J63", "CPA_M69_M70", "CPA_Q87_Q88", 
               "CPA_M74_M75", "CPA_O84", "CPA_P85", "CPA_D35"), 
      to = c("CPA_N80-82", "CPA_R90-92", "CPA_E37-39", 
             "CPA_C10-12", "CPA_C13-15", 
             "CPA_C31_32", "CPA_J59_60", 
             "CPA_J62_63", "CPA_M69_70", "CPA_Q87_88", 
             "CPA_M74_75", "CPA_O", "CPA_P", "CPA_D")
    )
  } #end of _r2 
  
  
  if ( "stk_flow" %in% names ( downloaded ) ) {
    downloaded_nested <- tidyr::nest ( dplyr::group_by ( downloaded,
                                                         geo, geo_lab,
                                                         time, time_lab, year, 
                                                         unit, unit_lab, 
                                                         stk_flow, stk_flow_lab) )
    
  } else { 
    downloaded_nested <- tidyr::nest ( dplyr::group_by ( downloaded,
                                                         geo, geo_lab,
                                                         time, time_lab, year, 
                                                         unit, unit_lab ) )
    
    }
  

  if( !is.null(data_directory) ) {
    
    save_file_name <- file.path(data_directory, paste0(source, ".rds"))
    saveRDS( downloaded_nested, file = save_file_name )
    message ( "Saved the raw data of this table tpye in ",
              save_file_name, "." )
  }
  
  downloaded_nested 
}
