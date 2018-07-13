#' Download input-output tables
#'
#' This function downloads standard input-output table files. Currently only Eurostat files are supported.
#' You are not likely to use this function, because \code{\link{iotable_get}} will
#' call this function if necessary and properly filter out an input-output table.
#' The only parameter is the Eurostat code of the table: 
#' Symmetric input-output table - current prices (NACE Rev. 2) [naio_cp17_r2]
#' Symmetric input-output table at basic prices (product by product) (naio_10_cp1700)	
#' Symmetric input-output table at basic prices (industry by industry) (naio_10_cp1750)
#' Symmetric input-output table at basic prices (product by product) (previous years prices) (naio_10_pyp1700)
#' Symmetric input-output table at basic prices (industry by industry) (previous years prices) (naio_10_pyp1750)
#' 
#' EU-level tables
#' Input-output table for domestic output at current prices, 60 branches - EU aggregates (NACE Rev. 2) (naio_18_agg_60_r2)
#' Input-output table at current prices, 10 branches - EU aggregates (NACE Rev. 2) (naio_17_agg_10_r2)
#' Input-output table at current prices, 6 branches - EU aggregates (naio_17_agg_6)	 
#' 
#' At the moment import and domestic tables are not yet supported in the package.
#' The data is downloaded in the \code{tempdir()}under the name the statistical product as an
#' rds file. (For example: \code{naio_10_cp1750.rds})
#' The temporary directory is emptied at every normal R session exit.
#' @param source Currently only source = \code{eurostat} works. Later OECD Stan will be added.
#' @param data_directory Defaults to \code{NULL}, if a valid directory, it will try to save the pre-processed 
#' data file here with labelling. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing downloaded file
#' in the \code{data_directory} or the temporary directory, if it exists.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join rename
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#'  io_tables <- iotables_download ( source = "naio_cp17_r2" )
#'  }
#' @export

iotables_download <- function ( source = "naio_cp17_r2", 
                                data_directory = NULL,
                                force_download = TRUE ) {
  t_cols2_lab = NULL; t_rows2_lab = NULL; values_lab = NULL
  . = NULL; downloaded <- NULL; downloaded_labelled <- NULL

  possible_sources_60 <- c( "naio_10_cp1700", "naio_10_cp1750", 
                         "naio_10_pyp1700", "naio_10_pyp1750", "naio_10_cp1620",
                         "naio_17_agg_60_r2", "naio_18_agg_60_r2", "naio_10_cp1630",
                         "naio_19_agg_60_r2")
  source <- tolower (source)
  if ( ! source %in%  possible_sources_60 ) {
    supported_tables <- paste( possible_sources_60, collapse = ", ")
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
    eurostat::label_eurostat (., fix_duplicated = TRUE) %>%          #add meaningful labels to raw data
    stats::setNames( ., paste0( names (.), "_lab" ) )    %>%  
    dplyr::mutate ( rows = 1:nrow(.) ) %>%  #because long and wide formats are not symmetric
    dplyr::rename ( values = values_lab ) 
  
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
  
  if( !is.null(data_directory) ) {
    save_file_name <- paste0(data_directory, "/", source, ".rds")
    saveRDS( downloaded, file = save_file_name )
    message ( "Saved the table with stk_flow = ", 
              stk_flow, " in ", save_file_name )
  }
  downloaded
}
