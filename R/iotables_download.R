#' Download input-output tables
#'
#' This function downloads standard input-output table files. Currently only Eurostat files are supported.
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
#' The data is downloaded in the tempdir() under the name the statistical product as an
#' rds file. (For example: naio_10_cp1750.rds)
#' The temporary directory is emptied at every normal R sesssion exit.
#' @param source Currently only source = "eurostat" works. Later OECD Stan will be added.
#' @param stk_flow Defaults to "TOTAL". Possible values are "DOM", "IMP", "TOTAL". In tables
#' where no distinction is made it is not needed.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join rename
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom stats setNames
#' @examples
#' \dontrun{
#'  io_tables <- iotables_download ( source = "naio_cp17_r2" )
#'  }
#' @export

iotables_download <- function ( source = "naio_cp17_r2", stk_flow = "DOM" ) {
  t_cols2_lab = NULL; t_rows2_lab = NULL; values_lab = NULL
  . = NULL; downloaded <- NULL; downloaded_labelled <- NULL

  possible_sources_60 <- c( "naio_cp17_r2", "naio_10_cp1700", "naio_10_cp1750", 
                         "naio_10_pyp1700", "naio_10_pyp1750", 
                         "naio_17_agg_60_r2", "naio_18_agg_60_r2", 
                         "naio_19_agg_60_r2")
  source <- tolower (source)
  if ( ! source %in%  possible_sources_60 ) {
    supported_tables <- paste( possible_sources_60, collapse = ", ")
    stop (source, " is not in supported tables [", supported_tables, "]") 
  }
  
  retrieve_from_temp_bulk <-paste0(tempdir(),
     "\\eurostat/", source, "_date_code_TF.rds" )

  #only download the Eurostat bulk file if necessary.
  if(!file.exists(retrieve_from_temp_bulk)){
    downloaded <- tryCatch(eurostat::get_eurostat (source),
                    error=function(e) message ("No data was found with this identifier."))
  } else {
    warning ('The bulk Eurostat file is retrieved from the temporary directory.')
    downloaded <- readRDS( retrieve_from_temp_bulk )
  }
  
  #label the raw Eurostat file, add rename variables with _lab suffix
  downloaded_labelled <- downloaded  %>%
    eurostat::label_eurostat (., fix_duplicated = TRUE) %>%          #add meaningful labels to raw data
    setNames( ., paste0( names (.), "_lab" ))    %>%  
    dplyr::mutate ( rows = 1:nrow(.)) %>%  #because long and wide formats are not symmetric
    dplyr::rename ( values = values_lab ) 
  
  #join the labelled and the not labelled files, so that both versions are avialable
  
  downloaded <- downloaded  %>%
    dplyr::mutate ( rows = 1:nrow(.)) %>%
    dplyr::left_join (., downloaded_labelled, by = c("rows", "values")) 
  message ("Joined labelled and not labelled data.")
  
  if ( "stk_flow" %in% names ( downloaded )) {
    downloaded <- downloaded %>%
      dplyr::filter ( stk_flow == stk_flow )
    message ("Type" , stk_flow, " is returned.")
  }
  
   return( downloaded )
}
