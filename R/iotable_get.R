#' @title Get an input-output table from bulk file
#' 
#' @description This function is used to filter out  a single input-output table from
#' a database, for example a raw file downloaded from the Eurostat 
#' website. It provides some functionality to avoid some pitfalls.
#' 
#' @details Unless you want to work with bulk data files, 
#' you should not invoke  \code{\link{iotables_download}} 
#' directly, rather via this function, if and when it is necessary.
#'  
#' @param source A data source, for example \code{naio_10_cp1700}. 
#'  \itemize{
##'  \item{\code{naio_10_cp1700}}{ Symmetric input-output table at basic prices (product by product)}
##'  \item{\code{naio_10_pyp1700}}{ Symmetric input-output table at basic prices (product by product) (previous years prices)}
##'  \item{\code{naio_10_cp1750}}{ Symmetric input-output table at basic prices (industry by industry)}
##'  \item{\code{naio_10_pyp1750}}{ Symmetric input-output table at basic prices (industry by industry) (previous years prices) }
##'  \item{\code{naio_10_cp15}}{ Supply table at basic prices incl. transformation into purchasers' prices }
##'  \item{\code{naio_10_cp16}}{ Use table at purchasers' prices }
##'  \item{\code{naio_10_cp1610}}{ Use table at basic prices }
##'  \item{\code{naio_10_pyp1610}}{ Use table at basic prices (previous years prices) (naio_10_pyp1610) }
##'  \item{\code{naio_10_cp1620}}{ Table of trade and transport margins at basic prices}
##'  \item{\code{naio_10_pyp1620}}{ Table of trade and transport margins at previous years' prices}
##'  \item{\code{naio_10_cp1630}}{ Table of taxes less subsidies on products at basic prices}
##'  \item{\code{naio_10_pyp1630}}{ Table of taxes less subsidies on products at previous years' prices}
##' } 
#' For further information consult the 
#' \href{https://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/overview}{Eurostat Symmetric Input-Output Tables} page.
#' @param labelled_io_data If you have downloaded a bulk data file with 
#' \code{\link{iotables_download}}, it is faster to work with the data
#' in the memory. Defaults to \code{NULL} when  the data will be retrieved from
#' the hard disk or from the Eurostat website invoking the same function.
#' @param geo A country code or a country name.  
#' For example, \code{SK} or as \code{Slovakia}.
#' @param year A numeric variable containing the year. 
#' Defaults to \code{2010}, because this year has the most data. 
#' @param unit A character string containing the currency unit,
#' defaults to \code{MIO_NAC} (million national currency unit). 
#' The alternative is \code{MIO_EUR}. 
#' @param stk_flow Defaults to \code{DOM} as domestic output, 
#' alternative \code{IMP} for imports 
#' and \code{TOTAL} for total output. For \code{source = 'naio_10_cp1620'} and 
#' trade and transport margins and  \code{source = 'naio_10_cp1630'} taxes 
#' less subsidies only \code{TOTAL} is not used.
#' @param labelling Defaults to \code{iotables} which gives standard row 
#' and column names regardless of the source of the table, or if it is a 
#' product x product, industry x industry or product x industry table.
#' The alternative is \code{short} or \code{eurostat} which is the 
#' original short row or column code of Eurostat or OECD.
#' @param data_directory Defaults to \code{NULL} when the files will be temporarily stored
#' in the path retrieved by \code{\link[base]{tempdir}}. If it is a different valid directory, 
#' it will try to save the pre-processed data file here with labelling. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing 
#' downloaded file in the \code{data_directory} or the temporary 
#' directory, if it exists. Will force download only in a new session.
#' @return A wide format data.frame with a well-ordered input-output table.
#' The bulk data files on the Eurostat website are in a long form and they are 
#' not correctly ordered for further matrix equations.
#' @importFrom dplyr filter select mutate rename left_join arrange all_of
#' @importFrom tidyr pivot_wider
#' @importFrom forcats fct_reorder
#' @importFrom lubridate year
#' @importFrom utils data
#' @importFrom rlang .data
#' @importFrom utils globalVariables
#' @importFrom glue glue
#' @family import functions
#' @examples 
#' germany_table <- iotable_get( source = "germany_1990", 
#'                  geo = 'DE', year = 1990, unit = "MIO_EUR", 
#'                  labelling  = "iotables" )
#' @export 

iotable_get <- function (labelled_io_data = NULL, 
                         source = "germany_1990", 
                         geo = "DE",
                         year = 1990, 
                         unit = "MIO_EUR", 
                         stk_flow = "DOM", 
                         labelling = "iotables", 
                         data_directory = NULL, 
                         force_download = FALSE) { 
  
  uk_tables <- c("uk_2010_siot", "uk_2010_coeff", "uk_2010_inverse")
  croatia_files <- c('croatia_2010_1700', 'croatia_2010_1800', 
                     'croatia_2010_1900')
  internal_datasets <- c(croatia_files, "germany_1990")
  is_internal_dataset <- source %in% internal_datasets
  
  if ( source == "germany_1990" ) { 
    year <- year_input <- 1990
    geo <- geo_input <- "DE"
    unit <- unit_input <- "MIO_EUR"
    source_input <- source
    stk_flow_input <- "DOM"
  } else if ( source %in% uk_tables ) {
    labelling <-  'short'
    year <- year_input <- 2010 
    unit <- unit_input <- 'MIO_NAC'
    geo <- geo_input <-"UK"
    stk_flow <- stk_flow_input <- "TOTAL"
    source_input <- source
  } else if ( source %in% croatia_files ) {
    year <- year_input <- 2010
    geo <- geo_input <- "HR"
    unit <- unit_input <- "T_NAC"
    source_input <- source
    stk_flow <- stk_flow_input <- ""
  } else {
    year_input <- year
    source_input <- source   
    unit_input <- unit
    stk_flow_input <- stk_flow
    geo_input <- geo
  }
  
  if ( labelling == 'eurostat' ) labelling <- 'short'
  
  ## Whenever possible and force_download = FALSE use the data that has been already processed
  ## in this session, or saved by the user in any session(s) to the data_directory ----------------
  
  ## Initialize path and filename, this will be used for retrieval or for saving at least
  ## into the data retrieved by tempdir().
  if (is.null(data_directory)) { 
    data_directory <- tempdir() 
    save_msg_text <- " into the path retrieved by tempdir()."
  } else { save_msg_text <- paste0("into ", data_directory) }
  
  save_file_name <- paste0(geo_input, '_', year_input, '_', 
                           source_input, '_', stk_flow_input, '_', unit_input, 
                           '.rds')
  save_file_path <- file.path(data_directory, save_file_name)
  
  if (!force_download & file.exists(save_file_path) & ! is_internal_dataset ) {
    ## If there is no forced downloading and the data has been already used in this
    ## session, it has been saved to the data_directory, which is by default the 
    ## path retrieved by tempdir().
    message("Retrieving ", save_file_name, " from tempdir().")
    return(readRDS(save_file_path))
  }
  
  ## Parameter exception handling --------------------------------------------------------------------
  if (is.null(source)){ stop ("Parameter 'source' is a mandatory input.")}
  if (is.null(labelled_io_data) & !source %in% c( internal_datasets, "uk_2010" )) {
    validate_source(source)
  }
  
  ## Try to find the dataset in the temporary directory  or in package internal files ----------------
  ## First check for the internal datasets of the package, then use the naming convention of 
  ## iotables_download()
  ## The labelled_io_data will contain one or more, potentially nested IOTs.
  tmp_rds <- file.path(tempdir(), paste0(source, "_processed", ".rds"))
  
  if ( is.null(labelled_io_data) ) {
    # The data is retrieved if the user is not using already reatrieved data as input. 
    if (source_input %in% c("germany_1990", "croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")) {
      labelled_io_data <- get_package_iots(source_input)   ## internal function in get_saved_table.R
    } else if (file.exists(tmp_rds) & ! force_download & ! is_internal_dataset ) {
      ## Try to retrieve the already downloaded file 
      message("Reading ", source_input, " data (", round(file.info(tmp_rds)$size /(1024*1024),1), " megabytes) from the temporary directory.")
      labelled_io_data <- readRDS(tmp_rds) 
    } else if (! is_internal_dataset ) { 
      ## Try to download from Eurostat
      message("Reading ", source_input, " data from the Eurostat Rest API. This may take a while.")
      labelled_io_data <- iotables_download(source,
                                            data_directory = data_directory, 
                                            force_download = force_download) 
    }
  }
  
  ### Extra formatting for the UK 2010 test data ----------------------------------------------------
  if ( source_input %in% uk_tables ) {
    labelled_io_data <- uk_test_data_exceptions(labelled_io_data, source_input)   ## internal function in get_saved_table.R
  } 
  
  ## Selecting table from labelled_io_data, some may be nested -----------------------------------------
  if ( source %in% c("croatia_2010_1700" , "croatia_2010_1800" , 
                     "croatia_2010_1900" , 
                     "germany_1990", uk_tables ) ) { 
    ## These are not nested datasets, no unnesting is needed
    iotable <- labelled_io_data 
  } else {
    ## The old code is in data-raw/old_table_selection_function.R
    iotable <- get_saved_table(labelled_io_data, 
                               geo_input, 
                               year_input, 
                               unit_input,  
                               stk_flow_input)
  }
  
  ## From this point, iotable contains the unordered data in long form. It needs to be ordered, then
  ## pivotted to a wider form. 
  
  ## Converting factors to numbers --------------------------------------------------------------------
  if ( class(iotable$values) %in% c("character", "factor") ) {
    iotable$values <- trimws(as.character(iotable$values), which = "both")
    iotable$values <- as.numeric(iotable$values)
  }
  
  ## Get and order the IOT -------------------------------------------------------------------------- 
  iotable_labelled_wide <- order_iotable(iotable = iotable, 
                                         stk_flow = stk_flow_input, 
                                         source = source_input, 
                                         labelling = labelling ) 
  
  ## Save the results, either into the user's data_directory, or, if it is NULL, save them
  ## to the temporary directory retrieved by tempdir().
  ## No need to save the data when the data source is one of the internal (replication) datasets 
  ## of the iotables package itself.
  
  if ( ! source %in% c("germany_1990", croatia_files) ) {
    ## No need to save the internal replication data of the package.
    message ( "Saving ", save_file_name, save_msg_text )
    saveRDS(iotable_labelled_wide, save_file_path, version=2)
  }
  
  ## At last return the iotable in wide (and labelled, ordered) format.
  iotable_labelled_wide
}




