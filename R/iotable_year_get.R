#' Get Available Years For Input-Output Tables
#' 
#' The function selects the available tables by year or time as a date 
#' for a specific country and currency unit in the Eurostat bulk file.
#' Unless you want to work with bulk data files, you should not invoke  \code{\link{iotables_download}} 
#' directly, rather via this function, if and when it is necessary. 
#' @param source A data source, for example \code{naio_10_cp1700}. 
#' Symmetric input-output table at basic prices (product by product) (naio_10_cp1700)	
#' Symmetric input-output table at basic prices (industry by industry) (naio_10_cp1750)
#' Symmetric input-output table at basic prices (product by product) (previous years prices) (naio_10_pyp1700)
#' Symmetric input-output table at basic prices (industry by industry) (previous years prices) (naio_10_pyp1750)
#' Table of trade and transport margins at basic prices (naio_10_cp1620) and 
#' at previous' years prices (naio_10_pyp1620)
#' Table of taxes less subsidies on products at basic prices (naio_10_cp1630)	and
#' at previous' years prices (naio_10_pyp1630)
#' For further information consult the 
#' \href{https://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/overview}{Eurostat Symmetric Input-Output Tables} page.
#' @param labelled_io_data If you have downloaded a bulk data file with 
#' \code{\link{iotables_download}}, it is faster to work with the data
#' in the memory. Defaults to \code{NULL} when  the data will be retrieved from
#' the hard disk or from the Eurostat website invoking the same function.
#' @param geo A country code or a country name.  For example, \code{SK} or as \code{Slovakia}.
#' @param unit A character string containing the currency unit, defaults to \code{MIO_NAC} (million national currency unit). 
#' The alternative is \code{MIO_EUR}. 
#' @param stk_flow Defaults to \code{DOM} as domestic output, alternative \code{IMP} for imports 
#' and \code{TOTAL} for total output. For \code{source = 'naio_10_cp1620'} and 
#' trade and transport margins and  \code{source = 'naio_10_cp1630'} taxes 
#' less subsidies only \code{TOTAL} is not used.
#' @param time_unit Defaults to \code{'year'} and years are returned as numbers. 
#' Alternative is to return  \code{'time'} as vector of dates. 
#' @param data_directory Defaults to \code{NULL}. Use if it you used a data_directory
#' parameter with \code{\link{iotable_get}} or \code{\link{iotables_download}}. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing 
#' downloaded file in the \code{data_directory} or the temporary directory, 
#' if it exists. Will force download only in a new session.
#' @return A vector with the years that have available input-output tables.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename left_join arrange across
#' @importFrom forcats fct_reorder
#' @importFrom lubridate year
#' @examples 
#' germany_years <- iotable_year_get ( source = "germany_1990", geo = 'DE', 
#'                                     unit = "MIO_EUR" )
#' @export 

iotable_year_get <- function ( labelled_io_data = NULL, 
                          source = "germany_1990", 
                          geo = "DE",
                          unit = "MIO_EUR",
                          time_unit = 'year',
                          stk_flow = 'TOTAL',
                          data_directory = NULL,
                          force_download = TRUE ) { 
##Initialize variables ------------
  values  <- .<-  NULL #non-standard evaluation creates a varning in build. 
  time <- t_cols2  <- t_rows2 <- by_row <- by_col <- tmp_rds <- NULL
  account_group <- digit_1 <- digit_2 <- group <- quadrant <- NULL
  iotables_row <- iotables_col <- prod_na <- induse <- variable <-  NULL
  row_order <- col_order <- iotables_label <- code <- numeric_label <- label <- NULL
 
  source_inputed <- source; unit_input <- unit
  geo_input <- geo; stk_flow_input <- stk_flow

  if ( source %in% c("naio_10_cp1620", "naio_10_cp1630", 
                     "naio_10_pyp1620", "naio_10_pyp1630")
       ) {
    stk_flow_input <- 'TOTAL'  #tax and margin tables only have one version 
  }
  
  if ( ! time_unit %in% c("year", "time") ) { time_unit <- "year"}
  if ( source == "germany_1990") { time_unit <- "time"   }
  

  
##Veryfing source parameter and loading the labelling  ----
  prod_ind <- c("naio_10_cp1700", "naio_10_cp1750", "naio_10_pyp1700",
                "naio_10_pyp1750", "naio_10_cp1620", "naio_10_cp1630", 
                "naio_10_pyp1620", "naio_10_pyp1630" )
  trow_tcol <-  c(  "croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")
  croatia_files <- c( "croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")
  
  if ( source %in% prod_ind ) { 
    metadata_rows <- iotables::metadata %>%  #tables that follow prod_ind vocabulary
      dplyr::filter ( variable == "prod_na") %>%
      dplyr::rename ( prod_na = code) %>%
      dplyr::rename ( prod_na_lab = label ) %>%
      dplyr::rename ( row_order = numeric_label ) %>%
      dplyr::rename ( iotables_row = iotables_label )
    
    metadata_cols <- iotables::metadata %>%
      dplyr::filter ( variable == "induse") %>%
      dplyr::rename ( induse = code) %>%
      dplyr::rename ( induse_lab = label )%>%
      dplyr::rename ( col_order = numeric_label ) %>%
      dplyr::rename ( iotables_col = iotables_label )
    
  } else if ( source %in% trow_tcol ) {   #tables that follow trow_tcol vocabulary
    
    metadata_rows <- iotables::metadata %>%
      dplyr::filter ( variable == "t_rows") %>%
      dplyr::rename ( t_rows2 = code) %>%
      dplyr::rename ( t_rows2_lab = label ) %>%
      dplyr::rename ( row_order = numeric_label ) %>%
      dplyr::rename ( iotables_row = iotables_label )
    
    metadata_cols <- iotables::metadata %>%
      dplyr::filter ( variable == "t_cols") %>%
      dplyr::rename ( t_cols2 = code) %>%
      dplyr::rename ( t_cols2_lab = label ) %>%
      dplyr::rename ( col_order = numeric_label ) %>%
      dplyr::rename ( iotables_col = iotables_label )
    
  } else if ( source == "germany_1990" ) {  #German simplified tables
    metadata_rows <- germany_metadata_rows  
    metadata_cols <- germany_metadata_cols 
  } else {
    stop ("This type of input-output database is not (yet) recognized by iotables.")
  }
  
  metadata_rows <- mutate ( metadata_rows, across(where(is.factor), as.character) )
  metadata_cols <- mutate ( metadata_cols, across(where(is.factor), as.character) )
  
  ###Exception handling for wrong paramters-----
  if ( is.null(labelled_io_data) ) {  #if not directly inputed data 
    if (is.null(geo)) stop ("Error: no country selected.")
  
    if (! unit  %in% c("MIO_NAC", "MIO_EUR", "T_NAC")) {
      stop("Currency unit must be MIO_NAC, MIO_EUR or T_NAC")
    }
    if ( source %in% c("naio_10_cp1620", "naio_10_cp1630")) {
      if ( stk_flow != "TOTAL") {
        stk_flow_input <- "TOTAL"
        warning ( "The parameter stk_flow was changed to TOTAL." )
      }
    }

    ##Creating a temporary file name for the input-output table ----
    tmp_rds1 <- file.path(tempdir(), paste0(source, "_iotables.rds")) #if iotables labelled version was created earlier
    tmp_rds2 <- file.path(tempdir(), paste0(source, "_short.rds")) #if short labelled version was created earlier
    tmp_rds3 <- file.path(tempdir(), paste0(source, ".rds")) #if non-labelled was created earlier
    if ( source_inputed == "germany_1990" ) {
      labelled_io_data <- iotables::germany_1990    # use germany example 
    } else if ( source_inputed == "croatia_2010_1700" ) { 
      labelled_io_data <- iotables::croatia_2010_1700 %>%
        mutate ( year = lubridate::year ( time ))
    } else if ( source_inputed == "croatia_2010_1800" )  {
      labelled_io_data <- iotables::croatia_2010_1800   %>%
        mutate ( year = lubridate::year ( time ))
    } else if ( source_inputed == "croatia_2010_1900" )  {
      labelled_io_data <- iotables::croatia_2010_1900 %>%
        mutate ( year = lubridate::year ( time ))
    } else  {
      if ( any( c(tmp_rds1, tmp_rds2, tmp_rds3 ) %in% 
                list.files (path = tempdir()) )) {
        tmp_rds <- c(tmp_rds1, tmp_rds2, tmp_rds3 )[which ( !is.null (c(tmp_rds1, tmp_rds2, tmp_rds3 )) )]
        
        labelled_io_data <- readRDS( tmp_rds ) 
      } else { #getting or downloading the bulk longform data
        labelled_io_data <- iotables_download ( source,
                                                data_directory = data_directory, 
                                                force_download = force_download ) 
      }
    } # use eurostat files 
  } #end of possible downloads or data retrieval if not directly inputed
  
 ##Veryfing parameters ----  
  
  if ( nchar(geo_input) == 2 & geo_input == tolower(geo_input)) { 
     geo_input <- toupper (geo_input)
    warning("Warning: country code changed to upper case.")
  }
  
  if ( ! unit_input %in% labelled_io_data$unit ) { 
    stop("This currency unit is not found in the raw data frame.")
  }
  
  if ( ! geo_input %in% labelled_io_data$geo ) { 
    stop("This currency unit is not found in the raw data frame.")
  }


 ## Converting factors to characters ------  

 selected_tables <- which (   ##get the number of table to be selected
      as.character(labelled_io_data$geo) == geo &
      labelled_io_data$unit == unit)
  
 
 if ( time_unit == "year" ) {
   return_values <- sort(unique ( labelled_io_data$year[selected_tables] )) 
 } else { 
   return_values <- sort(unique ( labelled_io_data$time[selected_tables] )) 
 }
  
 if ( length( return_values > 0 ) ) {
   message ( "The following years are available for ", geo, " in ", unit , " currency units:\n", 
             paste(return_values, collapse = '; ' ), ".")
 } else { 
   warning ( "No tables are available for ", 
             geo, " in ", unit , " currency units.")
   }
  
 return_values
}


