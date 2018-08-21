#' Get an input-output table from bulk file
#' 
#' This function is used to filter out  a single input-output table from a database, 
#' for example a raw file downloaded from the Eurostat website.  It provides some 
#' functionality to avoid some pitfalls.
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
#' \href{http://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/methodology/symmetric-input-output-tables}{Eurostat Symmetric Input-Output Tables} page.
#' @param labelled_io_data If you have downloaded a bulk data file with 
#' \code{\link{iotables_download}}, it is faster to work with the data
#' in the memory. Defaults to \code{NULL} when  the data will be retrieved from
#' the hard disk or from the Eurostat website invoking the same function.
#' @param geo A country code or a country name.  For example, \code{SK} or as \code{Slovakia}.
#' @param year A numeric variable containing the year. Defaults to \code{2010}, because this year has the most data. 
#' @param unit A character string containing the currency unit, defaults to \code{MIO_NAC} (million national currency unit). 
#' The alternative is \code{MIO_EUR}. 
#' @param stk_flow Defaults to \code{DOM} as domestic output, alternative \code{IMP} for imports 
#' and code{'TOTAL'} for total output. For \code{source = 'naio_10_cp1620'} and 
#' trade and transport margins and  \code{source = 'naio_10_cp1630'} taxes 
#' less subsidies only \code{TOTAL} is not used.
#' @param labelling Defaults to \code{iotables} which gives standard row and column names regardless of the
#' source of the table, or if it is a product x product, industry x industry or product x industry table.
#' The alternative is \code{short} which is the original short row or column code of Eurostat or OECD.
#' @param data_directory Defaults to \code{NULL}, if a valid directory, it will try to save the pre-processed 
#' data file here with labelling. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing downloaded file
#' in the \code{data_directory} or the temporary directory, if it exists. Will force
#' download only in a new session.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename left_join arrange mutate_if
#' @importFrom tidyr spread
#' @importFrom forcats fct_reorder
#' @importFrom lubridate year
#' @examples 
#' germany_table <- iotable_get( source = "germany_1990", geo = 'DE', 
#'              year = 1990,   unit = "MIO_EUR", 
#'              labelling  = "iotables")
#' @export 

iotable_get <- function ( labelled_io_data = NULL, 
                          source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          stk_flow = "DOM", labelling = "iotables", 
                          data_directory = NULL, 
                          force_download = TRUE) { 
##Initialize variables ------------
  time = NULL; t_cols2 = NULL; t_rows2 = NULL; 
  stk_flow_input <- stk_flow
  stk_flow <- NULL
  values = NULL ; .= NULL #non-standard evaluation creates a varning in build. 
  iotables_row <- iotables_col <- prod_na <- induse <- variable <-  NULL
  row_order <- col_order <- iotables_label <- code <- numeric_label <- label <- NULL

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
    metadata_rows <-  germany_metadata_rows  
    metadata_cols <-  germany_metadata_cols 
  } else {
    stop ("This type of input-output database is not (yet) recognized by iotables.")
  }
  
  metadata_rows <- dplyr::mutate_if ( metadata_rows, is.factor, as.character )
  metadata_cols <- dplyr::mutate_if ( metadata_cols, is.factor, as.character )
  
  
  if ( is.null(labelled_io_data) ) {
    if (is.null(geo)) stop ("Error: no country selected.")
    if (! labelling %in% c("iotables", "short")) {
      stop("Only iotables or original short columns can be selected.")
    }
    if (! unit  %in% c("MIO_NAC", "MIO_EUR", "T_NAC")) {
      stop("Currency unit must be MIO_NAC, MIO_EUR or T_NAC")
    }
    if ( source %in% c("naio_10_cp1620", "naio_10_cp1630")) {
      if ( stk_flow != "TOTAL") {
        stk_flow_input <- "TOTAL"
        warning ( "The parameter stk_flow was changed to TOTAL." )
      }
    }
    source_inputed <- source
    
     
    ##Creating a temporary file name for the input-output table ----
    tmp_rds <- file.path(tempdir(), paste0(source, "_", labelling, ".rds"))
    if ( source == "germany_1990") {
      labelled_io_data <- iotables::germany_1990    # use germany example 
      labelled_io_data$year = 1990
    } else if ( source == "croatia_2010_1700" ) { 
      labelled_io_data <- iotables::croatia_2010_1700 %>%
        mutate ( year = lubridate::year ( time ))
    } else if ( source == "croatia_2010_1800" )  {
      labelled_io_data <- iotables::croatia_2010_1800   %>%
        mutate ( year = lubridate::year ( time ))
    } else if ( source == "croatia_2010_1900" )  {
      labelled_io_data <- iotables::croatia_2010_1900 %>%
        mutate ( year = lubridate::year ( time ))
    } else  {
      if ( tmp_rds %in% list.files (path = tempdir()) ) {
        labelled_io_data <- readRDS( tmp_rds ) 
      } else { #getting or downloading the bulk longform data
        labelled_io_data <- iotables_download ( source,
                                                data_directory = data_directory, 
                                                force_download = force_download ) 
      }
    } # use eurostat files 
  } #end of possible downloads or data retrieval
  
 ##Loading the data and veryfing parameters ----  
  
  if ( nchar(geo) == 2 & geo == tolower(geo)) { 
     geo <- toupper (geo)
    warning("Warning: country code changed to upper case.")
  }
  
  if ( ! unit %in% labelled_io_data$unit ) { 
    stop("This currency unit is not found in the raw data frame.")
  }
  
  if ( ! geo %in% labelled_io_data$geo ) { 
    stop("This currency unit is not found in the raw data frame.")
  }
  
  if ( ! year %in% labelled_io_data$year ) { 
    stop("This currency unit is not found in the raw data frame.")
  }
  

###converting factors to characters------  
  
 selected_table <- which (
    labelled_io_data$year == year & 
      as.character(labelled_io_data$geo) == geo &
      labelled_io_data$unit == unit)
 
 if ( length( selected_table) == 0  )  {
   stop ( "There is no available table for country ", geo, " in the year ", year , 
          " with ", unit, " units.")
 }

  if ( ! source %in% c("croatia_2010_1700" , "croatia_2010_1800" , "croatia_2010_1900" , 
                       "germany_1990")) {
    iotable <- labelled_io_data$data[[selected_table]]
  } else {
    iotable <- labelled_io_data
  }

  
  if ( class(iotable$values) %in% c("character", "factor")) {
    iotable$values  = trimws(as.character(iotable$values), which = "both")
    iotable$values = as.numeric(iotable$values)
    warning("Warning: original data was converted to numeric format.")
  }
  
 if ( source %in% prod_ind ) {
     
    iotable_labelled <- iotable %>%
      dplyr::mutate_if ( is.factor, as.character ) %>%
      dplyr::full_join (.,  metadata_cols, by = c("induse", "induse_lab"))  %>%
      dplyr::full_join ( ., metadata_rows, by = c("prod_na", "prod_na_lab")) %>%
      dplyr::mutate ( prod_na = forcats::fct_reorder(prod_na, 
                                              as.numeric(row_order))) %>%
      dplyr::mutate ( induse = forcats::fct_reorder(induse, 
                                             as.numeric(col_order))) %>%
      dplyr::mutate ( iotables_row = forcats::fct_reorder(iotables_row ,
                                                       as.numeric(row_order))) %>%
      dplyr::mutate ( iotables_col = forcats::fct_reorder(iotables_col, 
                                                       as.numeric(col_order))) %>%
      dplyr::filter (stk_flow == stk_flow_input ) 
    
  } else  {
    if ( ! source %in% croatia_files ){  # !prod_ind
      iotable_labelled <- iotable %>%
        dplyr::mutate_if ( is.factor, as.character ) %>%
        dplyr::left_join (., metadata_cols, by = c("t_cols2", "t_cols2_lab"))  %>%
        dplyr::left_join ( ., metadata_rows, by = c("t_rows2", "t_rows2_lab")) %>%
        dplyr::arrange ( row_order, col_order )
    } else {
      iotable_labelled <- iotable 
    }
    
    iotable_labelled <- iotable_labelled %>%
      dplyr::mutate ( t_rows2 = forcats::fct_reorder(t_rows2, 
                                              as.numeric( row_order))) %>%
      dplyr::mutate ( t_cols2 = forcats::fct_reorder(t_cols2, 
                                              as.numeric( col_order ))) %>%
      dplyr::mutate ( iotables_row = forcats::fct_reorder(iotables_row , 
                                                       as.numeric(row_order))) %>%
      dplyr:::mutate ( iotables_col = forcats::fct_reorder(iotables_col, 
                                                         as.numeric( col_order)))
  }

  if ( labelling == "iotables" ) {
    
    iotable_labelled_w <- iotable_labelled %>%
      dplyr::arrange ( iotables_row, iotables_col) %>%
      dplyr::select ( iotables_col, iotables_row, values ) %>% 
      tidyr::spread ( iotables_col, values )
    
  } else if ( labelling == "short" & source %in% prod_ind ) {
    
    iotable_labelled_w <- iotable_labelled %>%
      dplyr::select (prod_na, induse, values ) %>%
      dplyr::filter ( !is.na(prod_na)) %>%
      tidyr::spread (induse, values )
    
  } else {
    iotable_labelled_w <- iotable_labelled %>%
      dplyr::select ( t_rows2, t_cols2, values ) %>%
      tidyr::spread (t_cols2, values )
  }
  
  if (!is.null(data_directory) ) {
    save_file_name <- paste0(geo, '_', year, '_', 
      source, '_', stk_flow, '_', unit, 
      '.rds')
    save_file_name <- file.path(data_directory, save_file_name)
    message ( "Saving ", save_file_name, '.')
    saveRDS(iotable_labelled_w, save_file_name)
  }
  
 iotable_labelled_w
}


