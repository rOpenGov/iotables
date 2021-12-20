#' @title Get an input-output table fom bulk file
#' 
#' @description This function is used to filter out  a single input-output table from
#' a database, for example a raw file downloaded from the Eurostat 
#' website.  It provides some functionality to avoid some pitfalls.
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
##'  \item{\code{naio_10_pyp1630}}{Table of taxes less subsidies on products at previous years' prices}
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
#' @param data_directory Defaults to \code{NULL}, if a valid directory, 
#' it will try to save the pre-processed data file here with labelling. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing 
#' downloaded file in the \code{data_directory} or the temporary 
#' directory, if it exists. Will force download only in a new session.
#' @return A wide format data.frame with a well-ordered input-output table.
#' The bulk data files on the Eurostat website are in a long form and they are 
#' not correctly ordered for further matrix equations.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename left_join arrange all_of
#' @importFrom tidyr spread
#' @importFrom forcats fct_reorder
#' @importFrom lubridate year
#' @importFrom utils data
#' @importFrom rlang .data
#' @importFrom utils globalVariables
#' @family import functions
#' @examples 
#' germany_table <- iotable_get( source = "germany_1990", 
#'                  geo = 'DE', year = 1990, unit = "MIO_EUR", 
#'                  labelling  = "iotables" )
#' @export 

iotable_get2 <- function ( labelled_io_data = NULL, 
                          source = "germany_1990", 
                          geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          stk_flow = "DOM", 
                          labelling = "iotables", 
                          data_directory = NULL, 
                          force_download = TRUE) { 
 
  if ( labelling == 'eurostat' ) labelling <- 'short'
  validated_iotable_inputs(source, geo, labelling, unit, force_download)
  ## Parameter exception handling -------------------------------------
  if (is.null(labelled_io_data) & is.null(geo)) stop ("The 'geo' parameter must be a valid Eurostat 'geo' code")
  if (is.null(labelled_io_data) & !source %in% c("germany_1990", 
                                                 "uk_2010", 
                                                 "croatia_2010_1900", 
                                                 "croatia_2010_1800", 
                                                 "croatia_2010_1700")) {
    validate_source(source)
  }
  
  ## Exception handling for tax and margin tables -----------------------
  if ( source %in% c("naio_10_cp1620",  "naio_10_cp1630", 
                     "naio_10_pyp1620", "naio_10_pyp1630")) {
    if ( stk_flow != "TOTAL") {
      stk_flow_input <- "TOTAL"
      warning ( "The parameter stk_flow was changed to TOTAL. Tax and margin tables only have one version." )
      
    }
    stk_flow_input <- 'TOTAL'  #tax and margin tables only have one version 
  }
  
  uk_tables <- c("uk_2010_siot", "uk_2010_coeff", "uk_2010_inverse")
  
  ##Veryfing source parameter and loading the labelling  -----------
  prod_ind <- c("naio_10_cp1700", "naio_10_cp1750", "naio_10_pyp1700",
                "naio_10_pyp1750", "naio_10_cp15", "naio_10_cp16",
                "naio_10_cp1610", "naio_10_cp1620", "naio_10_cp1630", 
                "naio_10_pyp1620", "naio_10_pyp1630", "germany_1990")
  
  trow_tcol <- croatia_files <- c('croatia_2010_1700', 'croatia_2010_1800', 
                                  'croatia_2010_1900')
  
  metadata_rows <- get_siot_metadata_rows(source)
  metadata_cols <- get_siot_metadata_cols(source)
  
  prod_ind <- c(prod_ind, uk_tables)
  
  ##Potential geo corrections ----  
  geo <- parameter_corrections_geo(geo)
  
  ## Selecting table from nested data, if nested at all ---------------  
  select_iotable(labelled_io_data = labelled_io_data, geo=geo, year=year, unit=unit, stk_flow=stk_flow)
  
  ### Exception handling for UK test data ------------------------------------------------------
  if ( source %in% uk_tables ) labelled_io_data <- uk_metadata_adjusment(labelled_io_data, source)
  
  
  ## Get and order the SIOT-------  
  if ( source %in% prod_ind ) {
    col_join <- names ( iotable ) [ which( names(iotable) %in% c("induse", "induse_lab", "iotables_col", "uk_col") )] 
    row_join <- names ( iotable ) [ which( names(iotable) %in% c("prod_na", "prod_na_lab", "iotables_row", "uk_row") )] 
    
    remove_vars <- c("quadrant", "account_group", "variable", 
                     "group", "eu_prod_na")
    remove_vars  <- remove_vars [remove_vars %in% names (metadata_cols)]
    
    iotable_labelled <- iotable %>%
      filter(stk_flow == stk_flow_input )  %>%
      mutate( across(where(is.factor), as.character) ) %>%
      left_join( metadata_cols, by = col_join  ) %>%
      select( -all_of(remove_vars) ) %>%  #remove repeating columns before joining rows
      mutate( across(where(is.factor), as.character) ) %>% 
      left_join ( metadata_rows, by = row_join ) 
    
    if ( nrow (iotable_labelled) == 0 ) {
      stop ( "No rows found with geo = ", geo_input, " year = ", year, 
             " unit = ", unit, " and stk_flow = ", stk_flow_input, "." )
    }
    
    iotable_labelled <- iotable_labelled %>%
      mutate(prod_na = forcats::fct_reorder(prod_na, 
                                            as.numeric(.data$row_order))) %>%
      mutate(induse  = forcats::fct_reorder(induse, 
                                            as.numeric(.data$col_order))) 
    
    if ( all(c("iotables_row", "iotables_col") %in%  names (iotable_labelled)) ) {
      iotable_labelled <-  iotable_labelled %>%
        mutate(iotables_row = forcats::fct_reorder(iotables_row ,
                                                   as.numeric(.data$row_order))) %>%
        mutate(iotables_col = forcats::fct_reorder(iotables_col, 
                                                   as.numeric(.data$col_order)))
    }
    
  } else  {
    if ( ! source %in% croatia_files ){  # !prod_ind
      
      by_col <- names(iotable)[which ( names(iotable) %in% c("t_cols2", "t_cols2_lab", "iotables_col") )]
      by_row <- names(iotable)[which ( names(iotable) %in% c("t_rows2", "t_rows2_lab", "iotables_row") )]
      
      iotable_labelled <- iotable %>%
        mutate ( across(where(is.factor), as.character) ) %>%
        left_join(metadata_cols, by = by_col)  %>%
        left_join(metadata_rows, by = by_row) %>%
        arrange ( .data$row_order, .data$col_order )
    } else {
      iotable_labelled <- iotable 
    }
    iotable_labelled <- iotable_labelled %>%
      mutate(t_rows2 = forcats::fct_reorder(.data$t_rows2, 
                                            as.numeric(.data$row_order))) %>%
      mutate(t_cols2 = forcats::fct_reorder(.data$t_cols2, 
                                            as.numeric( .data$col_order ))) %>%
      mutate(iotables_row = forcats::fct_reorder(.data$iotables_row, 
                                                 as.numeric(.data$row_order))) %>%
      mutate(iotables_col = forcats::fct_reorder(.data$iotables_col, 
                                                 as.numeric(.data$col_order)))
  } #end of not prod_na cases
  
  if ( labelling == "iotables" ) {
    
    iotable_labelled_w <- iotable_labelled %>%
      arrange (.data$iotables_row, .data$iotables_col) %>%
      select( all_of(c("iotables_col", "iotables_row", "values")) ) %>% 
      tidyr::spread (iotables_col, .data$values)
    
  } else if ( labelling == "short" & source %in% prod_ind ) {
    
    iotable_labelled_w <- iotable_labelled %>%
      select(.data$prod_na, .data$induse, .data$values) %>%
      filter( !is.na(.data$prod_na) )  %>%
      tidyr::spread (induse, .data$values )
    
  } else {
    iotable_labelled_w <- iotable_labelled %>%
      select( all_of(c("t_rows2", "t_cols2", "values")) ) %>%
      tidyr::spread ( .data$t_cols2, .data$values )
  }
  
  if (!is.null(data_directory) ) {
    save_file_name <- paste0(geo, '_', year, '_', 
                             source, '_', stk_flow, '_', unit, 
                             '.rds')
    save_file_name <- file.path(data_directory, save_file_name)
    message ( "Saving ", save_file_name, '.')
    saveRDS(iotable_labelled_w, save_file_name, version=2)
  }
  
  iotable_labelled_w
}

#' @keywords internal
get_siot_metadata_rows <- function( source ) {
  ##Avoiding no visible binding for global variable 'data' ----------
  getdata <- function(...)
  {
    e <- new.env()
    name <- utils::data(..., envir = e)[1]
    e[[name]]
  }
  if ( source %in% prod_ind ) { 
    metadata_rows <- getdata (metadata) %>%  #tables that follow prod_ind vocabulary
      filter( variable == "prod_na") %>%
      dplyr::rename ( prod_na = .data$code) %>%
      dplyr::rename ( prod_na_lab = .data$label ) %>%
      dplyr::rename ( row_order = .data$numeric_label ) %>%
      dplyr::rename ( iotables_row = .data$iotables_label ) 
  } else if (source %in% trow_tcol) {
    metadata <- getdata(metadata)
    metadata_rows <- metadata %>%
      filter( variable == "t_rows") %>%
      dplyr::rename ( t_rows2 = .data$code) %>%
      dplyr::rename ( t_rows2_lab = .data$label ) %>%
      dplyr::rename ( row_order = .data$numeric_label ) %>%
      dplyr::rename ( iotables_row = .data$iotables_label )
  } else if ( source %in% uk_tables ) {
    metadata_uk_2010 <- getdata(metadata_uk_2010)
    metadata_rows <- metadata_uk_2010  %>%
      filter ( !is.na(.data$uk_row)) %>%
      select ( -all_of(c("uk_col", "uk_col_label", "induse", "col_order")) ) %>%
      mutate ( uk_row = gsub("\\.", "-", as.character(.data$uk_row))) %>%
      mutate ( uk_row = gsub(" & ", "-", as.character(.data$uk_row)))
  }
  metadata_rows %>% mutate ( across(where(is.factor), as.character) )
}

#' @keywords internal
get_siot_metadata_cols <- function(source) {
  ##Avoiding no visible binding for global variable 'data' ----------
  getdata <- function(...)
  {
    e <- new.env()
    name <- utils::data(..., envir = e)[1]
    e[[name]]
  }
  if ( source %in% prod_ind ) { 
    metadata_cols <- getdata(metadata) %>%
      filter( variable == "induse") %>%
      dplyr::rename ( induse = .data$code) %>%
      dplyr::rename ( induse_lab = .data$label )%>%
      dplyr::rename ( col_order = .data$numeric_label ) %>%
      dplyr::rename ( iotables_col = .data$iotables_label )
  } else if ( source %in% trow_tcol ) {
    metadata_cols <- metadata %>%
      filter( variable == "t_cols") %>%
      dplyr::rename ( t_cols2 = .data$code) %>%
      dplyr::rename ( t_cols2_lab = .data$label ) %>%
      dplyr::rename ( col_order = .data$numeric_label ) %>%
      dplyr::rename ( iotables_col = .data$iotables_label )
  } else if ( source %in% uk_tables ) {
    metadata_uk_2010 <- getdata(metadata_uk_2010)
    metadata_cols <- metadata_uk_2010  %>%
      filter( !is.na(.data$uk_col)) %>%
      select( -.data$uk_row, -.data$uk_row_label, -.data$prod_na, -.data$row_order) %>%
      mutate ( uk_col = gsub("\\.", "-", as.character(.data$uk_col))) %>%
      mutate ( uk_col = gsub(" & ", "-", as.character(.data$uk_col))) %>%
      mutate ( uk_col = trimws(.data$uk_col, 'both'))
  }
  metadata_cols %>% mutate ( across(where(is.factor), as.character) )
}


#' @keywords internal
select_iotable <- function(labelled_io_data, geo, year, unit, stk_flow) {
  
 
  
  if ( is.null(labelled_io_data)) {
    nested <- FALSE
  } else if ( "data" %in% names(labelled_io_data) ) {
    if ( "list" %in% class(labelled_io_data$data) )
      nested <- TRUE
  } else {
    nested <- FALSE
  }
  
  if (nested) {
    
    selected_table <- which (   ## Get the number of table to be selected
      labelled_io_data$year == year & 
        as.character(labelled_io_data$geo) == geo &
        labelled_io_data$unit == unit  &
        labelled_io_data$stk_flow == stk_flow
    )
    
    assertthat::assert_that(length(selected_table)>0, 
                            msg = glue::glue("In source='{source}' there is no available table for geo='{geo_input}' in year='{year}' with unit='{unit}' and stk_flow='{stk_flow}'.")
    )
    
    assertthat::assert_that(length(selected_table)==1, 
                            msg = glue::glue("In source='{source}' geo='{geo}' in year='{year}' with unit='{unit}' and stk_flow='{stk_flow}' do not select a unique table.")
    )
    
    assert_that ( geo %in% labelled_io_data$geo, 
                  msg = glue::glue("geo='{geo}' not found in the labelled IO data table."))
    
    selected_table <- labelled_io_data %>%
      filter (.data$geo == geo)
    
    assert_that ( stk_flow %in% labelled_io_data$stk_flow, 
                  msg = glue::glue("stk_flow='{stk_flow}' not found in the labelled IO data table."))
    
    selected_table <- selected_table %>%
      filter (.data$stk_flow == stk_flow)
    
    assert_that ( year %in% labelled_io_data$year, 
                  msg = glue::glue("year='{year}' not found in the labelled IO data table."))
    
    
    selected_table <- selected_table %>%
      filter (.data$year == year)
    
    assert_that ( unit %in% labelled_io_data$unit, 
                  msg = glue::glue("The (currency) unit='{unit}' not found in the labelled IO data table."))
    
    selected_table <- selected_table %>%
      filter (.data$unit == unit)
    
    return_table <- labelled_io_data$data[[selected_table]]
    
  }  else if ( !is.null(labelled_io_data)) {
    
    if ( ! unit %in% labelled_io_data$unit ) { 
      stop("This (currency) unit is not found in the raw data frame.")
    }
    
    if ( ! geo %in% labelled_io_data$geo ) { 
      stop("This geographical unit is not found in the raw data frame.")
    }
    
    if ( ! year %in% labelled_io_data$year ) { 
      stop("This year is not found in the raw data frame.")
    }
    return_table <- labelled_io_dat
  } else if ( is.null(labelled_io_data)) { #if not directly inputed data 
      ## Creating a temporary file name for the input-output table ----
      tmp_rds <- file.path(tempdir(), paste0(source, "_", labelling, ".rds"))
      
      ## Read from file or internal dataset ----
      if ( source_inputed == "germany_1990" ) {
        
        germany_1990 <- getdata(germany_1990) 
        labelled_io_data <- germany_1990    # use germany example 
        labelled_io_data$year <- 1990
        
      } else if ( source_inputed == "croatia_2010_1700" ) { 
        
        croatia_2010_1700 <- getdata(croatia_2010_1700)
        labelled_io_data <- croatia_2010_1700 %>%
          mutate ( year = lubridate::year(.data$time))
        
      } else if ( source_inputed == "croatia_2010_1800" )  {
        
        croatia_2010_1800 <- getdata(croatia_2010_1800)
        labelled_io_data <- croatia_2010_1800   %>%
          mutate ( year = lubridate::year (.data$time))
        
      } else if ( source_inputed == "croatia_2010_1900" )  {
        
        croatia_2010_1900 <- getdata(croatia_2010_1900)
        labelled_io_data <- croatia_2010_1900 %>%
          mutate ( year = lubridate::year(.data$time))
        
      } else {
        if ( tmp_rds %in% list.files (path = tempdir()) ) {
          labelled_io_data <- readRDS( tmp_rds ) 
        } else { #getting or downloading the bulk long-form data
          labelled_io_data <- iotables_download ( source,
                                                  data_directory = data_directory, 
                                                  force_download = force_download ) 
        }
        
        
      } # use eurostat files 
    } #end of possible downloads or data retrieval if not directly inputed
    
  }
  
  ## Converting factors to numbers --------------------------------------
  if ( class(return_table$values) %in% c("character", "factor") ) {
    return_table$values <- trimws(as.character(iotable$values), which = "both")
    return_table$values <- as.numeric(iotable$values)
    message("The original 'values' data was converted to numeric format.")
  }
  
  return_table
}

#' @keywords internal
validated_iotable_inputs <- function(source, geo, labelling, unit, force_download) {
  if (is.null(source)){ stop ("Parameter 'source' is a mandatory input.")}
  assertthat::assert_that(is.logical(force_download), 
                          msg = "Parameter force_download must be TRUE or FALSE")
  
  assertthat::assert_that(labelling %in% c("iotables", "short"), 
                          msg = "Parmater labelling must be 'short' or 'iotables'.")
  
  assertthat::assert_that( unit  %in% c("MIO_NAC", "MIO_EUR", "T_NAC"), 
                           msg = "Currency unit must be MIO_NAC, MIO_EUR or T_NAC.")
  # what to do with geo?
}

parameter_corrections_geo <- function(geo) {
  if ( nchar(geo) == 2 & geo == tolower(geo)) { 
    geo <- toupper (geo)
    warning("Warning: country code changed to upper case.")
  }
  geo
}


uk_metadata_adjustment <- function(labelled_io_data, source) {
  if ( source == "uk_2010_siot") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Input-Output table (domestic use, basic prices, product by product)')
  }
  
  if ( source == "uk_2010_use") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Domestic use table at basic prices (product by industry)')
  }
  
  if ( source == "uk_2010_imports") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Imports use table at basic prices (product by product)')
  }
  
  if ( source == "uk_2010_coeff") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Matrix of coefficients (product by product)')
  }
  
  if ( source == "uk_2010_inverse") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Leontief Inverse (product by product)')
  }
} 
