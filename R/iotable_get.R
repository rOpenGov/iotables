#' Get An Input-Output Table Fom Bulk File
#' 
#' This function is used to filter out  a single input-output table from
#' a database, for example a raw file downloaded from the Eurostat 
#' website.  It provides some functionality to avoid some pitfalls.
#' 
#' Unless you want to work with bulk data files, 
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
#' @importFrom dplyr filter select mutate rename left_join arrange 
#' @importFrom dplyr mutate_if
#' @importFrom tidyselect one_of all_of
#' @importFrom tidyr spread
#' @importFrom forcats fct_reorder
#' @importFrom lubridate year
#' @importFrom utils data
#' @importFrom rlang .data
#' @family iotables import functions
#' @examples 
#' germany_table <- iotable_get( source = "germany_1990", 
#'                  geo = 'DE', year = 1990, unit = "MIO_EUR", 
#'                  labelling  = "iotables" )
#' @export 

iotable_get <- function ( labelled_io_data = NULL, 
                          source = "germany_1990", 
                          geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          stk_flow = "DOM", 
                          labelling = "iotables", 
                          data_directory = NULL, 
                          force_download = TRUE) { 
  
  ## Initialize NSE variables -----------------------------------------
  values  <- .<-  NULL #non-standard evaluation creates a warning in build. 
  #these should be eliminated, but this is a very long code.
  t_cols2  <- t_rows2 <- by_row <- by_col <- NULL
  account_group <- digit_1 <- digit_2 <- group <- quadrant <- NULL
  iotables_row <- iotables_col <- prod_na <- induse <- variable <-  NULL
  row_order <- col_order <- iotables_label <- code <- numeric_label <- label <- NULL
  uk_col <- uk_col_label <- uk_row <- uk_row_label <- indicator <- NULL

  if ( labelling == 'eurostat' ) labelling <- 'short'
  ## Parameter exception handling -------------------------------------
  if (is.null(source)){ stop ("Parameter 'source' is a mandatory input.")}
  if (is.null(labelled_io_data) & is.null(geo)) stop ("The 'geo' parameter must be a valid Eurostat 'geo' code")
  if (is.null(labelled_io_data) & !source %in% c("germany_1990", 
                                                 "uk_2010", 
                                                 "croatia_2010_1900", 
                                                 "croatia_2010_1800", 
                                                 "croatia_2010_1700")) {
    validate_source(source)
  }
  
  ##Avoiding no visible binding for global variable 'data' ----------
  getdata <- function(...)
  {
    e <- new.env()
    name <- utils::data(..., envir = e)[1]
    e[[name]]
  }
  
  ## Exception handling for tax and margin tables -----------------------
  if ( source %in% c("naio_10_cp1620",  "naio_10_cp1630", 
                     "naio_10_pyp1620", "naio_10_pyp1630")
       ) {
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
 
  if ( source %in% prod_ind ) { 
    
    metadata_rows <- getdata (metadata) %>%  #tables that follow prod_ind vocabulary
      dplyr::filter ( variable == "prod_na") %>%
      dplyr::rename ( prod_na = .data$code) %>%
      dplyr::rename ( prod_na_lab = .data$label ) %>%
      dplyr::rename ( row_order = .data$numeric_label ) %>%
      dplyr::rename ( iotables_row = .data$iotables_label )
    
    metadata_cols <- getdata(metadata) %>%
      dplyr::filter ( variable == "induse") %>%
      dplyr::rename ( induse = .data$code) %>%
      dplyr::rename ( induse_lab = .data$label )%>%
      dplyr::rename ( col_order = .data$numeric_label ) %>%
      dplyr::rename ( iotables_col = .data$iotables_label )
    
    if ( source == "germany_1990" ) { 
      year <- 1990
      geo <- "DE"
      unit <- "MIO_EUR"
      source <- "germany_1990"
    }
    
    year_input <- year
    geo_input <- geo
    unit_input <- unit
    source_inputed <- source
    
  } else if ( source %in% trow_tcol ) {   #tables that follow trow_tcol vocabulary
    
    metadata <- getdata(metadata)
    
    metadata_rows <- metadata %>%
      dplyr::filter ( variable == "t_rows") %>%
      dplyr::rename ( t_rows2 = .data$code) %>%
      dplyr::rename ( t_rows2_lab = .data$label ) %>%
      dplyr::rename ( row_order = .data$numeric_label ) %>%
      dplyr::rename ( iotables_row = .data$iotables_label )
    
    metadata_cols <- metadata %>%
      dplyr::filter ( variable == "t_cols") %>%
      dplyr::rename ( t_cols2 = .data$code) %>%
      dplyr::rename ( t_cols2_lab = .data$label ) %>%
      dplyr::rename ( col_order = .data$numeric_label ) %>%
      dplyr::rename ( iotables_col = .data$iotables_label )
    
    year_input <- year
    geo_input <- geo
    unit_input <- unit
    source_inputed <- source
    
  } else if ( source %in% uk_tables ) {
      labelling <-  'short'
      year <- year_input <- 2010 
      unit <- unit_input <- 'MIO_NAC'
      geo <- geo_input <-"UK"
      stk_flow <- stk_flow_input <- "TOTAL"
      
      metadata_uk_2010 <- getdata(metadata_uk_2010)
      
      metadata_cols <- metadata_uk_2010  %>%
        dplyr::filter ( !is.na(.data$uk_col)) %>%
        dplyr::select ( -uk_row, -uk_row_label, -prod_na, -row_order) %>%
        mutate ( uk_col = gsub("\\.", "-", as.character(.data$uk_col))) %>%
        mutate ( uk_col = gsub(" & ", "-", as.character(.data$uk_col))) %>%
        mutate ( uk_col = trimws(.data$uk_col, 'both'))
      
      metadata_rows <- metadata_uk_2010  %>%
        filter ( !is.na(.data$uk_row)) %>%
        select ( -all_of(c("uk_col", "uk_col_label", "induse", "col_order")) ) %>%
        mutate ( uk_row = gsub("\\.", "-", as.character(.data$uk_row))) %>%
        mutate ( uk_row = gsub(" & ", "-", as.character(.data$uk_row)))
      
      prod_ind <- c(prod_ind, uk_tables)
    }  else {
    stop ("This type of input-output database is not (yet) recognized by iotables.")
  }
  
  metadata_rows <- mutate_if ( metadata_rows, is.factor, as.character )
  metadata_cols <- mutate_if ( metadata_cols, is.factor, as.character )
  
  ###Exception handling for wrong paramters that are not directly inputed-----
  if ( is.null(labelled_io_data) ) {  #if not directly inputed data 
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

    ##Creating a temporary file name for the input-output table ----
    tmp_rds <- file.path(tempdir(), paste0(source, "_", labelling, ".rds"))
    
    ##Read from file or internal dataset ----
    if ( source_inputed == "germany_1990" ) {
      
      germany_1990 <- getdata(germany_1990) 
      labelled_io_data <- germany_1990    # use germany example 
      labelled_io_data$year = 1990
      
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

  ###Exception handling for UK test data----
  if ( source %in% uk_tables ) {
    if ( source == "uk_2010_siot") {
      labelled_io_data <- labelled_io_data %>%
        dplyr::filter ( .data$indicator == 'Input-Output table (domestic use, basic prices, product by product)')
    }
    
    if ( source == "uk_2010_use") {
      labelled_io_data <- labelled_io_data %>%
        dplyr::filter ( .data$indicator == 'Domestic use table at basic prices (product by industry)')
    }
    
    if ( source == "uk_2010_imports") {
      labelled_io_data <- labelled_io_data %>%
        dplyr::filter ( .data$indicator == 'Imports use table at basic prices (product by product)')
    }
    
    if ( source == "uk_2010_coeff") {
      labelled_io_data <- labelled_io_data %>%
        dplyr::filter ( .data$indicator == 'Matrix of coefficients (product by product)')
    }
    
    if ( source == "uk_2010_inverse") {
      labelled_io_data <- labelled_io_data %>%
        dplyr::filter ( .data$indicator == 'Leontief Inverse (product by product)')
    }
  } 
   
  ##Verifying parameters ----  
  year_input <- year
  source_inputed <- source   
  unit_input <- unit
  stk_flow_input <- stk_flow
  geo_input <- geo
  
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
  
  if ( ! year_input %in% labelled_io_data$year ) { 
    stop("This year is not found in the raw data frame.")
  }
  

 ## Selecting table from nested data, if nested at all ---------------  

  if ( ! source %in% c("croatia_2010_1700" , "croatia_2010_1800" , 
                       "croatia_2010_1900" , 
                       "germany_1990", uk_tables ) ) {
    selected_table <- which (   ## get the number of table to be selected
      labelled_io_data$year == year & 
        as.character(labelled_io_data$geo) == geo &
        labelled_io_data$unit == unit)
    
    if ( length( selected_table) == 0  )  {
      stop ( paste0("There is no available table for country ", geo_input, 
                    " in the year ", year, 
                    " with ", unit_input, " units.") )
    } else if (length( selected_table) == 3) { 
      selected_table <- which (   ##get the number of table to be selected
        labelled_io_data$year == year & 
          as.character(labelled_io_data$geo) == geo &
          labelled_io_data$unit == unit  &
          labelled_io_data$stk_flow == stk_flow_input
        )
    }  
    
    if (length(selected_table) != 1) {
      stop ( "The parameters geo=", geo, "; unit=",  unit_input, 
             "; stk_flow=", stk_flow_input, 
             "\ndo not select a unique table.")
    }
    
    iotable <- labelled_io_data$data[[selected_table]]  ## the relevant io table data in long form
  } else {  #if data is not nested
    iotable <- labelled_io_data 
  }

 ## Converting factors to numbers --------------------------------------
 if ( class(iotable$values) %in% c("character", "factor") ) {
    iotable$values = trimws(as.character(iotable$values), which = "both")
    iotable$values = as.numeric(iotable$values)
    message("Warning: original data was converted to numeric format.")
 }

 ## Get and order the SIOT-------  
 if ( source %in% prod_ind ) {
  col_join <- names ( iotable ) [ which( names(iotable) %in% c("induse", "induse_lab", "iotables_col", "uk_col") )] 
  row_join <- names ( iotable ) [ which( names(iotable) %in% c("prod_na", "prod_na_lab", "iotables_row", "uk_row") )] 

  remove_vars <- c("quadrant", "account_group", "variable", 
                   "group", "eu_prod_na")
  remove_vars  <- remove_vars [remove_vars %in% names (metadata_cols)]
  
  iotable_labelled <- iotable %>%
    dplyr::filter (stk_flow == stk_flow_input )  %>%
    dplyr::mutate_if ( is.factor, as.character ) %>%
    dplyr::left_join ( metadata_cols, by = col_join  ) %>%
    dplyr::select ( -one_of(remove_vars) ) %>%  #remove repeating columns before joining rows
    dplyr::mutate_if ( is.factor, as.character ) %>% 
    dplyr::left_join ( metadata_rows, by = row_join ) 
    
    if ( nrow (iotable_labelled) == 0 ) {
      stop ( "No rows found with geo = ", geo_input, " year = ", year, 
             " unit = ", unit, " and stk_flow = ", stk_flow_input, "." )
    }
  
    iotable_labelled <- iotable_labelled %>%
      dplyr::mutate ( prod_na = forcats::fct_reorder(prod_na, 
                                              as.numeric(row_order))) %>%
      dplyr::mutate ( induse  = forcats::fct_reorder(induse, 
                                             as.numeric(col_order))) 
    
    if ( all(c("iotables_row", "iotables_col") %in%  names (iotable_labelled)) ) {
      iotable_labelled <-  iotable_labelled %>%
        dplyr::mutate ( iotables_row = forcats::fct_reorder(iotables_row ,
                                                            as.numeric(row_order))) %>%
        dplyr::mutate ( iotables_col = forcats::fct_reorder(iotables_col, 
                                                            as.numeric(col_order)))
    }
   
  } else  {
    if ( ! source %in% croatia_files ){  # !prod_ind
      
      by_col <- names(iotable)[which ( names(iotable) %in% c("t_cols2", "t_cols2_lab", "iotables_col") )]
      by_row <- names(iotable)[which ( names(iotable) %in% c("t_rows2", "t_rows2_lab", "iotables_row") )]
      
      iotable_labelled <- iotable %>%
        mutate_if ( is.factor, as.character ) %>%
        left_join ( metadata_cols, by = by_col )  %>%
        left_join ( metadata_rows, by = by_row ) %>%
        arrange ( row_order, col_order )
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
      dplyr::mutate ( iotables_col = forcats::fct_reorder(iotables_col, 
                                                         as.numeric( col_order)))
  } #end of not prod_na cases

  if ( labelling == "iotables" ) {
    
    iotable_labelled_w <- iotable_labelled %>%
      dplyr::arrange (iotables_row, iotables_col) %>%
      dplyr::select ( all_of(c("iotables_col", "iotables_row", "values")) ) %>% 
      tidyr::spread (iotables_col, values)
    
  } else if ( labelling == "short" & source %in% prod_ind ) {
    
    iotable_labelled_w <- iotable_labelled %>%
      dplyr::select (.data$prod_na, .data$induse, .data$values) %>%
      dplyr::filter ( !is.na(.data$prod_na) )  %>%
      tidyr::spread (induse, values )

  } else {
    iotable_labelled_w <- iotable_labelled %>%
      dplyr::select ( all_of(c("t_rows2", "t_cols2", "values")) ) %>%
      tidyr::spread ( t_cols2, values )
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


