#' Get an input-output table from bulk file
#' 
#' This function is used to filter out  a single input-output table from a database, 
#' for example a raw file downloaded from the Eurostat website.  It provides some 
#' functionality to avoid some pitfalls.
#' Unless you want to work with bulk data files, you should not invoke  \code{\link{iotables_download}} 
#' directly, rather via this function, if and when it is necessary. 
#' @param source A data source, for example "naio_10_cp1700".
#' @param geo A country code or a country name.  For example, "SK" or as "Slovakia".
#' @param year A numeric variable containing the year. Defaults to 2010, because this year has the most data. 
#' @param unit A character string containing the currency unit, defaults to "MIO_NAC" (million national currency unit). The alternative is "MIO_EUR". 
#' @param stk_flow Defaults to "DOM", alternative "IMP". 
#' @param labelling Defaults to "iotables" which gives standard row and column names regardless of the
#' source of the table, or if it is a product x product, industry x industry or product x industry table.
#' The alternative is "short" which is the original short row or column code of Eurostat or OECD.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename left_join arrange mutate_if
#' @importFrom tidyr spread
#' @importFrom forcats fct_reorder
#' @examples 
#' germany_table <- iotable_get( source = "germany_1990", geo = 'DE', year = 1990,
#'              unit = "MIO_EUR", labelling  = 'iotables')
#' @export 

iotable_get <- function ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          stk_flow = "DOM", labelling = "iotables") {  
  time = NULL; t_cols2 = NULL; t_rows2 = NULL; 
  values = NULL ; .= NULL #non-standard evaluation creates a varning in build. 
  iotables_row <- iotables_col <- prod_na <- induse <- variable <-  NULL
  unit_input <- unit ; geo_input <- geo; stk_flow_input <- stk_flow
  row_order <- col_order <- iotables_label <- code <- numeric_label <- label <- NULL
  
  if (is.null(geo)) stop ("Error: no country selected.")
  if (! labelling %in% c("iotables", "short")) {
    stop("Only iotables or original short columns can be selected.")
  }
  source_inputed <- source

##Veryfing source parameter and loading the labelling  ----
  prod_ind <- c("naio_10_cp1700", "naio_10_cp1750", "naio_10_pyp1700", "naio_10_pyp1750")
  trow_tcol <-  c("naio_cp17_r2", "naio_17_agg_60_r2", "naio_17_agg_10_r2", 
                  "croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")
  croatia_files <- c( "croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")
  
  if ( source %in% prod_ind ) { 
    metadata_rows <- iotables::metadata %>%  #tables that follow prod_ind vocabulary
       filter ( variable == "prod_na") %>%
       dplyr::rename ( prod_na = code) %>%
       dplyr::rename ( prod_na_lab = label ) %>%
       dplyr::rename ( row_order = numeric_label ) %>%
       dplyr::rename ( iotables_row = iotables_label )
       
    metadata_cols <- iotables::metadata %>%
      filter ( variable == "induse") %>%
      dplyr::rename ( induse = code) %>%
      dplyr::rename ( induse_lab = label )%>%
      dplyr::rename ( col_order = numeric_label ) %>%
      dplyr::rename ( iotables_col = iotables_label )
    
  } else if ( source %in% trow_tcol ) {   #tables that follow trow_tcol vocabulary
    
    metadata_rows <- iotables::metadata %>%
      filter ( variable == "t_rows") %>%
      dplyr::rename ( t_rows2 = code) %>%
      dplyr::rename ( t_rows2_lab = label ) %>%
      dplyr::rename ( row_order = numeric_label ) %>%
      dplyr::rename ( iotables_row = iotables_label )
    
    metadata_cols <- iotables::metadata %>%
      filter ( variable == "t_cols") %>%
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
  
##Creating a temporary file name for the input-output table ----
  tmp_rds <- paste0(tempdir(), "\\", source, "_", labelling, ".rds")
  
##Loading the data and Veryfing other parameters ----  
   if ( nchar(geo) == 2 & geo == tolower(geo)) { 
    geo_input <- toupper(geo)
    geo <- toupper (geo)
    warning("Warning: country code changed to upper case.")
  }
  
  if ( source == "germany_1990") {
    labelled_io_data <- iotables::germany_1990    # use germany example 
  } else if ( source == "croatia_2010_1700" ) { 
    labelled_io_data <- iotables::croatia_2010_1700 
  } else if ( source == "croatia_2010_1800" )  {
    labelled_io_data <- iotables::croatia_2010_1800  
    } else if ( source == "croatia_2010_1900" )  {
      labelled_io_data <- iotables::croatia_2010_1900
    } else {
    if ( tmp_rds %in% list.files (path = tempdir()) ) {
      labelled_io_data <- readRDS( tmp_rds ) 
    } else { 
      labelled_io_data <- iotables_download ( source, 
                                              stk_flow = stk_flow_input ) 
      }
  } # use eurostat files 
  
  if ( ! unit_input %in% labelled_io_data$unit ) { 
    stop("This currency unit is not found in the raw data frame.")
  }
  
##Veryifing year and country name input ---
  available_years <- unique (as.numeric(substr(as.character(labelled_io_data$time),1,4)))
  available_country_codes <- as.character(unique(labelled_io_data$geo))
  available_country_names <- as.character(unique(labelled_io_data$geo_lab))
  available_countries <- c(available_country_codes, available_country_names)
  
  if ( ! year %in% available_years) {
    stop("Error: no data is available for this year.")
  }
  if ( ! geo %in% available_countries ) {
    stop("Error: no data is available for this country or geographical unit.")
  }
  if (geo %in% available_country_names) {
    geo <- as.character(labelled_io_data$geo[which(labelled_io_data$geo_lab == geo)][1])
  }
  if ( class(labelled_io_data$values) %in% c("character", "factor")) {
    labelled_io_data$values  = trimws(as.character(labelled_io_data$values), which = "both")
    labelled_io_data$values = as.numeric(labelled_io_data$values)
    warning("Warning: original data was converted to numeric format.")
  }
  
  iotable <- labelled_io_data   %>%
    dplyr::filter ( geo == geo_input )  %>%
    dplyr::filter ( time == as.Date(paste0(as.character(year), "-01-01" ))) %>%
    dplyr::filter ( unit == unit_input ) 
  
  if ( source %in% prod_ind ) {
    iotable_labelled <- iotable %>%
      mutate_if ( is.factor, as.character ) %>%
      left_join (., metadata_cols, by = c("induse", "induse_lab"))  %>%
      left_join ( ., metadata_rows, by = c("prod_na", "prod_na_lab")) %>%
      mutate ( prod_na = forcats::fct_reorder(prod_na, 
                                              as.numeric(row_order))) %>%
      mutate ( induse = forcats::fct_reorder(induse, 
                                             as.numeric(col_order))) %>%
      mutate ( iotables_row = forcats::fct_reorder(iotables_row ,
                                                       as.numeric(row_order))) %>%
      mutate ( iotables_col = forcats::fct_reorder(iotables_col, 
                                                       as.numeric(col_order))) %>%
      filter ( stk_flow == stk_flow_input )
  } else  {
    if ( ! source %in% croatia_files ){
      iotable_labelled <- iotable %>%
         mutate_if ( is.factor, as.character ) %>%
        left_join (., metadata_cols, by = c("t_cols2", "t_cols2_lab"))  %>%
        left_join ( ., metadata_rows, by = c("t_rows2", "t_rows2_lab")) %>%
        arrange ( row_order, col_order )
    } else {
      iotable_labelled <- iotable 
    }
    iotable_labelled <- iotable_labelled %>%
      mutate ( t_rows2 = forcats::fct_reorder(t_rows2, 
                                              as.numeric( row_order))) %>%
      mutate ( t_cols2 = forcats::fct_reorder(t_cols2, 
                                              as.numeric( col_order ))) %>%
      mutate ( iotables_row = forcats::fct_reorder(iotables_row , 
                                                       as.numeric(row_order))) %>%
      mutate ( iotables_col = forcats::fct_reorder(iotables_col, 
                                                         as.numeric( col_order)))
  }

  if ( labelling == "iotables") {
    iotable_labelled_w <- iotable_labelled %>%
      arrange ( iotables_row, iotables_col) %>%
      select ( iotables_col, iotables_row, values ) %>% 
      tidyr::spread ( iotables_col, values )
    
  } else if ( labelling == "short" & source %in% prod_ind ) {
    iotable_labelled_w <- iotable_labelled %>%
      select (prod_na, induse, values ) %>%
      tidyr::spread (induse, values )
  } else {
    iotable_labelled_w <- iotable_labelled %>%
      select ( t_rows2, t_cols2, values ) %>%
      tidyr::spread (t_cols2, values )
  }
  
 return( iotable_labelled_w  )
}


