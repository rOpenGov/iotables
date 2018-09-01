#' Get an output vector
#' 
#' @param source A data source, for example "naio_10_cp1700". Possible codes are "naio_10_cp1700",
#' "naio_10_cp1750", "naio_10_pyp1700", "naio_10_pyp1750", "naio_10_cp1620", "naio_10_pyp1620",
#' "naio_10_cp1630", "naio_10_pyp1630", "croatia_2010_1700", "croatia_2010_1800", 
#' "croatia_2010_1900". For further information consult the 
#' \href{http://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/methodology/symmetric-input-output-tables}{Eurostat Symmetric Input-Output Tables} page.
#' @param labelled_io_table If you have created the IO table earlier with 
#' \code{\link{iotable_get}}, it is faster to work with the data
#' in the memory. Defaults to \code{NULL} when  the data will be retrieved from
#' the hard disk or from the Eurostat website invoking \code{\link{iotables_download}} 
#' and \code{\link{iotable_get}}
#' @param geo A country code or a country name.  For example, \code{SK} for Slovakia.
#' @param year A numeric variable containing the year. Defaults to 2010, because this year has the most data. 
#' @param unit A character string containing the currency unit, defaults to \code{MIO_NAC} (million national currency unit). The alternative is \code{MIO_EUR}. 
#' @param stk_flow Defaults to \code{DOM}, alternative \code{IMP}. 
#' @param households If you need to make household demand endogenous, or "close the households off", TRUE selects 
#' wages and final household consumption. This is needed for induced-effects calculations.
#' @param labelling Defaults to \code{iotables} which gives standard row and column names regardless of the
#' source of the table, or if it is a product x product, industry x industry or product x industry table.
#' The alternative is \code{short} which is the original short row or column code of Eurostat or OECD.
#' @param keep_total Logical variable. Defaults to \code{FALSE} and removes the totaling row and column from the matrix.  
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join mutate_if arrange
#' @importFrom tidyr gather spread 
#' @importFrom forcats fct_reorder
#' @examples 
#' output_get (  source = "germany_1990", geo = "DE",
#'               year = 1990, unit = "MIO_EUR", 
#'               stk_flow = "DOM", labelling = "iotables")
#'\dontrun{
#' output <-  iotables_download ( "naio_10_cp1700" ) %>%
#'  iotable_get (labelled_io_data = ., geo = "CZ", 
#'               source = "naio_10_cp1700",
#'               year = 2015, unit = "MIO_NAC", 
#'               labelling = "short") %>% 
#'  output_get ()
#' }                              
#' @export 

output_get <- function ( labelled_io_table = NULL,
                         source = "germany_1990", geo = "DE",
                         year = 1990, unit = "MIO_EUR",
                         households = FALSE,  
                         labelling = "iotables", 
                         stk_flow = "DOM", 
                         keep_total = FALSE) {  
  ##Initialize variables ------------
  time <- t_cols2 <- t_rows2 <- values <- . <- NULL #non-standard evaluation creates a varning in build. 
  iotables_row <- iotables_col <- prod_na <- induse <- NULL
  unit_input <- unit ; geo_input <- geo; stk_flow_input <- stk_flow
  source_inputed <- source 
  
  tmp_rds <- file.path(tempdir(), paste0(source, "_", labelling, ".rds"))
  
  if ( is.null ( labelled_io_table ) ) { #no pre-downloaded table inserted
    ##Exception handling --------------
    if ( source == "croatia_2010_1900" ) {
      stop("The table croatia_2010_1900 is an import table and has no output field.")
    }
    if (! labelling %in% c("iotables", "short")) {
      stop("Only iotables or original short columns can be selected.")
    }
    
    ##Germany----
    if ( source == "germany_1990" ) {
      labelled_io_table <- iotable_get ( source = "germany_1990", 
                                         geo = geo_input, year = year, 
                                         unit = unit_input,
                                         labelling = labelling )     # use germany example 
      output_vector <- labelled_io_table[16,]
      household_col <- which  (names(output_vector) == "consumption_expenditure_household")
      if ( households == TRUE ) {
          output_vector <- output_vector[1, 1:household_col] 
        } else {
        output_vector <- output_vector [1,1:(household_col-1)]
      }
      return ( output_vector )  #return simplified example table and do not run rest of the code
    } else {
      if ( tmp_rds %in% list.files (path = tempdir()) ) {
        labelled_io_table <- readRDS( tmp_rds ) #if already downloaded and saved as rds 
      } else { 
        labelled_io_table <- iotable_get ( source = source, 
                                           geo = geo_input, year = year, 
                                           unit = unit_input, labelling = labelling,
                                           stk_flow = stk_flow_input) }
    } # use eurostat files 
    
  } 
  
  labelled_io_table <- labelled_io_table %>% 
    dplyr::mutate_if ( is.factor, as.character)
 
  total_col <- NA
  if ( any ( tolower(names(labelled_io_table)) %in% c("total", "cpa_total") )) {
    total_col <- which (names ( labelled_io_table ) == "TOTAL") #find the total column, position varies if L68 or G47 is missing
  }
    

###Inclusion or exclusion of households------
    
  if ( households == TRUE ) {
###households to be kept------    
    household_consumption_col <- which ( names (labelled_io_table ) %in% 
                                           c('final_consumption_households', 'P3_S14'))
    
    if (length( household_consumption_col) > 1 ) {
      warning ( "Beware, more household consumption items were found in the table.")
    }
    if ( length( household_consumption_col) == 0 ) {
      stop ( "No household consumption data was found.")
    }
    output_row <- which (labelled_io_table[[1]] %in%  
                                     c('output_bp', 'P1', 'output') )
   
    if ( length( output_row) == 0 ) {
      stop ( "No output data was found.")
    }
    
    if (keep_total == TRUE) {
      message ( "Households were added to the matrix.")
      output_vector <- labelled_io_table[output_row[1] , 
                                          c(1:total_col, household_consumption_col[1]) ] 
      output_vector [1,total_col+1] <- 0
    } else {
      message ( "Households were added to the matrix.")
      message ( "Total column was removed from the matrix.")
      output_vector <- labelled_io_table[    output_row[1] , 
                                             c(1:total_col-1,
                                               household_consumption_col[1]) ] 
      output_vector [1,total_col] <- 0
    }
  } else {  
###no households case-----
    output_row <- which (labelled_io_table[[1]] %in%  
                           c('output_bp', 'P1', 'output') )
    
    if (source == "naio_cp17_r2") {
      output_row <- which (labelled_io_table[[1]] %in%  
                             c('total', "CPA_TOTAL") )
    }
    
    if ( length(output_row) == 0 ) {
      stop ( "No output data was found.")
    }
    
    if ( length(output_row) > 1 ) {
      warning ( "Multiple output rows were found, using first.")
    }
    
    
    ##Keep total column?
    if ( keep_total == TRUE && !is.na(total_col) ) {
      output_vector <- labelled_io_table[ output_row[1], 1:total_col   ] 
    } else {
      if ( is.na(total_col)) { total_col <- ncol(labelled_io_table) }
      output_vector <- labelled_io_table[ output_row[1], 1:total_col-1 ]
      message ( "Total column was not found or removed from the matrix.")
    }
    
  } # end of no household case 
  
  output_vector 
}


