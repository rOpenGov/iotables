#' Create a use (input flow) matrix
#' 
#' @param source A data source, for example "naio_10_cp1700".
#' @param geo A country code or a country name.  For example, "SK" or as "Slovakia".
#' @param year A numeric variable containing the year. Defaults to 2010, because this year has the most data. 
#' @param unit A character string containing the currency unit, defaults to "MIO_NAC" (million national currency unit). The alternative is "MIO_EUR". 
#' @param households If you need to make household demand endogenous, or "close the households off", TRUE selects 
#' wages and final household consumption. This is needed for induced-effects calculations.
#' @param stk_flow Defaults to "DOM", alternative "IMP". 
#' @param keep_total Logical variable. Defaults to FALSE and removes the totalling row and column from the matrix.  
#' @param labelling Defaults to "iotables" which gives standard row and column names regardless of the
#' source of the table, or if it is a product x product, industry x industry or product x industry table.
#' The alternative is "short" which is the original short row or column code of Eurostat or OECD.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join mutate_if arrange
#' @importFrom tidyr gather spread 
#' @importFrom forcats fct_reorder
#' @examples 
#' use_de <- use_table_get( source = "germany_1990", geo = 'DE', year = 1990,
#'                        unit = "MIO_EUR", labelling  = 'iotables')
#' \dontrun{
#'sk_use_1700_2 <- use_table_get2 ( source = "naio_10_cp1700", geo = "SK",
#'                                  year = 2010, unit = "MIO_EUR",
#'                                  stk_flow = "DOM", households = TRUE,
#'                                  keep_total = TRUE)
#'  }

#' @export 

use_table_get <- function ( source = "germany_1990", geo = "DE",
                            year = 1990, unit = "MIO_EUR", stk_flow = "DOM",
                            households = FALSE, keep_total = FALSE, 
                            labelling = "iotables" ){  
  
  time = NULL; t_cols2 = NULL; t_rows2 = NULL; values = NULL ;.= NULL #non-standard evaluation creates a varning in build. 
  iotables_row =NULL; iotables_col = NULL; prod_na = NULL; induse = NULL
  unit_input <- unit ; geo_input <- geo; stk_flow_input <- stk_flow

  tmp_rds <- paste0(tempdir(), "\\", source, "_", labelling, ".rds")
  source_inputed <- source ; unit_input = unit
  
  if ( ! labelling %in% c("iotables", "short")) {
    stop("Currently only labelling = 'iotables' and labelling = 'short' is supported.")
  }
  
  if ( source == "germany_1990") {
    labelled_io_table <- iotable_get ( source, geo, year, unit, labelling )     # use germany example 
    use_table <- labelled_io_table[1:7, 1:8]
    if (keep_total == FALSE ) use_table <- use_table [1:6, 1:8]
    if (households == TRUE ) {
      household_row <- labelled_io_table[11,1:8]
      household_row[1,8] <- 0
      use_table <- rbind ( use_table, household_row)
      if ( keep_total == FALSE) use_table <- use_table [ -7, ]
      } else {
        if (keep_total == TRUE) {
          use_table <- labelled_io_table[1:7, 1:7]
        } else {
          use_table <- labelled_io_table[1:6, 1:7]
        }
     }
    return ( use_table )  #return simplified example table and do not run rest of the code
    } else {
    if ( tmp_rds %in% list.files (path = tempdir()) ) {
      labelled_io_table <- readRDS( tmp_rds ) #if already downloaded and saved as rds 
    } else { 
      labelled_io_table <- iotable_get ( source = source, 
                                         geo = geo_input, year = year, 
                                         unit = unit_input, labelling = labelling,
                                         stk_flow = stk_flow_input) }
  } # use eurostat files 

 labelled_io_table <- labelled_io_table %>% 
    mutate_if ( is.factor, as.character)
  
 use_table <- labelled_io_table[c(1:66), 1:67]

 if (households == TRUE) {
   household_consumption_col <- which ( names (labelled_io_table ) %in% 
              c('final_consumption_households', 'P3_S14'))
   
   if (length( household_consumption_col) > 1 ) {
     warning ( "Beware, more household consumption items were found in the table.")
   }
   if ( length( household_consumption_col) == 0 ) {
     stop ( "No household consumption data was found.")
   }
   household_income_row <- which (labelled_io_table[[1]] %in%  
                                    c('wages_salaries', 'D11') )
   
   if  (length( household_income_row) < 1 ) {
     household_income_rowl <- which ( labelled_io_table[[1]] %in% 
                                        c('compensation_employees', 'D1'))
   }
   if ( length( household_income_row) == 0 ) {
     stop ( "No household income data was found.")
   }
   
   message ( "Households are added to the matrix.")
   use_table <- labelled_io_table[    c(1:66,household_income_row[1]) , 
                                  c(1:67, household_consumption_col[1]) ] 
   use_table [67,68] <- 0
   if (keep_total == FALSE ) {
     use_table <- use_table [-66,-67]
     message ( "Total row and column removed from the matrix.")
   }
     
   } else {    #no households 
   if (keep_total == FALSE )  {
     use_table <- use_table [1:65,1:66]
     message ( "Total row and column removed from the matrix.")
   } else {
     use_table <- labelled_io_table[1:66, 1:67]
     }
   } # end of no household case 
    return ( use_table ) 
}


