#' Get primary inputs
#'
#' This function will retrieve any primary input from the input-output table. 
#' You can use the iotables or the original (Eurostat) short labels to select the 
#' primary input.
#' If you work with the original Eurostat labels, you can review the codes of
#' variables with \code{View(metadata)}.
#' @param input A character string or a character vector containing the indicator names. 
#' Any of \code{compensation_employees}, \code{wages_salaries}, \code{mixed_income_gross}, 
#' \code{gva} (for gross value added), \code{surplus_mixed_gross}, \code{surplus_mixed_net},
#' \code{net_tax_production}, \code{import_goods_services} (and its breakup ..._MU, 
#'  _non_MU, ..._EU, ...non_EU). If the indicator is not found in the table, you 
#'  will get an error. The input parameter is case sensitive. 
#' @param source A data source, for example \code{naio_10_cp1700}. Possible codes are \code{naio_10_cp1700},
#' \code{naio_10_cp1750}, \code{naio_10_pyp1700}, \code{naio_10_pyp1750}, \code{naio_cp17_r2}, \code{naio_17_agg_60_r2}, 
#' \code{naio_17_agg_10_r2}, \code{croatia_2010_1700}, \code{croatia_2010_1800}, 
#' \code{croatia_2010_1900}. For further information consult the 
#' \href{http://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/methodology/symmetric-input-output-tables}{Eurostat Symmetric Input-Output Tables} page.
#' @param geo A country code or a country name, defaults to \code{SK} that 
#' could be written as \code{Slovakia}, too.
#' @param year A numeric variable containing the year. 
#' @param unit A character string containing the currency unit, defaults to \code{MIO_NAC} (million national currency unit). 
#' The alternative is \code{MIO_EUR}.
#' @param households If the household are included in your model (adds final 
#' household expenditure column). Defaults to \code{FALSE}.
#' @param stk_flow Defaults to \code{DOM}, alternative \code{IMP}. 
#' @param labelling Defaults to \code{iotables} which gives standard row and column 
#' names regardless of the
#' source of the table, or if it is a product x product, industry x industry or 
#' product x industry table.
#' The alternative is \code{short} which is the original short row or column code of 
#' Eurostat or OECD.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr spread
#' @importFrom utils data 
#' @examples
#' comp_employees_de <- primary_input_get(
#'                             input = "compensation_employees", 
#'                             source = "germany_1990", geo = "DE", 
#'                             unit = "MIO_EUR", 
#'                             year = 1990, labelling = "iotables" ) 
#' @export

primary_input_get <- function ( input = "compensation_employees", 
                                source = "germany_1990", geo = "DE",
                                year = 1990, unit = "MIO_EUR",
                                households = FALSE, stk_flow = "DOM",
                                labelling = "iotables") {
  time <- t_cols2 <- t_rows2 <- values <- .<-  NULL #non-standard evaluation creates a varning in build. 
  iotables_row <- iotables_col <- prod_na <- induse <- NULL
  unit_input <- unit; geo_input <- geo;  stk_flow_input <- stk_flow
  tmp_rds <- paste0(tempdir(), "\\", source, "_", labelling, ".rds")
  source_inputed <- source ; unit_input <- unit
  
  if (source == "croatia_2010_1900") {
    stop("The table croatia_2010_1900 is an import table and has no primary input field.")
  }
  if (! labelling %in% c("iotables", "short")) {
    stop("Only iotables or original short columns can be selected.")
  }
  
  if ( source == "germany_1990") {
    labelled_io_table <- iotable_get ( source = "germany_1990", 
                                       geo = geo_input, year = year, 
                                       unit = unit_input, labelling = labelling )     # use germany example 
    if ( input %in% labelled_io_table[[1]] ) {
      input_row <- which ( labelled_io_table[[1]] == input )
    } else {
      stop("The input is not found in this data source.")
    }
    input_vector <- labelled_io_table[input_row,]
    if (households == TRUE ) {
      input_vector <- input_vector [1,1:8]
    } else {
      input_vector <- input_vector [1,1:7]
    }
    return ( input_vector )  #return simplified example table and do not run rest of the code
  } else {                   #end of germany case
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
  
  if ( input %in% labelled_io_table[[1]] ) {
    input_row <- which ( labelled_io_table[[1]] == input )
  } else {
    stop("The input is not found in this data source.")
  }
  
  if (households == TRUE) {
    household_consumption_col <- which ( names (labelled_io_table ) %in% 
                                           c('final_consumption_households', 'P3_S14'))
    
    if (length( household_consumption_col) > 1 ) {
      warning ( "Beware, more household consumption items were found in the table.")
    }
    if ( length( household_consumption_col) == 0 ) {
      stop ( "No household consumption data was found.")
    }
    message ( "Households are added to the matrix.")
    input_vector <- labelled_io_table[    input_row, 
                                          c(1:67, household_consumption_col[1]) ] 
    input_vector[1, 68] <- 0
    } else {    #no households 
      input_vector <- labelled_io_table[input_row, c(1:67) ] 
    if ( length( input_vector) == 0 ) {
      stop ( "No primary input data was found.")
    }
  } # end of no household case 
  input_vector[,1] <- as.character(input_vector[,1])
  
  input_vector 
}


