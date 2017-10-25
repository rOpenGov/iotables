#' Create a catalog of the technology rows and columns.
#
#' @param labelled_io_data An IO data frame created with the get_io_data () function.
#' @param source Defaults to "eurostat" in which case the Eurostat-styled technology description is followed. Later OECD Stan will be available, too. 
#' @param geo A country code or a country name, defaults to "SK" that could be written as "Slovakia", too.
#' @param year Defaults to 2010. 
#' @param unit Currency unit, defaults to "MIO_NAC" as million national currency, the other possibility is "M_EUR".
#' @param households A logical variable weather to close the model with respect to the households. The default is FALSE. If true, it will include the payments to the household sector and the use of the household sector. 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate distinct full_join
#' @examples
#' data (germany_1990)
#' get_technology_data(germany_1990, geo = 'DE', year = 1990, 
#'                     unit = "M_EUR", households = FALSE) 
#' @export

get_technology_data <- function ( labelled_io_data, 
                                  source = "eurostat", 
                                  geo = "SK", 
                                  unit = "MIO_NAC", 
                                  year = 2010,
                                  households = FALSE ) {
  time = NULL; t_cols2 = NULL; t_cols2_lab = NULL;
  t_rows2 = NULL; t_rows2_lab = NULL; values = NULL #non-standard evaluation creates a varning in build. 
  time <- as.Date (paste0( year, "-01-01"))
  source <- tolower(source)
  if ( nchar(geo) == 2 & geo == tolower(geo)) { 
    geo <- toupper(geo) 
    warning("Warning: Country code converted to uppper case.") 
  }
  if ( ! source %in% c("eurostat", "dzs")) {
    stop ("Error: currently only Eurostat and DZS data is handled.")
  }
  if ( source == 'dzs') {
    unit = "T_NAC"; geo = "HR" ; time = "2010-01-01"}
  
  if ( ! "data.frame" %in% class(labelled_io_data) ) stop (
    "Error: input is not a data.frame, tbl_df or tibble.")

  if ( ! all(
    c("geo", "time", "unit", "t_cols2", "t_rows2", "t_rows2_lab") %in% 
     names (labelled_io_data) )) {
    stop ("Error: Cannot find the Eurostat-style metadata in the inputed data frame.")
  }
  
  if ( ! unit %in% c("MIO_NAC", "M_EUR", "T_NAC") ) {
    stop ("Error: This currency unit is not found in the raww data.")
  }
  
  compensation <- "D1"  #Compensation of employees
  household_expenditure <- "P3_S14" #Final consumption expenditure by households

  technology_rows_data <- labelled_io_data %>%
    filter ( time == time) %>%
    filter ( geo  == geo) %>%
    filter ( unit == unit ) %>%
    distinct ( t_rows2, t_rows2_lab) %>%
    filter ( substr(t_rows2, 1,4) ==  "CPA_" | t_rows2 == compensation) %>%
    filter ( ! t_rows2 %in% c("CPA_T", "CPA_U", "CPA_TOTAL", "B1G")) %>%
    select ( t_rows2, t_rows2_lab) %>%
    mutate ( code = as.character(t_rows2))

  technology_cols_data <- labelled_io_data %>%
    filter ( time == time ) %>%
    filter ( geo  == geo) %>%
    filter ( unit == unit ) %>%
    distinct ( t_cols2, t_cols2_lab) 
  

  technology_cols_data <- filter (technology_cols_data, 
                                  substr(t_cols2, 1,4) ==  "CPA_" |
                                  t_cols2 == household_expenditure ) %>%
      filter ( ! t_cols2 %in% c("CPA_T", "CPA_U", "CPA_TOTAL", "B1G")) 
    
  technology_cols_data <- technology_cols_data %>%
       select ( t_cols2, t_cols2_lab) %>%
    mutate ( code = as.character(t_cols2) )
  
  if (households == FALSE ) {
    technology_cols_data <- technology_cols_data %>%
      filter ( t_cols2 != household_expenditure )
    
    technology_rows_data <- technology_rows_data %>%
      filter ( t_rows2 !=  compensation)
  }

  if ( source == "eurostat") {
    technology_data <- full_join ( technology_rows_data ,
                                   technology_cols_data,
                                   by = "code" )  %>%
      select ( t_rows2, t_cols2, t_rows2_lab, t_cols2_lab)
  } else if ( source == "dzs") {
    technology_data = cbind(technology_rows_data ,
                            technology_cols_data) 
    technology_data <- subset ( technology_data , 
                                select = c("t_rows2", "t_cols2", 
                                           "t_rows2_lab", "t_cols2_lab") )
  } #fails here 
 
  return ( technology_data)
}
