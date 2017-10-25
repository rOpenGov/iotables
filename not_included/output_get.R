#' Get the total output vector
#'
#' @param labelled_io_data An IO data frame created with the get_io_data () function.
#' @param geo A country code or a country name, defaults to "SK" that could be written as "Slovakia", too.
#' @param technology A technology data frame created with the create_technology_data () function. Defaults to NULL in which case it tries to create the technology data with the function.
#' @param year A numeric variable containing the year. 
#' @param unit A character string containing the currency unit, defaults to "MIO_NAC" (million national currency unit). The alternative is "MIO_EUR". 
#' @param named Defaults to TRUE returns a tibble containing the technology raws as a key column. FALSE returns a matrix without the key row. 
#' @param households A logical variable weather to close the model with respect to the households. The default is FALSE. If true, it will include the payments to the household sector and the use of the household sector. 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename
#' @examples
#'  data (germany_1990)
#'  output <- output_get( labelled_io_data = germany_1990, geo = "DE", 
#'                           year = 1990, unit = "M_EUR", named = TRUE, 
#'                           households = FALSE ) 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename
#' @export

output_get <- function ( labelled_io_data, technology = NULL,
                         geo = "SK",
                         year = 2010, 
                         unit = "MIO_NAC",
                         named = TRUE ,
                         households = FALSE) {
  time = NULL; t_cols2 = NULL; t_rows2 = NULL; values = NULL #non-standard evaluation creates a varning in build. 
  geo_input <- geo; unit_input <- unit
  
  if (is.null(geo)) stop ("Error: no country selected.")
  
  if ( nchar(geo) == 2 & geo == tolower(geo)) { 
    geo <- toupper(geo)
    warning("Warning: country code changed to upper case.")
  }
  if ( ! "data.frame" %in%  class(technology)) {
    if ( geo == "HR" & year == 2010 ) { source <- "dzs" } else { source <- "eurostat"}
    technology <- get_technology_data(labelled_io_data  = labelled_io_data, 
                                      source = source, geo = geo, year = year )
  }
  
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
  
  if (geo %in% available_country_names) {
    geo <- as.character(labelled_io_data$geo[which(labelled_io_data$geo_lab == geo)][1])
  } #end of checking basic errors
  
  output_name <- c("P1", "TUBP", "TU", "output_basic")
  #if ( year == 1990 & geo == "DE") output_name <- "output_basic"
  
  output <- labelled_io_data %>%
    dplyr::filter ( geo == geo_input)  %>%
    dplyr::filter ( time == paste0(year, "-01-01" )) %>%
    dplyr::filter ( unit == unit_input ) %>%
    dplyr::filter ( t_rows2 %in% technology$t_rows2 ) %>%
    dplyr::filter ( t_cols2  %in% output_name ) %>%  
    dplyr::select ( t_rows2, values ) %>%
    dplyr::rename ( output = values ) %>%
    dplyr::mutate ( t_rows2 = as.character(t_rows2)) %>%
    select ( t_rows2, output )
  
  if (households == TRUE) {
   output <- rbind ( output, c("P3_S14", 0))  #The earnings on household consumption is 0 by definiton.
  }
  
  output$output <- as.numeric ( output$output )
  if ( named == TRUE )  return ( output ) else {
    return ( as.vector (output$output) )
  } #end of returning data
} #end of function