#' Get the input coefficients for value added vector
#'
#' @param labelled_io_data An IO data frame created with the \code{\link{get_io_current_prices}} function.
#' @param geo A country code or a country name, defaults to "SK" that could be written as "Slovakia", too.
#' @param technology A technology data frame created with the \code{\link{get_technology_data}} function. Defaults to "" in which case it tries to create the technology data with the function.
#' @param year A numeric variable containing the year. 
#' @param unit A character string containing the currency unit, defaults to "MIO_NAC" (million national currency unit). The alternative is "MIO_EUR". 
#' @param named Defaults to TRUE and returns a matrix. TRUE returns a tibble containing the technology raws as a key column. The FALSE returns only a numeric vector.
#'
#' @examples
#'  data (germany_1995)
#'  value_added <- input_value_added_get( labelled_io_data = germany_1995, 
#'                           technology = NULL, geo = "DE", 
#'                           year = 1990, unit = "M_EUR", named = TRUE) 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr spread
#' @export

input_value_added_get <- function (labelled_io_data, technology = NULL,
                                  geo = "SK",
                                  year = 2010, 
                                  unit = "MIO_NAC",
                                  named = TRUE ) {
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
  
  va_name <- "B1G"
  if ( year == 1990 & geo == "DE") va_name <- "value_prices"  #names of the value added row

  iva <- labelled_io_data %>%
    filter ( geo == geo_input )  %>%
    filter ( time == paste0(year, "-01-01" )) %>%
    filter ( unit == unit_input ) %>%
    filter ( t_rows2 %in% va_name ) %>%
    filter ( t_cols2 %in% technology$t_cols2 )%>%  
    select ( t_cols2, t_rows2, values )  %>%
    mutate ( t_rows2 = as.character(t_rows2)) %>%
    spread (t_cols2, values )

  if (named) return ( iva ) else {
    return(as.vector(t(iva[1,2:ncol(iva)])))}
} #end of function

