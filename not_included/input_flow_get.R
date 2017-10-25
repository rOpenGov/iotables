#' Create an input flow matrix
#' 
#' @param labelled_io_data An IO data frame created with the \code{\link{get_io_current_prices}} function. 
#' @param technology A technology data frame created with the \code{\link{get_technology_data}} function. 
#' Defaults to NULL in which case it tries to create the technology data with the function.
#' @param geo A country code or a country name.  For example, "SK" or as "Slovakia".
#' @param year A numeric variable containing the year. Defaults to 2010, because this year has the most data. 
#' @param unit A character string containing the currency unit, defaults to "MIO_NAC" (million national currency unit). The alternative is "MIO_EUR". 
#' @param named Defaults to TRUE and returns a tibble containing the technology raws as a key column. FALSE returns a matrix without key column.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr spread
#' @examples 
#' data (germany_1990)
#' input_flow_get(labelled_io_data = germany_1990, geo = 'DE', year = 1990,
#'                unit = "M_EUR", named = FALSE)
#' @export 

input_flow_get <- function ( labelled_io_data, technology = NULL,
                                geo = NULL,
                                year = 2010, 
                                unit = "MIO_NAC", 
                                named = TRUE) {  
  
  time = NULL; t_cols2 = NULL; t_rows2 = NULL; values = NULL ;.= NULL #non-standard evaluation creates a varning in build. 
  if (is.null(geo)) stop ("Error: no country selected.")
  unit_input <- unit ; geo_input <- geo;
   
  if ( nchar(geo) == 2 & geo == tolower(geo)) { 
    geo <- toupper(geo)
    warning("Warning: country code changed to upper case.")
  }
  if ( ! "data.frame" %in%  class(technology)) {
    if (nrow(labelled_io_data) > 10000 ) {
      warning("The data file is large, creating the technology data frame may take\na good few seconds.")
    }
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
  if (!  unit_input %in% labelled_io_data$unit ) {
    stop("Error: no data is available with this currency unit in the input data frame.")
  }
    labelled_io_data <- labelled_io_data  %>%
    dplyr::select ( geo, time, unit, t_cols2, t_rows2, values )  %>%
    dplyr::filter ( geo == geo_input )  %>%
    dplyr::filter ( time == as.Date(paste0(as.character(year), "-01-01" ))) %>%
    dplyr::filter ( unit == unit_input )   %>%
    dplyr::select ( t_rows2, t_cols2, values ) %>%
    dplyr::filter ( t_rows2 %in% technology$t_rows2 ) %>%
    dplyr::filter ( t_cols2 %in% technology$t_cols2) %>%
    tidyr::spread ( t_cols2, values ) %>%
    dplyr::mutate ( t_rows2 = as.character(t_rows2))
    
    if ( is.na(
      labelled_io_data[nrow(labelled_io_data), ncol(labelled_io_data)]) &&
      labelled_io_data[nrow(labelled_io_data), 1] == "D1") {
      labelled_io_data[nrow(labelled_io_data), ncol(labelled_io_data)] <- 0
    } #only if closed off to household sector 
    
    if (named == TRUE ) return (labelled_io_data) else {
      return ( as.matrix(labelled_io_data[1:nrow(labelled_io_data), 2:ncol(labelled_io_data)]) )
    }
}


