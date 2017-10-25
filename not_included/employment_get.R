#' Get the employment satellite accounts 
#'
#' This function will retrieve the employment satellite data from the lablelled input-output data frame, if it exsits.
#' In most cases unfortunately this is not the case, and a similar employment vector must be created from other data sources.
#' (See the output of the example function.)
#' @param labelled_io_data An IO data frame created with the get_io_data () function.
#' @param indicator A character string or a character vector containing the indicator names in the employment satellite account. It defaults to 'employment_total'. 
#' @param technology A technology data frame created with the create_technology_data () function. Defaults to "" in which case it tries to create the technology data with the function.
#' @param geo A country code or a country name, defaults to "SK" that could be written as "Slovakia", too.
#' @param year A numeric variable containing the year. 
#' @param change_unit A logical variable that defaults to FALSE. If yes, the unit will be the natural unit, if original data is in thousands, the retunred values will be multiplied by 1000. 
#' @param named Defaults to FALSE and returns a matrix. TRUE returns a tibble containing the technology raws as a key column. 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr spread
#' @examples
#' \dontrun{
#'  data (germany_1990)
#'  employment_vector <- employment_get( 
#'                        labelled_io_data = germany_1990, 
#'                        indicator = "employment_total", geo = "DE", 
#'                        year = 1990, named = TRUE) 
#'}
#'                   

#' @export

employment_get <- function ( labelled_io_data, indicator = "employment_total",
                                  technology = NULL,
                                  geo = "SK", year = 2010, 
                                  change_unit = FALSE, named = TRUE) {
  time = NULL; t_cols2 = NULL; t_rows2 = NULL; values = NULL #non-standard evaluation creates a varning in build. 
  geo_input <- geo
  
  if (is.null(geo)) stop ("Error: no country selected.")
  
  if ( nchar(geo) == 2 & geo == tolower(geo)) { 
    geo <- toupper(geo)
    warning("Warning: country code changed to upper case.")
  }
  if ( ! "data.frame" %in%  class(technology)) {
    if (nrow(labelled_io_data)>5000) {
      warning ("Warning: The input file is large, this may take a few seconds.")
    }
    
    if ( geo == "HR" & year == 2010 ) { source <- "dzs" } else { source <- "eurostat"}
    technology <- get_technology_data(labelled_io_data  = labelled_io_data, 
                                      source = source, geo = geo_input, year = year )
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
  
  employment_name <- indicator

  employment <- labelled_io_data %>%
    filter ( geo == geo_input)  %>%
    filter ( time == paste0(year, "-01-01" )) %>%
    filter ( t_rows2 %in% indicator) %>%
    filter ( t_cols2  %in% technology$t_cols2 ) %>%  
    select ( t_cols2, t_rows2, values )  %>%
    mutate ( t_rows2 = as.character(t_rows2)) %>%
    spread (t_cols2, values )

  if (named) return( employment) else {
    return(as.vector(t(employment[1,2:ncol(employment)])))}
} #end of function

