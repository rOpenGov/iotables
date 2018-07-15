#' Get employment data
#'
#' Download the employment data for a country and arrange it to the 64x64 SIOTS.
#' @param geo The country code. 
#' @param year The year.  The avarege employment will be created for the given year, 
#' starting with \code{2008}, when the NACE Rev 2 was introduced in employment statistics.
#' @param sex Defaults to \code{"Total"}. Enter \code{"Females"} or \code{"F"} for
#' female employmnent, \code{"Males"} or \code{"M"} for male employment.
#' @param age Defaults to \code{"Y_GE15"}, which is the Eurostat code for employment in 
#' all age groups starting from 15-years-old. Any Eurostat code can be used as a 
#' parameter. 
#' @param data_directory Defaults to \code{NULL}, if a valid directory, it will try to save the pre-processed 
#' data file here with labelling. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing downloaded file
#' in the \code{data_directory} or the temporary directory, if it exists.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join rename ungroup summarize
#' @importFrom eurostat get_eurostat
#' @examples
#' \dontrun{
#'  io_tables <- get_employment ( 
#'                geo = "CZ", 
#'                year = "2010",
#'                sex = "Total", 
#'                age = "Y_GE15",
#'                data_directory = NULL,
#'                force_download = TRUE)
#'  }
#' @export

get_employment <- function ( geo = "CZ", 
                             year = "2010",
                             sex = "Total", 
                             age = "Y_GE15",
                             data_directory = NULL,
                             force_download = TRUE) {
  nace_r2 <- values <- code <- variable <- NULL
  geo_input = geo; year_input = year; age_input = age; 
  if ( geo_input %in% c("GB", "GR")) {
    if (geo_input == "GB") {
      warning ( "Switching GB to Eurostat abbreviation UK.")
      geo_input <- "UK"
    }
    if (geo_input == "GR") {
      warning ( "Switching GR to Eurostat abbreviation EL.")
      geo_input <- "EL"
    }
  }
  
  sex_input <- tolower(sex) 
  sex_input <- ifelse ( grepl("total", sex_input), "T", 
                        ifelse ( grepl("female", sex_input), "F", "M"))
  
  emp <- NULL
  
  if ( !is.null(data_directory)) {
    emp_file_name <- paste0(data_directory, "/", "lfsq_egan22d.rds") 
    if ( ! force_download ) {
        try({emp <- readRDS(emp_file_name)})
    }
  }
 
  if (is.null(emp)) {
    message ( "Downloading employment data from the Eurostat database.")
    emp <- eurostat::get_eurostat ("lfsq_egan22d")
    if ( !is.null(data_directory)) {
      message ( "Saving raw employment data to ", emp_file_name, '.')
      saveRDS(emp, emp_file_name )
    }
  }
 
 if ( geo_input %in% unique ( emp$geo ) ) {
    emp <- dplyr::filter ( emp, geo == geo_input )
  } else {
    stop ("No employment data found with geo parameter = ", geo_input )
  }
  
  emp$year <- as.numeric(substr(as.character(emp$time), start = 1, stop = 4))
  
  if ( year_input %in% unique ( emp$year ) ) {
    emp <- dplyr::filter ( emp, year == year_input )
  } else {
    stop ("No employment data found with the year parameter = ", year_input )
  }
  
  if ( age_input %in% unique ( emp$age ) ) {
    emp <- dplyr::filter ( emp, age == age_input )
  } else {
    stop ("No employment data found with the age parameter = ", age_input )
  }
  
  
  if ( sex_input %in% unique ( emp$sex ) ) {
    emp <- dplyr::filter ( emp, sex == sex_input )
  } else {
    stop ("No employment data found with sex parameter = ", sex_input )
  }
  
  emp$values <- ifelse ( is.na(emp$values), 0, emp$values ) 
  
  employment <- emp %>%
    dplyr::mutate ( nace_r2 = as.character(nace_r2) ) %>%
    dplyr::group_by ( nace_r2, year ) %>%
    dplyr::summarize ( values = mean(values)) %>%
    dplyr::rename ( emp_code = nace_r2 ) %>%
    dplyr::ungroup ( ) %>%
    dplyr::left_join ( employment_metadata, by = "emp_code") %>%
    dplyr::group_by ( code, variable ) %>%
    dplyr::summarize ( values = sum(values)) %>%
    dplyr::mutate ( geo = geo_input ) %>%
    dplyr::mutate ( year = year_input ) %>%
    dplyr::mutate ( sex = sex ) 
  
  if ( ! is.null(data_directory)) {
    save_employment_table <- paste0(data_directory, '/employment_',
                                    tolower(sex),
                                    '_', geo, '_', year, '_avg.rds')
    message ( "Saving ", save_employment_table )
    saveRDS(employment, file = save_employment_table)
  }
  employment
}
  
  
  