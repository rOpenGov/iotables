#' Get employment data
#'
#' Download the employment data for a country and arrange it to the 64x64 SIOTS.
#' Currently works only with product x product tables. 
#' @param geo The country code. 
#' @param year The year.  The average employment will be created for the given
#' year, starting with \code{2008}, when the NACE Rev 2 was introduced in 
#' employment statistics.
#' @param sex Defaults to \code{"Total"}. Enter \code{"Females"} or \code{"F"} for
#' female employment, \code{"Males"} or \code{"M"} for male employment.
#' @param age Defaults to \code{"Y_GE15"}, which is the Eurostat code for employment in 
#' all age groups starting from 15-years-old. Any Eurostat code can be used as a 
#' parameter. 
#' @param labelling Either \code{"iotables"} or the applicable short code, 
#' for product x product SIOTs \code{"prod_na"} and in the case of industry x 
#' industry SIOTs \code{"induse"}.
#' @param data_directory Defaults to \code{NULL}, if a valid directory, it will try to save the pre-processed 
#' data file here with labelling. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing downloaded file
#' in the \code{data_directory} or the temporary directory, if it exists.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join rename ungroup summarize
#' @importFrom tidyr spread
#' @importFrom eurostat get_eurostat
#' @source Eurostat statistic \href{http://appsso.eurostat.ec.europa.eu/nui/show.do?dataset=lfsq_egan22d&lang=en}{Employment 
#' by sex, age and detailed economic activity (from 2008 onwards, NACE Rev. 2 two digit level) - 1 000}
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

geo = "sk" 
labelling = 'short'
employment_get <- function ( geo = "CZ", 
                             year = "2010",
                             sex = "Total", 
                             age = "Y_GE15",
                             labelling = 'iotables', 
                             data_directory = NULL,
                             force_download = TRUE) {
  nace_r2 <- values <- code <- variable <- iotables_label <- NULL
  geo_input <- geo; year_input <- year; age_input <- age; sex_input <- sex
  
  if ( ! labelling %in% c("iotables", 'prod_na', 'induse')) {
    stop("Labelling must be any of 'iotables', 'prod_na' [product x product] or 'induse' [industry x industry]")
  }
  
  save_employment_file <- paste0('employment_', tolower (age_input), '_',
                                 tolower(sex_input), '_', 
                                 geo_input, '_', year, '_avg.rds')
  
  ###Changing to Eurostat in case of GB/UK and GR/EL-------
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
  
##Use data_directory if it exists--------------------------------  
  if ( !is.null(data_directory) ) {
    #pre-existing raw data file in the data directory
    emp_file_name <- file.path(data_directory, "lfsq_egan22d.rds") 
    
    if ( ! force_download ) {  #no new download and filtered version exists 
        if ( file.exists(file.path(data_directory, save_employment_file)) ) {
          message("Try to use pre-existing file ", save_employment_file )
          
          tryCatch({
            emp <- readRDS(file.path(data_directory, save_employment_file))
            }, error = function(cond) {
              message ( "Could not read file.", cond)
              return(NULL)
            }, finally ={ 
              return(emp)
              })
      
        } else {  #no filtered version exists, work with raw file
          
          tryCatch({
            #Read pre-existing unfiltered raw data file 
            emp <- readRDS(emp_file_name)
          }, error = function(cond) {
            message ( 'Could not read file ', emp_file_name, '\n', cond)
            return(NULL)
          }, finally ={ 
            message ( 'Read ', emp_file_name )
          })
        }
    }  #end case of no forced download
  }  #end case data_directory is not NULL
 
  ##Forced/new download--------------------------------  
  if ( is.null(emp) ) {
    
    message ( "Downloading employment data from the Eurostat database.")
    emp <- eurostat::get_eurostat ("lfsq_egan22d")
    
    if ( !is.null(data_directory) ) {
      #if !is.null emp_file_name is the general file name (without filtering
      # for the statistic and was created in the previous block including the 
      # directory name)

      tryCatch({
        #Read pre-existing unfiltered raw data file 
        saveRDS(emp, file = emp_file_name)
      }, error = function(cond) {
        message ( "Failed to save ", emp_file_name, '.')
      }, finally = { 
        message ( 'Saving raw employment data to ', emp_file_name )
      })
    }
  }
 
  ##Geo selection and exception handling--------------------------------  
 if ( geo_input %in% unique ( emp$geo ) ) {
    emp <- dplyr::filter ( emp, geo == geo_input )
  } else {
    stop ("No employment data found with geo parameter = ", geo_input )
  }
  
  emp$year <- as.numeric(substr(as.character(emp$time), start = 1, stop = 4))
  
  ##Year selection and exception handling--------------------------------  
  
  if ( year_input %in% unique ( emp$year ) ) {
    emp <- dplyr::filter ( emp, year == year_input )
  } else {
    stop ("No employment data found with the year parameter = ", year_input )
  }
  
  ##Age group selection and exception handling--------------------------------  
  if ( age_input %in% unique ( emp$age ) ) {
    emp <- dplyr::filter ( emp, age == age_input )
  } else {
    stop ("No employment data found with the age parameter = ", age_input )
  }
  
  ##Sex variable selection and exception handling-------------------------------- 
  if ( sex_input %in% unique ( emp$sex ) ) {
    emp <- dplyr::filter ( emp, sex == sex_input )
  } else {
    stop ("No employment data found with sex parameter = ", sex_input )
  }
  
  ##Missing values changed to 0-------------------------------- 
  emp$values <- ifelse ( is.na(emp$values), 0, emp$values ) 
  
  ##Data processing for employment variables-------------------------------- 
  employment <- emp %>%
    dplyr::mutate (   nace_r2 = as.character(nace_r2) ) %>%
    dplyr::group_by ( nace_r2, year ) %>%
    dplyr::summarize ( values = mean(values)) %>%
    dplyr::rename ( emp_code = nace_r2 ) %>%
    dplyr::ungroup ( ) %>%
    dplyr::left_join ( employment_metadata, by = "emp_code") %>%  # iotables:::employment_metadata
    dplyr::group_by (  code, variable, iotables_label ) %>%
    dplyr::summarize ( values = sum(values)) %>%
    dplyr::mutate ( geo = geo_input ) %>%
    dplyr::mutate ( year = year_input ) %>%
    dplyr::mutate ( sex = sex_input ) 
  
  
  ##If data_directory exists, save results-------------------------------- 
  
  if ( ! is.null(data_directory) ) {
    message ( "Saving ", save_employment_file )
    saveRDS(employment, file = file.path(data_directory, 
                                         save_employment_file)
            )
  }
 
  
  ##If data_directory exists, save results-------------------------------- 
  
  emp_sex <- ifelse ( tolower(sex_input) == "t", "total", 
                      ifelse (tolower(sex_input) == "f", "female", "male" ))
  

  if ( labelling == "iotables" ) {
    prefix <- data.frame ( 
      iotables_row = paste0("employment_", emp_sex )
    )
    
    primary_employment_input <- employment %>%
      dplyr::filter ( variable == "prod_na" ) #does not matter which, not used
    
    ##No employment for imputed rent column-------------------------------- 
    
    imputed_rent <- data.frame ( 
      real_estate_imputed_a = 0
    )
    primary_employment_input <-  primary_employment_input %>% 
      dplyr::ungroup() %>%
      dplyr::select ( iotables_label, values ) %>%
      tidyr::spread ( iotables_label, values )    #use iotables_label in this case
    
  } else if ( labelling == "prod_na" ){
    prefix <- data.frame ( 
      prod_na = paste0("employment_", emp_sex )
    )
    
    primary_employment_input <- employment %>%
      dplyr::filter ( variable == "prod_na" )
    
    imputed_rent <- data.frame ( 
      CPA_L68A = 0
    )
    primary_employment_input <-  primary_employment_input %>% 
      dplyr::ungroup() %>%
      dplyr::select ( code, values ) %>%
      tidyr::spread ( code, values )     #use code for standard Eurostat library
    
  } else if (labelling == "induse" ) {
    prefix <- data.frame ( 
      induse = paste0("employment_", emp_sex )
    )
    
    primary_employment_input <- employment %>%
      dplyr::filter ( variable == "induse" )
    
    imputed_rent <- data.frame ( 
      L68A = 0
    )
    primary_employment_input <-  primary_employment_input %>% 
      dplyr::ungroup() %>%
      dplyr::select ( code, values ) %>%
      tidyr::spread ( code, values )      #use code for standard Eurostat library
    
  } else {
    warning("No L68A was added.")
    return ( primary_employment_input )
  }
  
  return_employment <- cbind( prefix, primary_employment_input )
  return_employment <- cbind ( return_employment, imputed_rent )
  
  return_employment
}
  
  
  