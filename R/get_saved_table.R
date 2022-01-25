#' @title Get an input-output table from a downloaded bulk file
#' @importFrom dplyr distinct across left_join rename select any_of
#' @importFrom tidyr pivot_wider unnest
#' @importFrom rlang .data
#' @keywords internal
get_saved_table <- function (labelled_io_data, 
                             geo, 
                             year, 
                             unit,  
                             stk_flow) {
  
  if ( nchar(geo) == 2 & geo == tolower(geo)) { 
    geo <- toupper (geo)
  }
  
  iot <- find_saved_table(labelled_io_data, geo=geo, year=year, unit=unit, stk_flow=stk_flow )
  
  iot %>% unnest(.data$data)
}

#' @keywords internal
not_in_use <- function() {
  tmp_iot <- iot %>%
    unnest(iot, cols = "data") %>%
    select ( any_of(c( "prod_na", "induse", "values")))
  
  ## How to find out if it has prod_na?
  
  labelling_information <- tmp_iot %>% 
    dplyr::distinct( across ( any_of(c("trows", "prod_na", "induse")))) %>%
    rename ( code = .data$prod_na ) %>%
    left_join ( metadata, by = 'code' ) %>%
    arrange ( .data$numeric_label )
  
  if ( labelling != "iotables") {
    left_join (
      labelling_information %>% 
        select ( prod_na = .data$code ) %>%
        distinct( .data$prod_na), 
      tmp_iot %>%
        pivot_wider ( names_from ="induse", values_from = "values"),
      by = "prod_na"
    )
  } else {
    
    stop ("this is not yet working")
    
    induse_labels <-  labelling_information %>% 
      select ( induse = .data$code, .data$iotables_label ) %>%
      distinct( across(all_of(c("induse", "iotables_label")))) %>%
      semi_join ( tmp_iot %>% select ( any_of("induse")) )
    
    prod_na_labels <-    labelling_information %>% 
      select ( prod_na = .data$code, .data$iotables_label ) %>%
      distinct( across(all_of(c("prod_na", "iotables_label")))) %>%
      semi_join ( tmp_iot %>% select (any_of("prod_na")), by ="prod_na")
    
    dpulicates <- prod_na_labels %>%
      left_join ( tmp_iot, by  = "prod_na") %>%
      add_count( .data$prod_na, .data$induse) %>%
      filter ( n > 1)
    
    rename ( iotables_row = .data$iotables_label ) %>%
      select (-.data$prod_na) %>%
      left_join ( induse_labels ) %>%
      select (-.data$induse ) %>%
      pivot_wider( names_from = "iotables_label", values_from = "values")
  } 
}

#' @rdname get_saved_table
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @keywords internal
find_saved_table <- function(labelled_io_data, geo, unit, year, stk_flow) {
  geo_input <- geo
  unit_input <- unit
  year_input <- year
  stk_flow_input <- stk_flow
  
  assert_that( all( c("geo", "unit", "year", "stk_flow", "data") %in% names(labelled_io_data) ), 
               msg = "The columns 'geo', 'year', 'unit', 'stk_flow' columns and the nested 'data' column must be present in labelled_io_data.")
  
  assert_that( geo_input %in% labelled_io_data$geo, 
               msg = glue("The labelled_io_data$geo column does not contain geo='{geo_input}'."))
  
  subset_labelled_io_data <- labelled_io_data %>% filter ( .data$geo == geo_input )
  
  assert_that( unit_input %in% subset_labelled_io_data$unit, 
               msg = glue("The labelled_io_data$unit column does not contain unit='{unit_input}' (for geo='{geo_input}')."))
  
  subset_labelled_io_data  <- subset_labelled_io_data  %>% filter ( .data$unit == unit_input )
  
  assert_that( stk_flow_input %in% subset_labelled_io_data$stk_flow, 
               msg = glue("The labelled_io_data$stk_flow column does not contain {stk_flow_input} (for geo='{geo_input}')."))
  
  subset_labelled_io_data  <- subset_labelled_io_data  %>% filter ( .data$stk_flow == stk_flow_input )
  
  assert_that( year_input %in% subset_labelled_io_data$year, 
               msg = glue("The labelled_io_data$year column does not contain year='{year_input'} (for geo='{geo_input}', unit='{unit_input}', stk_flow='{stk_flow_input}'.)"))
  
  subset_labelled_io_data  %>% filter ( .data$year == year_input )
}

#' @rdname get_saved_table
#' @importFrom lubridate year
#' @importFrom dplyr mutate
#' @keywords internal
get_package_iots <- function( source_input ) {
  ## Read from file or internal dataset ----
  if ( source_input == "germany_1990" ) {
    
    germany_1990 <- getdata(germany_1990) 
    labelled_io_data <- germany_1990    # use germany example 
    labelled_io_data$year <- 1990
    
  } else if ( source_input == "croatia_2010_1700" ) { 
    
    croatia_2010_1700 <- getdata(croatia_2010_1700)
    labelled_io_data <- croatia_2010_1700 %>%
      mutate ( year = lubridate::year(.data$time))
    
  } else if ( source_input == "croatia_2010_1800" )  {
    
    croatia_2010_1800 <- getdata(croatia_2010_1800)
    labelled_io_data <- croatia_2010_1800   %>%
      mutate ( year = lubridate::year (.data$time))
    
  } else if ( source_input == "croatia_2010_1900" )  {
    
    croatia_2010_1900 <- getdata(croatia_2010_1900)
    labelled_io_data <- croatia_2010_1900 %>%
      mutate ( year = lubridate::year(.data$time))
    
  }
  labelled_io_data
}

#' @rdname get_saved_table
#' @keywords internal
uk_test_data_exceptions <- function (labelled_io_data, source) {
  if ( source == "uk_2010_siot") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Input-Output table (domestic use, basic prices, product by product)')
  }
  
  if ( source == "uk_2010_use") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Domestic use table at basic prices (product by industry)')
  }
  
  if ( source == "uk_2010_imports") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Imports use table at basic prices (product by product)')
  }
  
  if ( source == "uk_2010_coeff") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Matrix of coefficients (product by product)')
  }
  
  if ( source == "uk_2010_inverse") {
    labelled_io_data <- labelled_io_data %>%
      filter( .data$indicator == 'Leontief Inverse (product by product)')
  }
  
  labelled_io_data
}

## old_table_selection_function in data-raw/old_table_selection_function.R

