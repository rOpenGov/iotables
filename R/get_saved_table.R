#' @title Get an input-output table from a downloaded bulk file
#' @importFrom dplyr distinct across left_join rename select any_of
#' @importFrom tidyr pivot_wider unnest

#' @keywords internal
get_saved_table <- function(labelled_io_data,
                            geo,
                            year,
                            unit,
                            stk_flow) {
  if (nchar(geo) == 2 & geo == tolower(geo)) {
    geo <- toupper(geo)
  }

  iot <- find_saved_table(labelled_io_data, geo = geo, year = year, unit = unit, stk_flow = stk_flow)

  iot %>% unnest(.data$data)
}

#' @rdname get_saved_table
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter

#' @keywords internal
find_saved_table <- function(labelled_io_data, geo, unit, year, stk_flow) {
  geo_input <- geo
  unit_input <- unit
  year_input <- year
  stk_flow_input <- stk_flow

  assert_that(all(c("geo", "unit", "year", "stk_flow", "data") %in% names(labelled_io_data)),
    msg = "The columns 'geo', 'year', 'unit', 'stk_flow' columns and the nested 'data' column must be present in labelled_io_data."
  )

  assert_that(geo_input %in% labelled_io_data$geo,
    msg = glue("The labelled_io_data$geo column does not contain geo='{geo_input}'.")
  )

  subset_labelled_io_data <- labelled_io_data %>% filter(geo == geo_input)

  assert_that(unit_input %in% subset_labelled_io_data$unit,
    msg = glue("The labelled_io_data$unit column does not contain unit='{unit_input}' (for geo='{geo_input}').")
  )

  subset_labelled_io_data <- subset_labelled_io_data %>% filter(unit == unit_input)

  assert_that(stk_flow_input %in% subset_labelled_io_data$stk_flow,
    msg = glue("The labelled_io_data$stk_flow column does not contain {stk_flow_input} (for geo='{geo_input}').")
  )

  subset_labelled_io_data <- subset_labelled_io_data %>% filter(stk_flow == stk_flow_input)

  assert_that(year_input %in% subset_labelled_io_data$year,
    msg = glue("The labelled_io_data$year column does not contain year='{year_input'} (for geo='{geo_input}', unit='{unit_input}', stk_flow='{stk_flow_input}'.)")
  )

  subset_labelled_io_data %>% filter(year == year_input)
}

#' @rdname get_saved_table
#' @importFrom lubridate year
#' @importFrom dplyr mutate
#' @keywords internal
get_package_iots <- function(source_input) {
  croatia_tables <- c("croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")
  uk_tables <- c("uk_2010_siot", "uk_2010_coeff", "uk_2010_inverse")

  ## Read from file or internal dataset ----
  if (source_input == "germany_1995") {
    germany_1995 <- getdata("germany_1995")
    labelled_io_data <- germany_1995 # use germany example
    labelled_io_data$year <- 1990
  } else if (source_input == "croatia_2010_1700") {
    croatia_2010_1700 <- getdata("croatia_2010_1700")
    labelled_io_data <- croatia_2010_1700 %>%
      mutate(year = lubridate::year(.data$time))
  } else if (source_input == "croatia_2010_1800") {
    croatia_2010_1800 <- getdata("croatia_2010_1800")
    labelled_io_data <- croatia_2010_1800 %>%
      mutate(year = lubridate::year(.data$time))
  } else if (source_input == "croatia_2010_1900") {
    croatia_2010_1900 <- getdata("croatia_2010_1900")
    labelled_io_data <- croatia_2010_1900 %>%
      mutate(year = lubridate::year(.data$time))
  } else if (source_input %in% uk_tables) {
    labelled_io_data <- getdata("uk_2010_data") %>%
      mutate(year = 2010)

    if (source %in% uk_tables) {
      if (source == "uk_2010_siot") {
        labelled_io_data <- labelled_io_data %>%
          filter(indicator == "Input-Output table (domestic use, basic prices, product by product)")
      }

      if (source == "uk_2010_use") {
        labelled_io_data <- labelled_io_data %>%
          filter(indicator == "Domestic use table at basic prices (product by industry)")
      }

      if (source == "uk_2010_imports") {
        labelled_io_data <- labelled_io_data %>%
          filter(indicator == "Imports use table at basic prices (product by product)")
      }

      if (source == "uk_2010_coeff") {
        labelled_io_data <- labelled_io_data %>%
          filter(indicator == "Matrix of coefficients (product by product)")
      }

      if (source == "uk_2010_inverse") {
        labelled_io_data <- labelled_io_data %>%
          filter(indicator == "Leontief Inverse (product by product)")
      }
    }
  }
  labelled_io_data
}
