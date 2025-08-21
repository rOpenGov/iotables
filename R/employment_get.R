#' Get employment data
#'
#' Download Eurostat employment data for a country and arrange it to match 64x64
#' symmetric input–output tables (SIOTs).
#'
#' Currently only works with product × product tables.
#'
#' @param geo A two-letter country code (Eurostat style).
#' @param year Year of employment data (>= 2008, when NACE Rev. 2 was
#'   introduced).
#' @param sex Employment by sex. Defaults to `"Total"`. Use `"Females"`/`"F"` or
#'   `"Males"`/`"M"`.
#' @param age Eurostat age code. Defaults to `"Y_GE15"`. Any Eurostat code can
#'   be supplied.
#' @param labelling Either `"iotables"`, `"prod_na"` (product × product) or
#'   `"induse"` (industry × industry).
#' @param data_directory Optional path to save or load pre-processed employment
#'   data.
#' @param force_download Logical. If `TRUE`, forces a fresh download even if a
#'   file exists locally.
#'
#' @return A `data.frame` formatted to conform with Eurostat SIOTs, with
#'   auxiliary metadata.
#'
#' @source Eurostat dataset:
#'   <https://ec.europa.eu/eurostat/web/products-datasets/-/lfsq_egan22d>
#'
#' @family import functions
#' @examples
#' \dontrun{
#' employment <- employment_get(
#'   geo = "CZ",
#'   year = "2010",
#'   sex = "Total",
#'   age = "Y_GE15",
#'   data_directory = NULL,
#'   force_download = TRUE
#' )
#' }
#' @export

employment_get <- function(geo,
                           year = "2010",
                           sex = "Total",
                           age = "Y_GE15",
                           labelling = "iotables",
                           data_directory = NULL,
                           force_download = FALSE) {
  if (!labelling %in% c("iotables", "prod_na", "induse")) {
    stop("Labelling must be any of 'iotables', 'prod_na' [product x product] or 'induse' [industry x industry]")
  }

  year_number <- as.numeric(year)

  ## Avoiding no visible binding for global variable 'data' ----------
  getdata <- function(...) {
    e <- new.env()
    name <- utils::data(..., envir = e)[1]
    e[[name]]
  }

  employment_metadata <- getdata("employment_metadata")

  save_employment_file <- paste0(
    "employment_", tolower(age), "_",
    tolower(sex), "_",
    geo, "_", year, "_avg.rds"
  )

  ### Changing to Eurostat in case of GB/UK and GR/EL-------
  if (geo %in% c("GB", "GR")) {
    if (geo == "GB") {
      warning("Switching GB to Eurostat abbreviation UK.")
      geo <- "UK"
    }
    if (geo == "GR") {
      warning("Switching GR to Eurostat abbreviation EL.")
      geo <- "EL"
    }
  }

  sex <- tolower(sex)
  sex <- ifelse(grepl("total", sex), "T",
    ifelse(grepl("female", sex), "F", "M")
  )

  emp <- NULL

  ## Use data_directory if it exists--------------------------------
  if (!is.null(data_directory)) {
    # pre-existing raw data file in the data directory
    emp_file_name <- file.path(data_directory, "lfsq_egan22d.rds")

    if (!force_download) { # no new download and filtered version exists
      if (file.exists(file.path(data_directory, save_employment_file))) {
        message("Try to use pre-existing file ", save_employment_file)

        tryCatch({
          emp <- readRDS(file.path(data_directory, save_employment_file))
        }, error = function(cond) {
          message("Could not read file.", cond)
          return(NULL)
        }, finally = {
          return(emp)
        })
      } else { # no filtered version exists, work with raw file

        tryCatch({
          # Read pre-existing unfiltered raw data file
          emp <- readRDS(emp_file_name)
        }, error = function(cond) {
          message("Could not read file ", emp_file_name, "\n", cond)
          return(NULL)
        }, finally = {
          message("Read ", emp_file_name)
        })
      }
    } # end case of no forced download
  } # end case data_directory is not NULL

  ## Forced/new download--------------------------------
  if (is.null(emp)) {
    message("Downloading employment data from the Eurostat database.")
    emp <- eurostat::get_eurostat("lfsq_egan22d")

    if (!is.null(data_directory)) {
      # if !is.null emp_file_name is the general file name (without filtering
      # for the statistic and was created in the previous block including the
      # directory name)

      tryCatch({
        # Read pre-existing unfiltered raw data file
        saveRDS(emp, file = emp_file_name)
      }, error = function(cond) {
        message("Failed to save ", emp_file_name, ".")
      }, finally = {
        message("Saving raw employment data to ", emp_file_name)
      })
    }
  }

  ## Geo selection and exception handling--------------------------------
  if (geo %in% unique(emp$geo)) {
    select_geo <- which(as.character(emp$geo) %in% as.character(geo))
    emp <- emp[select_geo, ]
  } else {
    stop("No employment data found with geo parameter = ", geo)
  }

  if ("TIME_PERIOD" %in% names(emp)) {
    # Breaking change from eurostat 4.0.0
    emp <- emp %>% rename(time = TIME_PERIOD)
  }

  emp$year <- as.numeric(substr(as.character(emp$time), start = 1, stop = 4))

  ## Year selection and exception handling -------------------------------------

  if (year %in% unique(emp$year)) {
    select_year <- which(emp$year %in% year_number)
    emp <- emp[select_year, ]
  } else {
    stop("No employment data found with the year parameter = ", year)
  }

  ## Age group selection and exception handling ---------------------------------
  if (age %in% unique(emp$age)) {
    select_age <- which(as.character(emp$age) %in% as.character(age))
    emp <- emp[select_age, ]
  } else {
    stop("No employment data found with the age parameter = ", age)
  }

  ## Sex variable selection and exception handling--------------------------------
  if (sex %in% unique(emp$sex)) {
    select_sex <- which(as.character(emp$sex) %in% as.character(sex))
    emp <- emp[select_sex, ]
  } else {
    stop("No employment data found with sex parameter = ", sex)
  }

  ## Missing values changed to 0 -------------------------------------------------
  emp$values <- ifelse(is.na(emp$values), 0, emp$values)

  ## Data processing for employment variables ------------------------------------
  employment <- emp %>%
    mutate(nace_r2 = as.character(.data$nace_r2)) %>%
    group_by(nace_r2, year) %>%
    summarize(values = mean(.data$values), .groups = "drop") %>%
    dplyr::rename(emp_code = nace_r2) %>%
    ungroup() %>%
    left_join(employment_metadata,
      by = "emp_code"
    ) %>% # iotables:::employment_metadata
    dplyr::group_by(.data$code, .data$variable, .data$iotables_label) %>%
    dplyr::summarize(values = sum(.data$values), .groups = "drop")


  ## If data_directory exists, save results -------------------------------

  if (!is.null(data_directory)) {
    message("Saving ", save_employment_file)
    saveRDS(employment, file = file.path(
      data_directory,
      save_employment_file
    ))
  }


  ## If data_directory exists, save results--------------------------------

  emp_sex <- ifelse(tolower(sex) == "t", "total",
    ifelse(tolower(sex) == "f", "female", "male")
  )


  if (labelling == "iotables") { # this is the Eurostat manual-tutorial type labelling format
    prefix <- data.frame(
      iotables_row = paste0("employment_", emp_sex)
    )

    primary_employment_input <- employment %>%
      filter(variable == "prod_na") # does not matter which, not used

    ## No employment for imputed rent column--------------------------------

    imputed_rent <- data.frame(
      real_estate_imputed_a = 0
    )
    primary_employment_input <- primary_employment_input %>%
      dplyr::ungroup() %>%
      select(iotables_label, values) %>%
      tidyr::spread(iotables_label, values) # use iotables_label in this case
  } else if (labelling == "prod_na") { ## this is the product x product labelling format
    prefix <- data.frame(
      prod_na = paste0("employment_", emp_sex)
    )

    primary_employment_input <- employment %>%
      dplyr::filter(variable == "prod_na")

    imputed_rent <- data.frame(
      CPA_L68A = 0
    )
    primary_employment_input <- primary_employment_input %>%
      dplyr::ungroup() %>%
      dplyr::select(code, values) %>%
      tidyr::spread(code, values) # use code for standard Eurostat library
  } else if (labelling == "induse") { # this is the industry x industry labelling format
    prefix <- data.frame(
      induse = paste0("employment_", emp_sex)
    )

    primary_employment_input <- employment %>%
      dplyr::filter(variable == "induse")

    imputed_rent <- data.frame(
      L68A = 0
    )
    primary_employment_input <- primary_employment_input %>%
      dplyr::ungroup() %>%
      dplyr::select(code, values) %>%
      tidyr::spread(code, values) # use code for standard Eurostat library
  } else {
    warning("No L68A was added.")
    return(primary_employment_input)
  }

  return_employment <- cbind(prefix, primary_employment_input)
  return_employment <- cbind(return_employment, imputed_rent)

  return_employment
}
