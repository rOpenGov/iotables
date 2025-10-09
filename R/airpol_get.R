#' Get air pollutant data (Eurostat env_ac_ainah_r2)
#'
#' @description Retrieve air emissions accounts by NACE Rev. 2 activity for
#' environmental impact assessments. Automatically manages Eurostat caching via
#' `tempdir()` or a user-specified `data_directory`.
#'
#' @details The Eurostat dataset *Air emissions accounts by NACE Rev. 2
#' activity* (`env_ac_ainah_r2`) contains emissions of major pollutants,
#' including: CO2, biomass CO2, N2O, CH4, PFCs, HFCs, SF6, NOx, NMVOC, CO, PM10,
#' PM2.5, SO2, and NH3. See Eurostat metadata for definitions of aggregated
#' indicators (GHG, ACG, O3PR, etc.).
#'
#' @note The function adjusts the invalid industry codes, like
#' `C10-C12` to `C10-12`.
#'
#' @inheritParams iotables_download
#' @param airpol Pollutant code (e.g. `"GHG"`, `"CO2"`, `"CH4"`,
#' `"NOX"`, `"NH3"`, etc.).
#' @param geo Country code (e.g. `"BE"`, `"DE"`). The special value
#'   `"germany_1995"` returns the built-in example dataset
#'    [germany_airpol].
#' @param year Reference year (≥ 2008 for NACE Rev. 2).
#' @param unit Unit of measure (default `"THS_T"` = thousand tonnes).
#'
#' @return A tidy data frame with air pollutant emissions aligned to IO
#' classifications.
#'
#' @source Eurostat dataset:
#'   [env_ac_ainah_r2](https://ec.europa.eu/eurostat/web/products-datasets/-/env_ac_ainah_r2)
#' @importFrom dplyr everything filter select relocate case_when
#' @importFrom dplyr group_by summarise
#' @importFrom tidyr pivot_wider
#' @importFrom assertthat assert_that
#' @export
airpol_get <- function(airpol = "GHG",
                       geo = "BE",
                       year = 2020,
                       unit = "THS_T",
                       data_directory = NULL,
                       force_download = FALSE) {
  # --- Handle built-in dataset -------------------------------------------
  if (geo == "germany_1995") {
    airpol_input <- airpol
    return_df <- getdata("germany_airpol") %>%
      dplyr::filter(airpol %in% airpol_input) %>%
      dplyr::select(iotables_col, value) %>%
      tidyr::pivot_wider(
        names_from = iotables_col,
        values_from = value
      ) %>%
      dplyr::mutate(indicator = paste0(airpol_input, "_emission")) %>%
      dplyr::relocate(indicator,
        .before = dplyr::everything()
      )
    return(return_df)
  }

  # --- Define and prepare cache directory -------------------------------
  cache_dir <- if (!is.null(data_directory)) {
    data_directory
  } else {
    file.path(tempdir(), "eurostat")
  }
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  cache_file <- file.path(cache_dir, "env_ac_ainah_r2_processed.rds")

  # --- Retrieve data ----------------------------------------------------
  if (!force_download && file.exists(cache_file)) {
    message("Reading cached Eurostat air pollutant data from: ", cache_file)
    tmp <- tryCatch(readRDS(cache_file), error = function(e) NULL)
  }

  if (!is.null(geo) || is.null(unit) || is.null(year)) {
    filters_list <- list(geo = geo, unit = unit, time = year, airpol = airpol)
    tmp <- get_eurostat_data(
      id = "env_ac_ainah_r2",
      filters = filters_list
    )
  } else {
    message("Downloading Eurostat dataset env_ac_ainah_r2 (force_download=", force_download, ")")
    tmp <- tryCatch(
      eurostat::get_eurostat("env_ac_ainah_r2",
        cache = !force_download,
        cache_dir = cache_dir
      ),
      error = function(e) {
        stop("Eurostat download failed: ", conditionMessage(e), call. = FALSE)
      }
    )
    saveRDS(tmp, cache_file, version = 2)
    message("Saved processed dataset to ", cache_file)
  }

  # --- Basic format check ----------------------------------------------
  assertthat::assert_that(
    is.data.frame(tmp),
    msg = "Eurostat air pollutant dataset not loaded properly."
  )

  # --- Harmonize time column name --------------------------------------
  tmp <- dplyr::rename(tmp, time = dplyr::any_of("TIME_PERIOD"))

  # --- Filter by pollutant ---------------------------------------------
  assertthat::assert_that(
    airpol %in% tmp$airpol,
    msg = glue::glue(
      "{airpol} is not recognized as a valid pollutant in env_ac_ainah_r2."
    )
  )
  airpol_df <- dplyr::filter(tmp, airpol == !!airpol)

  # --- Filter by country -----------------------------------------------
  assertthat::assert_that(
    geo %in% airpol_df$geo,
    msg = glue::glue(
      "No data for geo='{geo}' in env_ac_ainah_r2."
    )
  )

  airpol_df <- dplyr::filter(airpol_df, geo == !!geo)

  # --- Filter by year --------------------------------------------------
  assertthat::assert_that(
    as.Date(paste0(year, "-01-01")) %in% airpol_df$time,
    msg = glue::glue(
      "No data for year={year} with geo='{geo}' in env_ac_ainah_r2."
    )
  )
  airpol_df <- dplyr::filter(
    airpol_df,
    time == as.Date(paste0(year, "-01-01"))
  )

  # --- Filter by unit --------------------------------------------------
  assertthat::assert_that(
    unit %in% airpol_df$unit,
    msg = glue::glue(
      "No data for unit='{unit}' for geo='{geo}' and airpol='{airpol}'."
    )
  )
  airpol_df <- dplyr::filter(airpol_df, unit == !!unit)

  # --- Harmonize NACE structure ---------------------------------------

  country_ghg <- airpol_df %>%
    dplyr::mutate(nace_r2 = dplyr::case_when(
      nace_r2 == "C10-C12" ~ "C10-12",
      nace_r2 == "C13-C15" ~ "C13-15",
      nace_r2 == "C31_C32" ~ "C31_32",
      nace_r2 == "E37-E39" ~ "E37-39",
      nace_r2 == "J59_J60" ~ "J59_60",
      nace_r2 == "J62_J63" ~ "J62_63",
      nace_r2 == "M69_M70" ~ "M69_70",
      nace_r2 == "M74_M75" ~ "M74_75",
      nace_r2 == "N80-N82" ~ "N80-82",
      nace_r2 == "Q87_Q88" ~ "Q87_88",
      nace_r2 == "R90-R92" ~ "R90-92",
      TRUE ~ nace_r2
    ))

  group_match <- country_ghg %>%
    dplyr::rename(nace = nace_r2) %>%
    dplyr::mutate(nace_r2 = substr(nace, 1, 1)) %>%
    dplyr::group_by(airpol, unit, geo, time, nace_r2) %>%
    dplyr::summarise(values = sum(values), .groups = "keep") %>%
    dplyr::anti_join(country_ghg, by = "nace_r2") %>%
    dplyr::mutate(freq = "A")

  return_df <- dplyr::bind_rows(
    country_ghg,
    group_match
  )

  if ("rowid" %in% names(return_df)) {
    return_df <- dplyr::select(return_df, -rowid)
  }

  return_df <- return_df %>%
    tidyr::pivot_wider(
      names_from = nace_r2,
      values_from = values
    ) %>%
    ensure_l68_columns(c("L68A", "L68B")) %>%
    dplyr::relocate(L68A, .after = "L") %>%
    dplyr::relocate(L68B, .after = "L68A") %>%
    dplyr::relocate(TOTAL, .after = "U") %>%
    dplyr::relocate(U, .before = "TOTAL") %>%
    dplyr::mutate(prod_na = paste0(airpol, "_emission")) %>%
    dplyr::relocate(prod_na, .before = dplyr::everything()) %>%
    select(-geo, -freq, -time, -airpol, -unit, -year)

  attr(return_df, "geo") <- geo
  attr(return_df, "year") <- year
  attr(return_df, "unit") <- unit
  attr(return_df, "freq") <- "A"
  return_df
}
