#' Get air pollutant data (Eurostat env_ac_ainah_r2)
#'
#' @description
#' Retrieve air emissions accounts by NACE Rev. 2 activity for environmental
#' impact assessments. Automatically manages Eurostat caching via `tempdir()`
#' or a user-specified `data_directory`.
#'
#' @details
#' The Eurostat dataset *Air emissions accounts by NACE Rev. 2 activity*
#' (`env_ac_ainah_r2`) contains emissions of major pollutants, including:
#' CO2, biomass CO2, N2O, CH4, PFCs, HFCs, SF6, NOx, NMVOC, CO, PM10, PM2.5,
#' SO2, and NH3. See Eurostat metadata for definitions of aggregated indicators
#' (GHG, ACG, O3PR, etc.).
#'
#' @inheritParams iotables_download
#' @param airpol Pollutant code (e.g. `"GHG"`, `"CO2"`, `"CH4"`, `"NOX"`, `"NH3"`, etc.).
#' @param geo Country code (e.g. `"BE"`, `"DE"`). The special value `"germany_1995"`
#'   returns the built-in example dataset [germany_airpol].
#' @param year Reference year (≥ 2008 for NACE Rev. 2).
#' @param unit Unit of measure (default `"THS_T"` = thousand tonnes).
#'
#' @return
#' A tidy data frame with air pollutant emissions aligned to IO classifications.
#'
#' @source Eurostat dataset: [env_ac_ainah_r2](https://ec.europa.eu/eurostat/web/products-datasets/-/env_ac_ainah_r2)
#' @importFrom dplyr everything filter select relocate
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
      tidyr::pivot_wider(names_from = iotables_col, 
                         values_from = value) %>%
      dplyr::mutate(indicator = paste0(airpol_input, "_emission")) %>%
      dplyr::relocate(indicator, .before = dplyr::everything())
    return(return_df)
  }

  # --- Define and prepare cache directory -------------------------------
  cache_dir <- if (!is.null(data_directory)) {
    data_directory
  } else {
    file.path(tempdir(), "eurostat")
  }
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

  eurostat::set_eurostat_cache_dir(cache_dir)

  cache_file <- file.path(cache_dir, "env_ac_ainah_r2_processed.rds")

  # --- Retrieve data ----------------------------------------------------
  if (!force_download && file.exists(cache_file)) {
    message("Reading cached Eurostat air pollutant data from: ", cache_file)
    tmp <- tryCatch(readRDS(cache_file), error = function(e) NULL)
  } else {
    message("Downloading Eurostat dataset env_ac_ainah_r2 (force_download=", force_download, ")")
    tmp <- tryCatch(
      eurostat::get_eurostat("env_ac_ainah_r2", cache = !force_download, cache_dir = cache_dir),
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

  # Harmonize time column name
  tmp <- dplyr::rename(tmp, time = dplyr::any_of("TIME_PERIOD"))

  # --- Filter by pollutant ---------------------------------------------
  assertthat::assert_that(
    airpol %in% tmp$airpol,
    msg = glue::glue("{airpol} is not recognized as a valid pollutant in env_ac_ainah_r2.")
  )
  airpol_df <- dplyr::filter(tmp, airpol == !!airpol)

  # --- Filter by country -----------------------------------------------
  assertthat::assert_that(
    geo %in% airpol_df$geo,
    msg = glue::glue("No data for geo='{geo}' in env_ac_ainah_r2.")
  )
  airpol_df <- dplyr::filter(airpol_df, geo == !!geo)

  # --- Filter by year --------------------------------------------------
  assertthat::assert_that(
    as.Date(paste0(year, "-01-01")) %in% airpol_df$time,
    msg = glue::glue("No data for year={year} with geo='{geo}' in env_ac_ainah_r2.")
  )
  airpol_df <- dplyr::filter(airpol_df, time == as.Date(paste0(year, "-01-01")))

  # --- Filter by unit --------------------------------------------------
  assertthat::assert_that(
    unit %in% airpol_df$unit,
    msg = glue::glue("No data for unit='{unit}' for geo='{geo}' and airpol='{airpol}'.")
  )
  airpol_df <- dplyr::filter(airpol_df, unit == !!unit)

  # --- Harmonize CPA/NACE structure -----------------------------------
  prod_na <- c(
    "CPA_A01", "CPA_A02", "CPA_A03", "CPA_B", "CPA_C10-12", "CPA_C13-15", "CPA_C16",
    "CPA_C17", "CPA_C18", "CPA_C19", "CPA_C20", "CPA_C21", "CPA_C22", "CPA_C23", "CPA_C24",
    "CPA_C25", "CPA_C26", "CPA_C27", "CPA_C28", "CPA_C29", "CPA_C30", "CPA_C31_32", "CPA_C33",
    "CPA_D", "CPA_E36", "CPA_E37-39", "CPA_F", "CPA_G45", "CPA_G46", "CPA_G47", "CPA_H49",
    "CPA_H50", "CPA_H51", "CPA_H52", "CPA_H53", "CPA_I", "CPA_J58", "CPA_J59_60", "CPA_J61",
    "CPA_J62_63", "CPA_K64", "CPA_K65", "CPA_K66", "CPA_L68A", "CPA_L68B", "CPA_M69_70",
    "CPA_M71", "CPA_M72", "CPA_M73", "CPA_M74_75", "CPA_N77", "CPA_N78", "CPA_N79",
    "CPA_N80-82", "CPA_O", "CPA_P", "CPA_Q86", "CPA_Q87_88", "CPA_R90-92", "CPA_R93",
    "CPA_S94", "CPA_S95", "CPA_S96", "CPA_T", "CPA_U", "TOTAL"
  )

  country_ghg <- airpol_df %>%
    dplyr::mutate(nace_r2 = ifelse(nace_r2 == "TOTAL",
      "TOTAL", paste0("CPA_", nace_r2)
    )) %>%
    dplyr::mutate(nace_r2 = dplyr::case_when(
      nace_r2 == "CPA_C10-C12" ~ "CPA_C10-12",
      nace_r2 == "CPA_C13-C15" ~ "CPA_C13-15",
      nace_r2 == "CPA_C31_C32" ~ "CPA_C31_32",
      nace_r2 == "CPA_E37-E39" ~ "CPA_E37-39",
      nace_r2 == "CPA_J59_J60" ~ "CPA_J59_60",
      nace_r2 == "CPA_J62_J63" ~ "CPA_J62_63",
      nace_r2 == "CPA_M69_M70" ~ "CPA_M69_70",
      nace_r2 == "CPA_M74_M75" ~ "CPA_M74_75",
      nace_r2 == "CPA_N80-N82" ~ "CPA_N80-82",
      nace_r2 == "CPA_Q87_Q88" ~ "CPA_Q87_88",
      nace_r2 == "CPA_R90-R92" ~ "CPA_R90-92",
      TRUE ~ nace_r2
    ))

  ghg <- tibble::tibble(nace_r2 = prod_na)

  direct_match <- dplyr::inner_join(country_ghg, ghg, by = "nace_r2")

  group_match <- country_ghg %>%
    dplyr::rename(nace = nace_r2) %>%
    dplyr::mutate(nace_r2 = substr(nace, 1, 5)) %>%
    dplyr::group_by(airpol, unit, geo, time, nace_r2) %>%
    dplyr::summarise(values = sum(values), .groups = "keep") %>%
    dplyr::inner_join(ghg, by = "nace_r2")

  return_df <- ghg %>%
    dplyr::left_join(
      direct_match %>%
        dplyr::left_join(group_match, by = c("airpol", "nace_r2", "unit", "geo", "time", "values")) %>%
        dplyr::select(nace_r2, values),
      by = "nace_r2"
    ) %>%
    tidyr::pivot_wider(names_from = nace_r2, values_from = values) %>%
    ensure_l68_columns(c("CPA_L68A", "CPA_L68B")) %>%
    dplyr::mutate(indicator = paste0(airpol, "_emission")) %>%
    dplyr::relocate(indicator, .before = dplyr::everything())

  attr(return_df, "geo") <- geo
  attr(return_df, "year") <- year
  attr(return_df, "unit") <- unit

  return(return_df)
}
