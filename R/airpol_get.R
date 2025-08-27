#' Get air pollutant data
#'
#' @description
#' Retrieve air emissions accounts by NACE Rev. 2 activity for environmental
#' impact assessments. Currently tested only with product × product tables.
#'
#' @details
#' The Eurostat dataset *Air emissions accounts by NACE Rev. 2 activity*
#' (`env_ac_ainah_r2`) contains emissions of major pollutants, including:
#' CO2, biomass CO2, N2O, CH4, PFCs, HFCs, SF6 (incl. NF3), NOx, NMVOC,
#' CO, PM10, PM2.5, SO2, and NH3.
#'
#' For details, see the
#' [Eurostat Reference Metadata (SIMS)](https://ec.europa.eu/eurostat/cache/metadata/en/env_ac_ainah_r2_sims.htm),
#' particularly on aggregated indicators: global warming potential (`GHG`),
#' acidifying gases (`ACG`), and tropospheric ozone precursors (`O3PR`).
#'
#' @param airpol Pollutant code. Defaults to `"GHG"`. Common values include
#'   `"ACG"`, `"CH4"`, `"CO2"`, `"NH3"`, `"NOX"`, `"PM10"`, `"PM2_5"`,
#'   `"SOX_SO2E"`. See **Details** for the full list.
#' @param geo Country code. The special value `"germany_1995"` returns the
#'   built-in replication dataset [germany_airpol].
#' @param year Reference year (≥2008 for NACE Rev. 2 statistics).
#' @param unit Unit of measure. Defaults to `"THS_T"` (thousand tons).
#' @param data_directory Optional directory path. If valid, the downloaded and
#'   pre-processed data will be saved here.
#' @param force_download Logical, defaults to `TRUE`. If `FALSE`, the function
#'   reuses an existing file in `data_directory` or a temporary directory.
#'
#' @return
#' A data frame with auxiliary metadata conforming to symmetric input–output
#' tables.
#'
#' @source
#' Eurostat dataset:
#' [Air emissions accounts by NACE Rev. 2 activity](https://ec.europa.eu/eurostat/web/products-datasets/-/env_ac_ainah_r2).
#'
#' @family import functions
#'
#' @examples
#' airpol_get(
#'   airpol = "CO2",
#'   geo = "germany_1995",
#'   year = 1995,
#'   unit = "THS_T"
#' )
#' @export


airpol_get <- function(airpol = "GHG",
                       geo = "BE",
                       year = 2020,
                       unit = "THS_T",
                       data_directory = NULL, force_download = TRUE) {
  if (geo == "germany_1995") {
    ## Avoid large examples on CRAN
    airpol_input <- airpol
    return_df <- getdata("germany_airpol") %>%
      filter(airpol %in% airpol_input) %>%
      select(iotables_col, value) %>%
      pivot_wider(
        names_from = iotables_col,
        values_from = value
      ) %>%
      mutate(indicator = paste0(airpol_input, "_emission")) %>%
      relocate(indicator, .before = everything())
    return(return_df)
  }

  if (force_download) {
    tmp <- eurostat::get_eurostat("env_ac_ainah_r2")
  } else {
    tmp <- tempdir_data("env_ac_ainah_r2")
  }

  if (!is.null(data_directory)) {
    if (dir.exists(data_directory)) {
      # If there is a temporary saving location given, save the data there.
      saveRDS(tmp, file = file.path(data_directory, "env_ac_ainah_r2.rds"))
    }
  }

  assert_that(
    airpol %in% tmp$airpol,
    msg = glue("{airpol} is not recognized as an air pollutant in Eurostat table env_ac_ainah_r2")
  )

  airpol_df <- tmp[tmp$airpol == airpol, ]

  assert_that(
    geo %in% airpol_df$geo,
    msg = glue("No data for geo='{geo}' with airpol='{airpol}' in year={year} and unit='{unit}' in Eurostat table env_ac_ainah_r2")
  )

  airpol_df <- airpol_df[airpol_df$geo == geo, ]

  assertthat::assert_that(
    as.Date(paste0(year, "-01-01")) %in% airpol_df$time,
    msg = glue::glue("No data for year={year} with geo='{geo}'an airpol='{airpol}' and unit='{unit}' in Eurostat table env_ac_ainah_r2")
  )

  airpol_df <- airpol_df[airpol_df$time == as.Date(paste0(year, "-01-01")), ]

  assertthat::assert_that(
    unit %in% airpol_df$unit,
    msg = glue::glue("No data for unit='{unit}' with geo='{geo}', airpol='{airpol}' and geo='{geo'} in Eurostat table env_ac_ainah_r2")
  )

  airpol_df <- airpol_df[airpol_df$unit == unit, ]

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
    mutate(nace_r2 = ifelse(.data$nace_r2 == "TOTAL",
      yes = "TOTAL",
      no = paste0("CPA_", nace_r2)
    )) %>%
    mutate(nace_r2 = case_when(
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

  ghg <- tibble(
    nace_r2 = prod_na
  )

  direct_match <- country_ghg %>%
    inner_join(ghg, by = "nace_r2")

  L68 <- tibble(
    nace_r2 = c("CPA_L68A", "CPA_L68B"),
    values = c(0, 0)
  )

  group_match <- country_ghg %>%
    rename(nace = nace_r2) %>%
    mutate(nace_r2 = substr(.data$nace, 1, 5)) %>%
    group_by(airpol, unit, geo, time, nace_r2) %>%
    summarise(values = sum(.data$values), .groups = "keep") %>%
    inner_join(ghg, by = "nace_r2")


  return_df <- ghg %>%
    left_join(
      direct_match %>%
        left_join(group_match, by = c("airpol", "nace_r2", "unit", "geo", "time", "values")) %>%
        select(nace_r2, values) %>%
        full_join(L68, by = c("nace_r2", "values")),
      by = "nace_r2"
    ) %>%
    pivot_wider(
      names_from = nace_r2,
      values_from = values
    ) %>%
    mutate(indicator = paste0(airpol, "_emission")) %>%
    relocate(indicator, .before = everything())

  return_df
}
