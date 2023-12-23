#' @title Get air pollutant data
#'
#' @description Get air emissions accounts by NACE Rev. 2 activity for environmental impact
#' assessments.
#' 
#' @details Currently tested only with product x product tables. 
#' The dataset air emissions accounts by NACE Rev. 2 activity [env_ac_ainah_r2] has five dimensions:
#' The Air pollutant \code{airpol} variables are collected on the emissions of the following pollutants: 
#' carbon dioxide without emissions from biomass (CO2), carbon dioxide from biomass (Biomass CO2), 
#' nitroux oxide (N2O), methane (CH4), perfluorocarbons (PFCs), Hydrofluorocarbons (HFCs), 
#' sulphur hexafluoride (SF6) including nitrogen trifluoride (NF3), nitrogen oxides (NOx), 
#' Non-methane volatile organic compounds, (NMVOC), carbon monoxide (CO), 
#' Particulate matter smaller than 10 micrometre (PM10), Particulate matter smaller than 2,5 micrometre (PM2,5), 
#' Sulphur dioxide (SO2), Ammonia (NH3).
#' 
#' See \href{https://ec.europa.eu/eurostat/cache/metadata/en/env_ac_ainah_r2_sims.htm}{Reference Metadata in Single Integrated Metadata Structure (SIMS)}
#' for further details, particularly on the calculation of Global warming potential \code{GHG}, 
#' Acidifying gases \code{ACG} and Tropospheric ozone precursors \code{O3PR}.
#' 
#' @param airpol The code of the air pollutant. Defaults \code{GHG}.
#'  \code{ACG}, \code{CH4}, \code{CH4_CO2E}, \code{CH4_NMVOCE}, \code{CO}, \code{CO2}, 
#'  \code{CO2_BIO}, \code{CO_NMVOCE}, \code{GHG}, \code{HFC_CO2E}, \code{N2O}, \code{N2O_CO2E}, 
#'  \code{NF3_SF6_CO2E}, \code{NH3}, \code{NH3_SO2E}, \code{NMVOC}, \code{NOX}, \code{NOX_NMVOCE}, 
#'  \code{NOX_SO2E}, \code{O3PR}, \code{PFC_CO2E}, \code{PM10}, \code{PM2_5}, \code{SOX_SO2E}.
#' @param geo The country code. The special value \code{'germany_1995'} will return the 
#' replication dataset \code{\link{germany_airpol}}.
#' @param year The year.  The average employment will be created for the given
#' year, starting with \code{2008}, when the NACE Rev 2 was introduced in 
#' employment statistics.
#' @param unit Defaults to \code{"THS_T"} (thousand tons.)
#' @param data_directory Defaults to \code{NULL}, if a valid directory, it will try to save the pre-processed 
#' data file here with labelling. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing downloaded file
#' in the \code{data_directory} or the temporary directory, if it exists.
#' @importFrom dplyr filter select mutate left_join full_join inner_join relocate everything group_by
#' @importFrom dplyr summarise rename case_when
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @importFrom eurostat get_eurostat
#' @importFrom rlang .data
#' @param data_directory Defaults to \code{NULL}, if a valid directory, it will 
#' try to save the pre-processed data file here with labelling. 
#' @param force_download Defaults to \code{TRUE}. If \code{FALSE} it will use the existing downloaded file
#' in the \code{data_directory} or the temporary directory, if it exists.
#' @source Eurostat folder  
#' \href{https://ec.europa.eu/eurostat/web/products-datasets/-/env_ac_ainah_r2}{Air emissions accounts by NACE Rev. 2 activity}
#' @return A data.frame with auxiliary metadata to conform the symmetric
#' input-output tables.
#' @family import functions
#' @autoglobal
#' @examples 
#' airpol_get(airpol = "CO2", geo="germany_1995", year = 1995, unit = "THS_T") 
#' @export

airpol_get <- function( airpol = "GHG", geo="BE", year = 2020, unit = "THS_T", 
                        data_directory = NULL, force_download = TRUE) {
  
  
  if ( geo == "germany_1995") {
    ## Avoid large examples on CRAN
    airpol_input <- airpol
    return_df <- getdata('germany_airpol') %>%
      filter ( airpol %in% airpol_input ) %>%
      select ( iotables_col, value ) %>%
      pivot_wider(names_from = iotables_col, 
                  values_from = value) %>%
      mutate ( indicator = paste0(airpol_input, "_emission")) %>%
      relocate ( indicator, .before = everything())
    return(return_df)
  }
  
  if ( force_download ) {
    tmp <- eurostat::get_eurostat("env_ac_ainah_r2")
  } else {
    tmp <- tempdir_data("env_ac_ainah_r2")
  }
  
  if (!is.null(data_directory)) {
    if ( dir.exists(data_directory) ) {
      # If there is a temporary saving location given, save the data there.
      saveRDS(tmp, file = file.path(data_directory, "env_ac_ainah_r2.rds")) 
      }
  }
  
  assert_that(
    airpol %in% tmp$airpol, 
    msg = glue("{airpol} is not recognized as an air pollutant in Eurostat table env_ac_ainah_r2")
  )
  
  airpol_df <- tmp[ tmp$airpol == airpol, ]
  
  assert_that(
    geo %in% airpol_df$geo, 
    msg = glue("No data for geo='{geo}' with airpol='{airpol}' in year={year} and unit='{unit}' in Eurostat table env_ac_ainah_r2")
  )
  
  airpol_df <- airpol_df[airpol_df$geo == geo,]
  
  assertthat::assert_that(
    as.Date(paste0(year, "-01-01")) %in% airpol_df$time, 
    msg = glue::glue("No data for year={year} with geo='{geo}'an airpol='{airpol}' and unit='{unit}' in Eurostat table env_ac_ainah_r2")
  )
  
  airpol_df <- airpol_df[airpol_df$time == as.Date(paste0(year, "-01-01")),] 
  
  assertthat::assert_that(
    unit %in% airpol_df$unit, 
    msg = glue::glue("No data for unit='{unit}' with geo='{geo}', airpol='{airpol}' and geo='{geo'} in Eurostat table env_ac_ainah_r2")
  )
  
  airpol_df <- airpol_df[airpol_df$unit == unit,] 
  
  prod_na <- c('CPA_A01', 'CPA_A02', 'CPA_A03', 'CPA_B', 'CPA_C10-12', 'CPA_C13-15', 'CPA_C16',
               'CPA_C17', 'CPA_C18', 'CPA_C19', 'CPA_C20', 'CPA_C21', 'CPA_C22', 'CPA_C23', 'CPA_C24', 
               'CPA_C25', 'CPA_C26', 'CPA_C27', 'CPA_C28', 'CPA_C29', 'CPA_C30', 'CPA_C31_32', 'CPA_C33',
               'CPA_D', 'CPA_E36', 'CPA_E37-39', 'CPA_F', 'CPA_G45', 'CPA_G46', 'CPA_G47', 'CPA_H49', 
               'CPA_H50', 'CPA_H51', 'CPA_H52', 'CPA_H53', 'CPA_I', 'CPA_J58', 'CPA_J59_60', 'CPA_J61',
               'CPA_J62_63', 'CPA_K64', 'CPA_K65', 'CPA_K66', 'CPA_L68A', 'CPA_L68B', 'CPA_M69_70',
               'CPA_M71', 'CPA_M72', 'CPA_M73', 'CPA_M74_75', 'CPA_N77', 'CPA_N78', 'CPA_N79', 
               'CPA_N80-82', 'CPA_O', 'CPA_P', 'CPA_Q86', 'CPA_Q87_88', 'CPA_R90-92', 'CPA_R93', 
               'CPA_S94', 'CPA_S95', 'CPA_S96', 'CPA_T', 'CPA_U', 'TOTAL')
  
  country_ghg <- airpol_df %>%
    mutate ( nace_r2 = ifelse (.data$nace_r2=="TOTAL",
                               yes = "TOTAL", 
                               no = paste0("CPA_", nace_r2))
    ) %>%
    mutate ( nace_r2 = case_when ( 
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
      TRUE ~ nace_r2))
  
  ghg <- tibble (
    nace_r2 = prod_na
  )
  
  direct_match <- country_ghg  %>%
    inner_join (ghg, by = 'nace_r2')
  
  L68 <- tibble ( 
    nace_r2  = c('CPA_L68A', 'CPA_L68B'), 
    values = c(0,0)
  )
  
  group_match <- country_ghg %>%
    rename ( nace  = nace_r2) %>%
    mutate ( nace_r2 = substr(.data$nace, 1,5)) %>%
    group_by ( airpol, unit, geo, time, nace_r2 ) %>%
    summarise ( values = sum(.data$values), .groups="keep") %>%
    inner_join ( ghg, by = "nace_r2" )
  
  
  return_df <- ghg %>% left_join (
    direct_match %>%
      left_join ( group_match, by = c("airpol", "nace_r2", "unit", "geo", "time", "values") ) %>%
      select ( nace_r2, values) %>%
      full_join (L68, by = c("nace_r2", "values") ), 
    by = "nace_r2"
  ) %>%
    pivot_wider ( names_from = nace_r2, 
                  values_from = values) %>%
    mutate ( indicator = paste0(airpol, "_emission")) %>%
    relocate ( indicator, .before = everything())
  
  return_df
  
}
