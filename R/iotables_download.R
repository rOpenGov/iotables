#' Download input–output tables (Eurostat)
#'
#' @description
#' Download standard input–output (IO) and related tables. At the moment,
#' only Eurostat products are supported. You usually do not need to call
#' this directly; [iotable_get()] will invoke it as needed and return a
#' filtered, tidy table.
#'
#' @details
#' Files are cached under `tempdir()` as RDS (e.g., `"naio_10_cp1750.rds"`).
#' The temporary directory is cleared when the R session ends. To persist
#' downloads across sessions (recommended for analytics), supply
#' `data_directory` and the processed, **nested** output will also be
#' written there as `"<source>_processed.rds"`.
#'
#' Supported Eurostat products include (non-exhaustive):
#'
#' - `naio_10_cp1700` — Symmetric IO table, basic prices (product × product)
#' - `naio_10_pyp1700` — Same, previous years’ prices
#' - `naio_10_cp1750` — Symmetric IO table, basic prices (industry × industry)
#' - `naio_10_pyp1750` — Same, previous years’ prices
#' - `naio_10_cp15` — Supply table at basic prices incl. margins/taxes
#' - `naio_10_cp16` — Use table at purchasers’ prices
#' - `naio_10_cp1610` — Use table at basic prices
#' - `naio_10_pyp1610` — Use table at basic prices (previous years’ prices)
#' - `naio_10_cp1620` — Trade and transport margins at basic prices
#' - `naio_10_pyp1620` — Trade and transport margins at previous years’ prices
#' - `naio_10_cp1630` — Taxes less subsidies on products at basic prices
#' - `naio_10_pyp1630` — Taxes less subsidies on products, prev. years’ prices
#' - `uk_2010` — United Kingdom IO Analytical Tables (handled internally)
#'
#' Eurostat API/format changes (e.g., `TIME_PERIOD` vs `time`) are handled
#' for backward compatibility.
#'
#' @param source Character. The Eurostat product code (see above) or `"uk_2010"`.
#' @param data_directory Optional directory path where the processed nested
#'   tables will be saved as `"<source>_processed.rds"`. If `NULL` (default),
#'   results are saved to `tempdir()`.
#' @param force_download Logical. If `FALSE` (default), reuse a cached file
#'   in `data_directory` or `tempdir()` when available. If `TRUE`, force a
#'   fresh download from Eurostat.
#'
#' @return
#' A **nested** `data.frame` (one row per IO table) with metadata columns
#' (`geo`, `unit`, `year`, `stk_flow`, etc.) and a list-column `data`
#' containing the tidy table for each combination.
#'
#' @examples
#' \donttest{
#' io_tables <- iotables_download(source = "naio_10_pyp1750")
#' }
#'
#' @family import functions
#' @importFrom dplyr filter select mutate left_join rename any_of
#' @importFrom tidyr nest
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom lubridate year
#' @importFrom rlang set_names
#' @importFrom glue glue
#' @importFrom assertthat assert_that
#' @export


iotables_download <- function(source = "naio_10_cp1700",
                              data_directory = NULL,
                              force_download = FALSE) {
  ## Parameter validation ---------------------------------------------
  if (!source %in% c("uk_2010", "germany_1995")) {
    validate_source(source)
    downloaded <- tempdir_data(source, force_download)
  } else if (source == "uk_2010") {
    return(uk_2010_get())
  }

  if (all(c("year", "data") %in% names(downloaded))) {
    # This is already processed
    message("Returning the processed SIOTs from tempdir. You can override this with force_download=TRUE.")
    return(downloaded)
  }

  assert_that(
    "data.frame" %in% class(downloaded) & ncol(downloaded) > 6 & nrow(downloaded) > 1,
    msg = glue("The download of {source} was not successful.")
  )

  lab_names <- paste0(names(downloaded), "_lab")

  # label the raw Eurostat file, add rename variables with _lab suffix
  downloaded_labelled <- downloaded %>%
    eurostat::label_eurostat(fix_duplicated = TRUE)

  assert_that(
    length(names(downloaded_labelled)) == length(lab_names),
    msg = "in iotables_download() ncol(downloaded_labelled) != ncol(downloaded)"
  )

  downloaded_labelled <- downloaded_labelled %>% # add meaningful labels to raw data
    rlang::set_names(lab_names) %>%
    mutate(rows = seq_len(nrow(downloaded))) %>% # because long and wide formats are not symmetric
    rename(values = values_lab)

  if ("TIME_PERIOD_lab" %in% names(downloaded_labelled)) {
    ## Breaking change in eurostat 4.0.0
    ## keep this for backward compatiblitiy
    downloaded_labelled <- downloaded_labelled %>%
      rename(time_lab = TIME_PERIOD_lab)
  }

  downloaded_labelled <- downloaded_labelled %>%
    mutate(year = lubridate::year(time_lab))

  # Join the labelled and the not labelled files, so that both versions are avialable

  downloaded <- downloaded %>%
    mutate(rows = seq_len(nrow(downloaded))) %>%
    left_join(downloaded_labelled, by = c("rows", "values"))

  if ("TIME_PERIOD" %in% names(downloaded)) {
    ## Breaking change in eurostat 4.0.0
    ## keep this for backward compatiblitiy
    downloaded <- downloaded %>%
      rename(time = TIME_PERIOD)
  }

  if (source == "naio_cp17_r2") {
    downloaded$t_cols2 <- plyr::mapvalues(
      downloaded$t_cols2,
      from = c(
        "CPA_N80-N82", "CPA_R90-R92", "CPA_E37-E39",
        "CPA_C10-C12", "CPA_C13-C15",
        "CPA_C31_C32", "CPA_J59_J60",
        "CPA_J62_J63", "CPA_M69_M70", "CPA_Q87_Q88",
        "CPA_M74_M75", "CPA_O84", "CPA_P85",
        "CPA_D35"
      ),
      to = c(
        "CPA_N80-82", "CPA_R90-92", "CPA_E37-39",
        "CPA_C10-12", "CPA_C13-15",
        "CPA_C31_32", "CPA_J59_60",
        "CPA_J62_63", "CPA_M69_70", "CPA_Q87_88",
        "CPA_M74_75", "CPA_O", "CPA_P", "CPA_D"
      )
    )

    downloaded$t_rows2 <- plyr::mapvalues(
      downloaded$t_rows2,
      from = c(
        "CPA_N80-N82", "CPA_R90-R92", "CPA_E37-E39",
        "CPA_C10-C12", "CPA_C13-C15",
        "CPA_C31_C32", "CPA_J59_J60",
        "CPA_J62_J63", "CPA_M69_M70", "CPA_Q87_Q88",
        "CPA_M74_M75", "CPA_O84", "CPA_P85", "CPA_D35"
      ),
      to = c(
        "CPA_N80-82", "CPA_R90-92", "CPA_E37-39",
        "CPA_C10-12", "CPA_C13-15",
        "CPA_C31_32", "CPA_J59_60",
        "CPA_J62_63", "CPA_M69_70", "CPA_Q87_88",
        "CPA_M74_75", "CPA_O", "CPA_P", "CPA_D"
      )
    )
  } # end of _r2


  if ("stk_flow" %in% names(downloaded)) {
    downloaded_nested <- nest(
      downloaded,
      data = -any_of(c(
        "geo", "geo_lab", "time", "time_lab",
        "year", "unit", "unit_lab", "stk_flow", "stk_flow_lab"
      ))
    )
  } else {
    downloaded_nested <- nest(
      downloaded,
      data = -any_of(c(
        "geo", "geo_lab", "time", "time_lab",
        "year", "unit", "unit_lab"
      ))
    )
  }

  if (!is.null(data_directory)) {
    assert_that(dir.exists(data_directory),
      msg = glue::glue("The data_directory={data_directory} does not exist.")
    )

    save_file_name <- file.path(data_directory, paste0(source, "_processed.rds")) # shoud have different name for processed
    message("Saving ", nrow(downloaded_nested), " input-output tables.")
    saveRDS(downloaded_nested, file = save_file_name, version = 2)
    message(
      "Saved the raw data of this table type in ",
      save_file_name, "."
    )
  } else {
    save_file_name <- file.path(tempdir(), paste0(source, "_processed.rds"))
    message("Saving ", nrow(downloaded_nested), " input-output tables into the temporary directory.")
    saveRDS(downloaded_nested, file = save_file_name, version = 2)
    message(
      "Saved the raw data of this table type in temporary directory ",
      save_file_name, "."
    )
  }

  downloaded_nested
}
