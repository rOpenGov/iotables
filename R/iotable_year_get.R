#' Get available years from bulk IO tables
#'
#' @description
#' Query which years are available for a given Eurostat IO product,
#' country (`geo`), and currency unit in a bulk download.
#'
#' @details
#' This function is usually called indirectly via [iotable_get()].
#' You normally do not need to call [iotables_download()] yourself
#' unless working with bulk Eurostat files.
#'
#' Supported Eurostat products include (non-exhaustive):
#'
#' - `"naio_10_cp1700"` — Symmetric IO table, basic prices (product × product)
#' - `"naio_10_cp1750"` — Symmetric IO table, basic prices (industry × industry)
#' - `"naio_10_pyp1700"` — Symmetric IO table (product × product),
#'   previous years’ prices
#' - `"naio_10_pyp1750"` — Symmetric IO table (industry × industry),
#'   previous years’ prices
#' - `"naio_10_cp1620"` / `"naio_10_pyp1620"` — Trade & transport margins
#' - `"naio_10_cp1630"` / `"naio_10_pyp1630"` — Taxes less subsidies on products
#'
#' See the [Eurostat Symmetric Input–Output Tables page](
#' https://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/overview).
#'
#' @param source Character. Eurostat product code (see Details).
#' @param labelled_io_data Optional labelled IO data from
#'   [iotables_download()]. If supplied, avoids re-reading from disk.
#' @param geo Country code or name (e.g. `"SK"` or `"Slovakia"`).
#' @param unit Currency unit. Defaults to `"MIO_NAC"` (millions of national
#'   currency). Alternative: `"MIO_EUR"`.
#' @param stk_flow Flow type. Defaults to `"DOM"` (domestic output).
#'   Alternatives:
#'   - `"IMP"` for imports
#'   - `"TOTAL"` for total output
#'   For sources `"naio_10_cp1620"` (margins) and `"naio_10_cp1630"` (taxes),
#'   only `"TOTAL"` is used.
#' @param time_unit Return mode for time. `"year"` (default) returns numeric
#'   years; `"time"` returns a vector of dates.
#' @param data_directory Optional path used with [iotable_get()] or
#'   [iotables_download()] to persist bulk data.
#' @param force_download Logical. Defaults to `TRUE`. If `FALSE`, reuse an
#'   existing file in `data_directory` or `tempdir()` when available.
#'
#' @return A numeric vector of years, or a date vector if
#'   `time_unit = "time"`.
#'
#' @importFrom dplyr filter select mutate rename left_join arrange across
#' @importFrom forcats fct_reorder
#' @importFrom lubridate year
#' @family iotables processing functions
#'
#' @examples
#' germany_years <- iotable_year_get(
#'   source = "germany_1995", geo = "DE", unit = "MIO_EUR"
#' )
#' # Return as dates
#' germany_dates <- iotable_year_get(
#'   source = "germany_1995", geo = "DE",
#'   unit = "MIO_EUR", time_unit = "time"
#' )
#'
#' @export

iotable_year_get <- function(labelled_io_data = NULL,
                             source = "germany_1995",
                             geo = "DE",
                             unit = "MIO_EUR",
                             time_unit = "year",
                             stk_flow = "TOTAL",
                             data_directory = NULL,
                             force_download = TRUE) {
  ## Initialize variables ------------
  # This function needs to be modernized at one point.  It does what it should but it is
  # superflous and uses old non-standard evaluation.
  values <- . <- NULL # non-standard evaluation creates a varning in build.
  time <- t_cols2 <- t_rows2 <- by_row <- by_col <- tmp_rds <- NULL
  account_group <- digit_1 <- digit_2 <- group <- quadrant <- NULL
  iotables_row <- iotables_col <- prod_na <- induse <- variable <- NULL
  row_order <- col_order <- iotables_label <- code <- numeric_label <- label <- NULL

  source_inputed <- source
  unit_input <- unit
  geo_input <- geo
  stk_flow_input <- stk_flow

  if (source %in% c(
    "naio_10_cp1620", "naio_10_cp1630",
    "naio_10_pyp1620", "naio_10_pyp1630"
  )
  ) {
    stk_flow_input <- "TOTAL" # tax and margin tables only have one version
  }

  if (!time_unit %in% c("year", "time")) {
    time_unit <- "year"
  }
  if (source == "germany_1995") {
    time_unit <- "time"
  }



  ## Veryfing source parameter and loading the labelling  ----
  prod_ind <- c(
    "naio_10_cp1700", "naio_10_cp1750", "naio_10_pyp1700",
    "naio_10_pyp1750", "naio_10_cp1620", "naio_10_cp1630",
    "naio_10_pyp1620", "naio_10_pyp1630"
  )
  trow_tcol <- c("croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")
  croatia_files <- c("croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")

  if (source %in% prod_ind) {
    metadata_rows <- iotables::metadata %>% # tables that follow prod_ind vocabulary
      filter(variable == "prod_na") %>%
      dplyr::rename(
        prod_na = code,
        prod_na_lab = label,
        row_order = numeric_label,
        iotables_row = iotables_label
      )

    metadata_cols <- iotables::metadata %>%
      dplyr::filter(variable == "induse") %>%
      dplyr::rename(
        induse = code,
        induse_lab = label,
        col_order = numeric_label,
        iotables_col = iotables_label
      )
  } else if (source %in% trow_tcol) {
    # tables that follow trow_tcol vocabulary

    metadata_rows <- iotables::metadata %>%
      dplyr::filter(variable == "t_rows") %>%
      dplyr::rename(
        t_rows2 = code,
        t_rows2_lab = label,
        row_order = numeric_label,
        iotables_row = iotables_label
      )

    metadata_cols <- iotables::metadata %>%
      dplyr::filter(variable == "t_cols") %>%
      dplyr::rename(
        t_cols2 = code,
        t_cols2_lab = label,
        col_order = numeric_label,
        iotables_col = iotables_label
      )
  } else if (source == "germany_1995") { # German simplified tables
    metadata_rows <- germany_metadata_rows
    metadata_cols <- germany_metadata_cols
  } else {
    stop("This type of input-output database is not (yet) recognized by iotables.")
  }

  metadata_rows <- mutate(metadata_rows, across(where(is.factor), as.character))
  metadata_cols <- mutate(metadata_cols, across(where(is.factor), as.character))

  # Exception handling for wrong paramters -----------------------
  if (is.null(labelled_io_data)) {
    # if not directly inputed data
    if (is.null(geo)) stop("Error: no country selected.")

    if (!unit %in% c("MIO_NAC", "MIO_EUR", "T_NAC")) {
      stop("Currency unit must be MIO_NAC, MIO_EUR or T_NAC")
    }
    if (source %in% c("naio_10_cp1620", "naio_10_cp1630")) {
      if (stk_flow != "TOTAL") {
        stk_flow_input <- "TOTAL"
        warning("The parameter stk_flow was changed to TOTAL.")
      }
    }

    # Creating a temporary file name for the input-output table -------------
    tmp_rds1 <- file.path(
      tempdir(), paste0(source, "_iotables.rds")
    ) # if iotables labelled version was created earlier
    tmp_rds2 <- file.path(
      tempdir(), paste0(source, "_short.rds")
    ) # if short labelled version was created earlier
    tmp_rds3 <- file.path(
      tempdir(), paste0(source, ".rds")
    ) # if non-labelled was created earlier
    if (source_inputed == "germany_1995") {
      labelled_io_data <- iotables::germany_1995 # use germany example
    } else if (source_inputed == "croatia_2010_1700") {
      labelled_io_data <- iotables::croatia_2010_1700 %>%
        mutate(year = lubridate::year(time))
    } else if (source_inputed == "croatia_2010_1800") {
      labelled_io_data <- iotables::croatia_2010_1800 %>%
        mutate(year = lubridate::year(time))
    } else if (source_inputed == "croatia_2010_1900") {
      labelled_io_data <- iotables::croatia_2010_1900 %>%
        mutate(year = lubridate::year(time))
    } else {
      if (any(c(tmp_rds1, tmp_rds2, tmp_rds3) %in%
        list.files(path = tempdir()))) {
        tmp_rds <- c(tmp_rds1, tmp_rds2, tmp_rds3)[which(!is.null(c(tmp_rds1, tmp_rds2, tmp_rds3)))]

        labelled_io_data <- readRDS(tmp_rds)
      } else { # getting or downloading the bulk longform data
        labelled_io_data <- iotables_download(source,
          data_directory = data_directory,
          force_download = force_download
        )
      }
    } # use eurostat files
  } # end of possible downloads or data retrieval if not directly inputed

  ## Veryfing parameters ----

  if (nchar(geo_input) == 2 & geo_input == tolower(geo_input)) {
    geo_input <- toupper(geo_input)
    warning("Warning: country code changed to upper case.")
  }

  if (!unit_input %in% labelled_io_data$unit) {
    stop("This currency unit is not found in the raw data frame.")
  }

  if (!geo_input %in% labelled_io_data$geo) {
    stop("This currency unit is not found in the raw data frame.")
  }

  ## Converting factors to characters ------

  selected_tables <- which( ## get the number of table to be selected
    as.character(labelled_io_data$geo) == geo &
      labelled_io_data$unit == unit
  )

  if (time_unit == "year") {
    return_values <- sort(unique(labelled_io_data$year[selected_tables]))
  } else {
    return_values <- sort(unique(labelled_io_data$time[selected_tables]))
  }

  if (length(return_values > 0)) {
    # message ( "The following years are available for ", geo, " in ", unit , " currency units:\n",
    #           paste(return_values, collapse = '; ' ), ".")
  } else {
    warning(
      "No tables are available for ",
      geo, " in ", unit, " currency units."
    )
  }

  return_values
}
