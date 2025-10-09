#' Download input–output tables (Eurostat only)
#'
#' @description
#' Downloads and prepares symmetric input–output (IO) or supply–use tables
#' directly from Eurostat. This modern version excludes all built-in
#' datasets (`germany_1995`, `croatia_2010_*`, `uk_2010_*`) which are handled
#' internally by [iotable_get()].
#'
#' @details
#' Files are cached under `tempdir()` as RDS (e.g. `"naio_10_cp1750_processed.rds"`).
#' The temporary directory is cleared when the R session ends. To persist
#' downloads across sessions (recommended for analytics), supply
#' `data_directory`.
#'
#' @param source Character. Eurostat dataset ID (e.g. `"naio_10_cp1700"`).
#' @param geo Country code (optional).
#' @param year Numeric (optional).
#' @param unit Character, usually `"MIO_EUR"` or `"MIO_NAC"`.
#' @param stk_flow Stock/flow indicator (`"DOM"`, `"IMP"`, `"TOTAL"`), optional.
#' @param data_directory Directory to save processed data (default: `tempdir()`).
#' @param force_download Logical, whether to force a new Eurostat download.
#'
#' @return
#' A **nested tibble** with metadata columns (`geo`, `year`, `unit`, etc.)
#' and a list-column `data` containing tidy IO tables.
#'
#' @family import functions
#' @importFrom utils object.size
#' @importFrom glue glue
#' @importFrom dplyr rename mutate select filter
#' @importFrom tidyselect any_of
#' @importFrom tidyr nest
#' @importFrom rlang set_names
#' @importFrom eurostat get_eurostat label_eurostat set_eurostat_cache_dir
#' @export
iotables_download <- function(source = "naio_10_cp1700",
                              geo = NULL,
                              year = NULL,
                              unit = NULL,
                              stk_flow = NULL,
                              data_directory = NULL,
                              force_download = FALSE) {
  data_directory <- data_directory # force it into local environment

  # ---- Guard clause: block built-in datasets ------------------------------
  builtin_sources <- c(
    "germany_1995",
    "croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900",
    "uk_2010", "uk_2010_siot", "uk_2010_coeff", "uk_2010_inverse",
    "netherlands_2000"
  )

  if (source %in% builtin_sources) {
    stop(glue::glue(
      "The dataset '{source}' is a built-in example handled by ",
      "[iotable_get_builtin()].\n",
      "Use that function instead of iotables_download()."
    ), call. = FALSE)
  }

  if (is.null(force_download)) force_download <- FALSE

  # ---- Validate source and setup cache --------------------------------------
  validate_source(source)
  cache_dir <- file.path(tempdir(), "eurostat")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  eurostat::set_eurostat_cache_dir(cache_dir)

  # ---- Download from Eurostat ----------------------------------------------
  if (!is.null(geo) || !is.null(year) || !is.null(unit) || !is.null(stk_flow)) {
    # Use lightweight JSON downloader when filters are given
    filters <- list()
    if (!is.null(geo)) filters$geo <- geo
    if (!is.null(year)) filters$time <- as.character(year)
    if (!is.null(unit)) filters$unit <- unit
    if (!is.null(stk_flow)) filters$stk_flow <- stk_flow

    downloaded <- get_eurostat_filtered(
      id      = as.character(source),
      filters = filters
    )

    message("Downloaded filtered subset (", nrow(downloaded), " rows).")
  } else {
    # Fallback to full Eurostat dataset + caching
    downloaded <- tryCatch(
      eurostat::get_eurostat(
        id        = source,
        cache     = !force_download,
        cache_dir = cache_dir
      ),
      error = function(e) {
        stop("Eurostat download failed for '", source, "': ", conditionMessage(e))
      }
    )
    message("Downloaded full dataset (", format(object.size(downloaded), units = "MiB"), ").")
  }

  message("Downloaded ", format(object.size(downloaded), units = "MiB"))

  # ---- Label using Eurostat vocabulary -------------------------------------
  downloaded_labelled <- downloaded %>%
    eurostat::label_eurostat(fix_duplicated = TRUE) %>%
    dplyr::mutate(rows = seq_len(nrow(downloaded))) %>%
    rlang::set_names(paste0(names(.), "_lab")) %>%
    dplyr::rename(rows = rows_lab) %>%
    dplyr::select(-tidyselect::any_of("values_lab")) %>%
    {
      # inside braces to allow conditional rename
      if ("TIME_PERIOD_lab" %in% names(.)) {
        dplyr::rename(., time_lab = TIME_PERIOD_lab)
      } else {
        .
      }
    }
  # ---- Standardize time and year fields ------------------------------------
  downloaded_time <- downloaded %>%
    dplyr::rename_with(~ gsub("TIME_PERIOD", "time", .x)) %>%
    dplyr::mutate(year = lubridate::year(time))

  # ---- Combine labelled + raw versions -------------------------------------
  combined <- downloaded_time %>%
    dplyr::mutate(rows = seq_len(nrow(downloaded_time))) %>%
    dplyr::left_join(downloaded_labelled, by = "rows") %>%
    dplyr::select(-rows)

  # ---- Sanity checks -------------------------------------------------------
  assertthat::assert_that(
    is.numeric(combined$values),
    msg = "combined$values must be numeric."
  )

  # ---- Free up memory -----------------------------------------------
  rm(downloaded_labelled)
  invisible(gc())

  # ---- Nest by metadata (identical to legacy schema) -----------------------
  nesting_vars <- c(
    "geo", "geo_lab", "time", "time_lab",
    "year", "unit", "unit_lab",
    "stk_flow", "stk_flow_lab"
  )

  non_nesting_vars <- setdiff(names(combined), nesting_vars)

  if (length(non_nesting_vars) == 0) {
    # lightweight download already corresponds to one table
    downloaded_nested <- tibble::tibble(
      geo = unique(combined$geo),
      year = unique(combined$year),
      unit = unique(combined$unit),
      stk_flow = unique(combined$stk_flow),
      time = unique(combined$time),
      data = list(combined)
    )
    message("Lightweight download contains a single SIOT; wrapped into nested structure.")
  } else {
    message("Creating nested structure...")
    downloaded_nested <- tidyr::nest(combined,
      data = -dplyr::any_of(nesting_vars)
    )
    message("Nesting done.")
  }

  # ---- Validation of nested structure --------------------------------------
  if (nrow(downloaded_nested) > 0) {
    assertthat::assert_that(
      all(
        vapply(
          downloaded_nested$data,
          function(df) is.numeric(df$values),
          logical(1)
        )
      ),
      msg = "Each nested table must have numeric `values`."
    )
  }

  # ---- Save ----------------------------------------------------------------
  # Build a safe and descriptive filename
  filter_suffix <- c()

  if (!is.null(geo)) filter_suffix <- c(filter_suffix, paste0("geo-", geo))
  if (!is.null(year)) filter_suffix <- c(filter_suffix, paste0("year-", year))
  if (!is.null(unit)) filter_suffix <- c(filter_suffix, paste0("unit-", unit))
  if (!is.null(stk_flow)) filter_suffix <- c(filter_suffix, paste0("stk-", stk_flow))

  # Join with underscores, sanitize for filesystem
  filter_suffix <- paste(filter_suffix, collapse = "_")
  if (filter_suffix != "") filter_suffix <- paste0("_", gsub("[^A-Za-z0-9_\\-]", "", filter_suffix))

  base_filename <- paste0(source, filter_suffix, "_processed.rds")

  save_path <- if (is.null(data_directory)) {
    file.path(tempdir(), base_filename)
  } else {
    assertthat::assert_that(
      dir.exists(data_directory),
      msg = paste("data_directory not found:", data_directory)
    )
    file.path(data_directory, base_filename)
  }

  saveRDS(downloaded_nested, save_path, version = 2)

  if (interactive()) message("Saved processed file to ", save_path)

  # ---- Final cleanup -----------------------------------------------
  vars_to_remove <- c("downloaded_labelled", "downloaded_time", "combined")
  vars_to_remove <- vars_to_remove[vars_to_remove %in% ls()]
  if (length(vars_to_remove) > 0) rm(list = vars_to_remove, envir = environment())
  invisible(gc())

  # ---- Return --------------------------------------------------------------
  downloaded_nested
}
