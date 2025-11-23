#' @keywords internal
iotable_get_new <- function(labelled_io_data = NULL,
                            source = "naio_10_cp1700",
                            geo = "BE",
                            year = 2020,
                            unit = "MIO_EUR",
                            stk_flow = "DOM",
                            labelling = "iotables",
                            data_directory = NULL,
                            force_download = FALSE) {
  # ---- 1. Handle built-in / legacy datasets ---------------------------------
  if (source %in% c(
    "germany_1995",
    "croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900"
  )) {
    return(iotable_get_builtin(source, labelling, year, geo, unit))
  }

  # ---- 2. Handle UK 2010 Analytical Tables ----------------------------------
  if (grepl("^uk_2010", source)) {
    # UK tables are stored internally and need no download
    return(iotable_get_uk_saved(source = source, labelling = labelling))
  }

  # ---- 3. Handle Eurostat datasets ------------------------------------------
  validate_source(source)

  if (source %in% c(
    "naio_10_cp1620", "naio_10_cp1630",
    "naio_10_pyp1620", "naio_10_pyp1630"
  )) {
    if (stk_flow != "TOTAL") {
      warning("Stock/flow forced to 'TOTAL' for tax and margin tables.")
      stk_flow <- "TOTAL"
    }
  }

  # ---- 4. Retrieve or reuse bulk dataset ------------------------------------
  if (is.null(labelled_io_data)) {
    labelled_io_data <- iotables_download(
      source = source,
      data_directory = data_directory,
      force_download = force_download
    )
  }

  # ---- 5. Parameter checks ---------------------------------------------------
  if (!geo %in% labelled_io_data$geo) {
    stop(glue::glue("No data for geo='{geo}' in {source}."))
  }
  if (!unit %in% labelled_io_data$unit) {
    stop(glue::glue("No data for unit='{unit}' in {source}."))
  }
  if (!year %in% labelled_io_data$year) {
    stop(glue::glue("No data for year={year} in {source}."))
  }
  if (!stk_flow %in% labelled_io_data$stk_flow) {
    warning(glue::glue("stk_flow='{stk_flow}' not found; using first available entry."))
  }

  # ---- 6. Select table from nested tibble -----------------------------------
  selected <- labelled_io_data %>%
    dplyr::filter(
      .data$geo == geo,
      .data$unit == unit,
      .data$year == year,
      .data$stk_flow == stk_flow
    )

  if (nrow(selected) != 1) {
    stop(glue::glue("No unique match for geo={geo}, year={year}, unit={unit}, stk_flow={stk_flow}."))
  }

  io_table <- selected$data[[1]]

  # ---- 7. Clean up -----------------------------------------------------------
  io_table <- io_table %>%
    dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
    dplyr::mutate(values = as.numeric(trimws(values)))

  # ---- 8. Attach metadata ----------------------------------------------------
  io_table_labelled <- io_table %>%
    iotable_attach_metadata(source = source, labelling = labelling)

  # ---- 9. Pivot to wide form -------------------------------------------------
  if (labelling == "iotables") {
    io_table_wide <- io_table_labelled %>%
      dplyr::arrange(iotables_row, iotables_col) %>%
      tidyr::pivot_wider(names_from = iotables_col, values_from = values)
  } else {
    io_table_wide <- io_table_labelled %>%
      tidyr::pivot_wider(names_from = induse, values_from = values)
  }

  # ---- 10. Optional save -----------------------------------------------------
  if (!is.null(data_directory)) {
    save_name <- file.path(
      data_directory,
      paste0(geo, "_", year, "_", source, "_", stk_flow, "_", unit, ".rds")
    )
    saveRDS(io_table_wide, save_name, version = 2)
    message("Saved processed IO table to: ", save_name)
  }

  io_table_wide
}
