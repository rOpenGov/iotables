#' Package globals
#'
#' Symbols declared here avoid R CMD check "no visible binding for global variable"
#' notes. This file is transitional: as functions are refactored to use
#' `.data$var` or `.env$var` pronouns, entries can be removed.
#'
#' @keywords internal
"_PACKAGE"

#' Package globals
#'
#' Symbols declared here avoid R CMD check "no visible binding for global variable"
#' notes. This file is transitional: as functions are refactored to use
#' `.data$var` or `.env$var` pronouns, entries can be removed.
#'
#' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  # pronouns / operators
  ".data",

  # airpol_get
  "iotables_col", "value", "indicator", "nace_r2", "time", "values",

  # employment_get
  "TIME_PERIOD", "code", "variable", "iotables_label",

  # iotable_get
  "time",

  # iotables_download
  "values_lab", "TIME_PERIOD_lab", "time_lab",

  # order_iotable
  "uk_col", "uk_row", "row_order", "col_order",
  "prod_na", "induse", "iotables_row", "iotables_col",
  "t_rows2", "t_cols2",

  # uk_2010_get
  "vars", "rowname", ".", "uk_col_lab", "uk_col",
  "uk_row_lab", "uk_row",

  # uk_2010_results_get
  "Product", "Rank...4", "Rank...6", "Rank...8",
  "Rank...10", "Rank...12",

  # vocabularies
  "label", "numeric_label",

  # get_metadata_cols
  "uk_row_label"
))
