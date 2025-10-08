#' Get a single input–output table from built-in datasets
#'
#' @description
#' Filter and reshape one IO/SUT table from a bulk dataset (typically a
#' Eurostat download). In most workflows you will call this function
#' rather than [iotables_download()], which it invokes as needed.
#'
#' @note
#' This is a wrapper around the
#' legacy getter function now only used for built-in datasets.
#'
#' @details
#' The Eurostat bulk tables arrive in long form and are not ordered for
#' matrix algebra. This function selects the requested country (`geo`),
#' year, unit and stock/flow (`stk_flow`), joins iotables metadata for
#' consistent row/column labelling, and returns a **wide** table ready
#' for analysis.
#'
#' Supported sources include:
#'
#' - `naio_10_cp1700` — symmetric IO, basic prices (prod × prod)
#' - `naio_10_pyp1700` — previous years' prices
#' - `naio_10_cp1750` — symmetric IO, basic prices (ind × ind)
#' - `naio_10_pyp1750` — previous years' prices
#' - `naio_10_cp15` — supply at basic prices incl. margins/taxes
#' - `naio_10_cp16` — use at purchasers' prices
#' - `naio_10_cp1610` — use at basic prices
#' - `naio_10_cp1620` — trade & transport margins (basic prices)
#' - `naio_10_cp1630` — taxes less subsidies on products (basic prices)
#' - `naio_10_pyp*` — corresponding previous years' prices variants
#' - `germany_1995` — packaged Beutel example
#' - `croatia_2010_1700/1800/1900` — packaged examples
#' - `uk_2010_*` — packaged UK 2010 variants
#'
#' @param labelled_io_data Optional nested bulk data as returned by
#'   [iotables_download()]. If `NULL` (default), data are retrieved from
#'   cache or downloaded.
#' @param source Data source code (see list above).
#' @param geo Country code or name, e.g. `"SK"` or `"Slovakia"`.
#' @param year Numeric year. Defaults to `1990` for `germany_1995`.
#' @param unit Currency unit, usually `"MIO_NAC"` or `"MIO_EUR"`.
#' @param stk_flow Stock/flow: `"DOM"`, `"IMP"`, or `"TOTAL"`. For
#'   margins/taxes (`cp1620`, `cp1630` and `pyp` variants) only `"TOTAL"`
#'   is used; other inputs are coerced with a warning.
#' @param labelling Column naming scheme: `"iotables"` (default) for
#'   consistent names; `"short"` for original short codes; `"eurostat"`
#'   is treated as `"short"`.
#' @param data_directory Optional directory to save the processed wide
#'   table (RDS). If `NULL`, nothing is saved.
#' @param force_download Logical. If `TRUE`, force a fresh download when
#'   `labelled_io_data` is not supplied. Defaults to `TRUE`.
#'
#' @return
#' A **wide** `data.frame` representing the selected IO table, with a key
#' column followed by ordered numeric columns.
#'
#' @family iotables import functions
#'
#' @examples
#' germany_table <- iotable_get(
#'   source = "germany_1995",
#'   geo = "DE", year = 1990, unit = "MIO_EUR",
#'   labelling = "iotables"
#' )
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename left_join arrange
#' @importFrom dplyr across
#' @importFrom tidyselect any_of all_of where
#' @importFrom tidyr spread pivot_wider
#' @importFrom forcats fct_reorder
#' @importFrom lubridate year
#' @importFrom utils data
#' @export
iotable_get <- function(
    labelled_io_data = NULL,
    source = "germany_1995",
    geo = "DE",
    year = 1990,
    unit = "MIO_EUR",
    stk_flow = "DOM",
    labelling = "iotables",
    data_directory = NULL,
    force_download = TRUE) {
  # Built-in datasets (legacy branch)
  builtin_sources <- c(
    "germany_1995",
    "croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900",
    "uk_2010_siot", "uk_2010_coeff", "uk_2010_inverse"
  )

  # Switch
  if (source %in% builtin_sources) {
    # Using legacy iotable_get() now called iotable_get_builtin()
    return(iotable_get_builtin(
      labelled_io_data = labelled_io_data,
      source = source,
      geo = geo,
      year = year,
      unit = unit,
      stk_flow = stk_flow,
      labelling = labelling,
      data_directory = data_directory,
      force_download = force_download
    ))
  } else {
    # Wrapper around iotable_get_eurostat()
    return(iotable_get_eurostat(
      labelled_io_data = labelled_io_data,
      source = source,
      geo = geo,
      year = year,
      unit = unit,
      stk_flow = stk_flow,
      labelling = labelling,
      data_directory = data_directory,
      force_download = force_download
    ))
  }
}
