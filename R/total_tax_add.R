#' Add a total tax row (D.2–D.3 and D.29–D.39)
#'
#' @description
#' Create and append a **total tax** row by summing selected tax rows in the
#' primary inputs block (Quadrant III) of a SIOT or use table.
#'
#' @details
#' In Eurostat/ESA terminology, tax rows commonly include:
#' - **Taxes less subsidies on products** (codes **D.2 – D.3**), and
#' - **Other net taxes on production** (codes **D.29 – D.39**).
#' These appear in the value-added (primary inputs) section of the use/SIOT
#' layout. :contentReference[oaicite:1]{index=1}
#'
#' The function sums the specified rows **column-wise** over all numeric
#' columns and appends the result as `total_tax_name`. If a household final
#' consumption column is present (e.g. `final_consumption_households` or
#' `p3_s14`), any missing value in the new total row is replaced by zero.
#'
#' @param data_table A symmetric input–output table (SIOT) or use table
#'   whose primary inputs include tax rows (see Details). Typically obtained
#'   via [iotable_get()].
#' @param tax_names Character vector of row labels to sum. Defaults to
#'   `c("d21x31","d29x39")`, shorthand for D.2–D.3 and D.29–D.39. Matching
#'   is currently made against the **lower-cased key column**.
#' @param total_tax_name Character scalar for the new row label. Default
#'   `"TOTAL_TAX"`. (See Enhancements regarding case handling.)
#'
#' @return
#' A data frame like `data_table`, with one additional row named
#' `total_tax_name` that equals the element-wise sum of the rows in
#' `tax_names` over numeric columns.
#'
#' @section Terminology:
#' Eurostat uses the lines “Taxes less subsidies on products” and “Other net
#' taxes on production” in published tables; these correspond, respectively,
#' to D.2–D.3 and D.29–D.39. :contentReference[oaicite:2]{index=2}
#'
#' @examples
#' de_io <- iotable_get()
#' total_tax_add(
#'   data_table    = de_io,
#'   tax_names     = c("net_tax_products", "net_tax_production"),
#'   total_tax_name = "total_tax"
#' )
#'
#' @family iotables processing functions
#' @importFrom dplyr full_join summarise mutate across
#' @importFrom tidyselect where
#' @export

total_tax_add <- function(data_table,
                          tax_names = c("d21x31", "d29x39"),
                          total_tax_name = "TOTAL_TAX") {
  . <- NULL

  if (is.null(tax_names)) {
    stop("Tax names must be set.")
  }

  key_column <- tolower(as.character(unlist(data_table[, 1])))

  if (!all(tax_names %in% key_column)) {
    stop(
      "The tax names ",
      paste(tax_names, collapse = ", "),
      " (not case sensitive) were not found in the data table."
    )
  }

  tax <- data_table[which(key_column %in% tax_names), ]

  tax <- summarise(tax, across(where(is.numeric), sum)) %>%
    cbind(data_table[1, 1], .) %>%
    dplyr::mutate(across(where(is.factor), as.character))

  tax[1, 1] <- total_tax_name

  names(tax)[1] <- names(data_table)[1]

  siot_ext <- full_join(
    mutate(data_table, across(where(is.factor), as.character)), tax,
    by = names(tax)
  )

  if (any(c("final_consumption_households", "p3_s14") %in% tolower(names(siot_ext)))) {
    household_col <- which(tolower(names(siot_ext)) %in% c("final_consumption_households", "p3_s14"))
    new_row <- which(tolower(as.character(siot_ext[, 1])) %in% total_tax_name)

    siot_ext[new_row, household_col] <- ifelse(test = is.na(siot_ext[new_row, household_col]),
      yes  = 0,
      no   = siot_ext[new_row, household_col]
    )
  } # end of households case

  siot_ext
}
