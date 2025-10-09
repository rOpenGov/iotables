#' Add supplementary rows to an IO/SUT table
#'
#' @description
#' Append supplementary indicators (e.g., emissions coefficients) as new
#' rows to a symmetric input–output table (SIOT), use, supply, or margins
#' table. This is a light wrapper around [rows_add()].
#'
#' @details
#' Column names in `supplementary_data` must match the numeric columns of
#' `data_table`. If the key column is missing, it is created from
#' `supplementary_names` or auto-generated as `supplementary_row_#`.
#'
#' When a household final consumption column is present (e.g.,
#' `final_consumption_households`, `P3_S14`), new rows get `0` in that
#' column if the supplied values are `NA`.
#'
#' For terminology, see Eurostat’s *Manual of Supply, Use and Input-Output
#' Tables*.
#' (Eurostat, 2008; ISBN 978-92-79-04704-3)
#'
#' @param data_table
#'   A SIOT, use, supply, or margins table (key column + numeric columns).
#' @param supplementary_data
#'   A data frame (or tibble) of one or more rows to add. It may already
#'   contain a key column (first column). Otherwise, provide
#'   `supplementary_names` or the keys will be auto-generated. All other
#'   column names must match `data_table`.
#' @param supplementary_names
#'   Optional character vector of row names for the key column; length
#'   must equal `nrow(supplementary_data)`. Ignored if a key column is
#'   already present.
#'
#' @references
#' Table 15.13 (p. 494) in Beutel (2008),
#'  *Eurostat Manual of Supply, Use and Input–Output Tables*,
#' shows an environmental‐emission model built by adding CO₂ and CH₄ rows to
#' the German 1995 input–output coefficients.  The same construction is
#' reproduced here by `supplementary_add()` and the results are checked
#' against this example.

#' @return
#' A `data.frame` with the rows of `supplementary_data` bound to
#' `data_table` and aligned to its key and numeric columns.
#'
#' @seealso [convert_industry_to_product()]
#'
#' @family iotables processing functions
#' @importFrom dplyr bind_cols
#' @examples
#' de_io <- iotable_get()
#' CO2_coefficients <- data.frame(
#'   agriculture_group = 0.2379,
#'   industry_group = 0.5172,
#'   construction = 0.0456,
#'   trade_group = 0.1320,
#'   business_services_group = 0.0127,
#'   other_services_group = 0.0530
#' )
#' CH4_coefficients <- data.frame(
#'   agriculture_group = 0.0349,
#'   industry_group = 0.0011,
#'   construction = 0,
#'   trade_group = 0,
#'   business_services_group = 0,
#'   other_services_group = 0.0021
#' )
#' CO2 <- cbind(
#'   data.frame(iotables_row = "CO2"),
#'   CO2_coefficients
#' )
#' CH4 <- cbind(
#'   data.frame(iotables_row = "CH4_coefficients"),
#'   CH4_coefficients
#' )

#' de_coeff <- input_coefficient_matrix_create ( iotable_get() )
#' emissions <- rbind (CO2, CH4)
#'
#' # Check with the Eurostat Manual page 494:
#' supplementary_add(de_io, emissions)
#' @export

supplementary_add <- function(data_table,
                              supplementary_data,
                              supplementary_names = NULL) {
  if (!is.null(supplementary_names)) {
    if (length(supplementary_names) !=
      nrow(as.data.frame(supplementary_data))) {
      stop("New names do not match the dimensions of the supplementary data.")
    }
  }

  if (!is_key_column_present(supplementary_data)) {
    key_column <- key_column_create(
      names(data_table)[1],
      ifelse(is.null(supplementary_names),
        yes = paste0("supplementary_row_", 1:nrow(supplementary_data)),
        no = supplementary_names
      )
    )

    supplementary_data <- bind_cols(
      key_column, supplementary_data
    )
  } else {
    key_column <- supplementary_data[, 1]
    names(key_column) <- names(data_table)[1]
  }

  siot_ext <- rows_add(data_table, rows_to_add = supplementary_data)

  if (any(c("final_consumption_households", "p3_s14") %in% tolower(names(siot_ext)))) {
    household_col <- which(tolower(names(siot_ext)) %in% c("final_consumption_households", "p3_s14"))
    new_rows <- which(tolower(as.character(siot_ext[, 1])) %in% key_column)
    siot_ext[new_rows, household_col] <- ifelse(is.na(siot_ext[new_rows, household_col]), 0, siot_ext[new_rows, household_col])
  }

  siot_ext
}
