#' Add Conforming Row(s) to an Input–Output Table
#'
#' Add a conforming row, or elements of a conforming row, to a named
#' input–output style data frame.
#'
#' @details
#' You can add rows in several ways:
#' * A **data frame** with one or more rows, where the first column contains
#'   row identifiers.
#' * A **named numeric vector**, which will be turned into a single-row
#'   data frame.
#'
#' If no `row_names` are supplied and the first column of `rows_to_add` is
#' numeric, new rows will be automatically labelled as `"new_row_1"`,
#' `"new_row_2"`, etc.
#'
#' Missing column values are filled with `empty_fill`, which defaults to `0`.
#' If you want to avoid division by zero in later computations, you can set
#' this to a very small value (e.g. `1e-6`).
#'
#' @param data_table A symmetric input–output table, a use table, a margins
#'   table, or a tax table retrieved by [iotable_get()].
#' @param rows_to_add A data frame or a named numeric vector containing the
#'   new row(s).
#' @param row_names Optional character vector giving names for the new key
#'   column. If `NULL`, names are inferred (see *Details*).
#' @param empty_fill Value used to fill missing columns. Defaults to `0`.
#'
#' @return
#' A `data.frame` containing the original `data_table` extended with the new
#' row(s).
#'
#' @family iotables processing functions
#' @importFrom dplyr bind_rows bind_cols
#'
#' @examples
#' rows_to_add <- data.frame(
#'   iotables_row = "CO2_emission",
#'   agriculture_group = 10448,
#'   industry_group = 558327, # construction is omitted
#'   trade_group = 11194
#' )
#'
#' rows_add(iotable_get(), rows_to_add = rows_to_add)
#'
#' rows_add(iotable_get(),
#'   rows_to_add = c(
#'     industry_group = 1534,
#'     trade_group = 4
#'   ),
#'   row_names = "CH4_emission"
#' )
#'
#' @export
rows_add <- function(data_table,
                     rows_to_add,
                     row_names = NULL,
                     empty_fill = 0) {
  if (is.numeric(rows_to_add)) {
    rows_to_add <- as.data.frame(t(rows_to_add))
  }
  
  if (is.null(row_names)) {
    if (!is.numeric(rows_to_add[, 1])) {
      key_column <- key_column_create(
        key_column_name = names(data_table)[1],
        key_column_values = unlist(rows_to_add[, 1])
      )
    } else {
      key_column <- key_column_create(
        key_column_name = names(data_table)[1],
        key_column_values = paste0("new_row_", seq_len(nrow(rows_to_add)))
      )
    }
  } else {
    if (nrow(rows_to_add) != length(row_names)) {
      stop("The number of rows to add and the number of row_names do not match.")
    }
    key_column <- key_column_create(
      key_column_name = names(data_table)[1],
      key_column_values = row_names
    )
  }
  
  columns_required <- ifelse(
    is_key_column_present(data_table),
    ncol(data_table) - 1,
    ncol(data_table)
  )
  
  numeric_names <- if (is_key_column_present(data_table)) {
    names(data_table)[-1]
  } else {
    names(data_table)
  }
  
  empty_values <- as.data.frame(
    matrix(
      rep(empty_fill, columns_required * nrow(rows_to_add)),
      ncol = columns_required
    )
  )
  names(empty_values) <- numeric_names
  empty_values <- bind_cols(key_column, empty_values)
  
  completed <- if (!is.numeric(rows_to_add[, 1])) {
    rows_to_add[, -1]
  } else {
    rows_to_add
  }
  
  to_complete_rows <- empty_values[, !names(empty_values) %in% names(completed)]
  dplyr::bind_rows(data_table, bind_cols(completed, to_complete_rows))
}
