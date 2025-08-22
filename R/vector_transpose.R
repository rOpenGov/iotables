#' Transpose a Vector to Long Form
#'
#' Convert a wide-form vector (e.g., indicators or multipliers) into long form,
#' which is often more useful for printing or joining. This is a thin wrapper
#' around [tidyr::pivot_longer()], provided so you do not need to load **tidyr**
#' explicitly.
#'
#' @param data_table A `data.frame` or tibble. The first column is assumed to
#'   be a key column.
#' @param names_to Name of the new column containing previous column names.
#'   Default: `"nace_r2"`.
#' @param values_to Name of the new column containing the values. Default:
#'   `"value"`.
#' @param key_column_name Optional. New name for the first (key) column. If
#'   `NULL` (default), the name is not changed.
#' @param .keep Logical. If `TRUE`, keep the indicator identifier column. If
#'   `FALSE` (default), drop it.
#'
#' @return A tibble in long format with a key column and, if requested, the
#'   indicator identifier column.
#'
#' @family iotables processing functions
#'
#' @examples
#' vector_transpose_longer(
#'   data.frame(
#'     indicator = "my_indicator",
#'     agriculture = 0.0123,
#'     manufacturing = 0.1436,
#'     trade = 0.0921
#'   )
#' )
#'
#' # Keep the indicator column
#' vector_transpose_longer(
#'   data.frame(
#'     indicator = "my_indicator",
#'     agriculture = 0.0123,
#'     manufacturing = 0.1436
#'   ),
#'   .keep = TRUE
#' )
#' @export
vector_transpose_longer <- function(data_table,
                                    names_to = "nace_r2",
                                    values_to = "value",
                                    key_column_name = NULL,
                                    .keep = FALSE) {
  is_key_column_present(data_table)
  key_column <- names(data_table)[1]
  
  return_df <- data_table %>%
    tidyr::pivot_longer(
      -all_of(key_column),
      names_to  = names_to,
      values_to = values_to
    )
  
  if (.keep) return_df else return_df[, -1]
}

#' @rdname vector_transpose_longer
vector_transpose <- function(data_table,
                             names_to = "nace_r2",
                             values_to = "value",
                             key_column_name = NULL,
                             .keep = FALSE) {
  .Deprecated(new = "vector_transpose_longer")

  vector_transpose_longer(data_table, names_to, values_to, key_column_name, .keep)
}


#' Transpose a Vector to Wide Form
#'
#' Convert a long-form vector (e.g., indicators, multipliers) into wide form,
#' which is often more useful for binding with input–output tables. This is a
#' thin wrapper around [tidyr::pivot_wider()], provided so you do not need to
#' load **tidyr** explicitly.
#'
#' @inheritParams key_column_create
#' @param data_table A `data.frame` or tibble, normally with a key column.
#'   If the key column must be created or replaced, use `key_column_name` and
#'   `key_column_values`.
#' @param names_from,values_from Columns specifying the names of the output
#'   columns (`names_from`) and the values to fill (`values_from`).
#' @param key_column_values Optional explicit key column values. Default:
#'   `NULL`, in which case values are inferred from the long data.
#'
#' @family iotables processing functions
#'
#' @examples
#' vector_transpose_wider(
#'   data_table = germany_airpol[, -2],
#'   names_from = "induse",
#'   values_from = "value"
#' )
#'
#' vector_transpose_wider(
#'   data_table = germany_airpol[1:8, 3:4],
#'   names_from = "induse",
#'   values_from = "value",
#'   key_column_values = "CO2_emission"
#' )
#' @export

vector_transpose_wider <- function(data_table,
                                   names_from,
                                   values_from,
                                   key_column_name = NULL,
                                   key_column_values = NULL) {
  if (is.null(key_column_name)) key_column_name <- names(data_table)[1]
  
  assertthat::assert_that(names_from %in% names(data_table),
                          msg = glue("in vector_transpose_wider(data_table, names_from='{names_from}') '{names_from}' cannot be found in the data_table")
  )
  
  assertthat::assert_that(values_from %in% names(data_table),
                          msg = glue("in vector_transpose_wider(data_table, values_from='{values_from}') '{values_from}' cannot be found in the data_table")
  )
  
  if (ncol(data_table) >= 2 && is_key_column_present(data_table) && is.null(key_column_values)) {
    pivot_wider(
      data_table,
      names_from = all_of(names_from),
      values_from = all_of(values_from)
    )
  } else if (is_key_column_present(data_table) && !is.null(key_column_values) &&
             names(data_table)[1] != names_from) {
    bind_cols(
      key_column_create(key_column_name, key_column_values),
      pivot_wider(
        data_table %>% select(-1),
        names_from = all_of(names_from),
        values_from = all_of(values_from)
      )
    )
  } else {
    bind_cols(
      key_column_create(key_column_name, key_column_values),
      pivot_wider(
        data_table,
        names_from = all_of(names_from),
        values_from = all_of(values_from)
      )
    )
  }
}


#' @title Create a key columnn
#' @description Create a key column for matching the dimensions of matrixes.
#' @details This function will likely be used with the creation of coefficients that need to be matched with
#' a matrix that has a key column.
#' @param key_column_name The name of the key column.
#' @param key_column_values The value(s) of the key column
#' @return A tibble with one column, named \code{key_column_name} and with values \code{key_column_values}.
#' @importFrom tibble tibble
#' @importFrom rlang set_names
#' @family iotables processing functions
#' @examples
#' key_column_create("iotables_row", c("CO2_multiplier", "CH4_multiplier"))
#' @export
key_column_create <- function(key_column_name,
                              key_column_values = NULL) {
  tibble::tibble(names = as.character(key_column_values)) %>%
    rlang::set_names(key_column_name)
}
