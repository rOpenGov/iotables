
#' @keywords internal
fn_na_to_null <- function(x) ifelse(is.na(x), 0, x)

#' @title Validate source parameter
#'
#' @description Internal function that checks whether the given `source`
#'   argument matches one of the supported Eurostat or UK table identifiers.
#'
#' @param source A character string naming the desired source table.
#'
#' @return Invisibly returns the validated source string, otherwise throws an
#'   error if the source is not supported.
#'
#' @keywords internal
validate_source <- function(source) {
  possible_download_sources <- c(
    "naio_10_cp1700", "naio_10_cp1750",
    "naio_10_pyp1700", "naio_10_pyp1750",
    "naio_10_cp15", "naio_10_cp16",
    "naio_10_cp1610", "naio_10_pyp1610",
    "naio_10_cp1620", "naio_10_pyp1620",
    "naio_10_cp1630", "naio_10_pyp1630",
    "uk_2010"
  )
  source <- tolower(source)
  if (!source %in% possible_download_sources) {
    supported_tables <- paste(possible_download_sources, collapse = ", ")
    stop(
      source, " is not in supported tables [", supported_tables, "]"
    )
  }
  invisible(source)
}

#' @title Check if a key column is present
#'
#' @description Tests whether the first column of a data frame contains either
#'   non-numeric values (default) or any of the `potential_keywords` supplied.
#'
#' @param data_table A data frame with a key column in its first position.
#' @param potential_keywords Optional character vector of keywords expected in
#'   the key column. Defaults to `NULL`, in which case the function only asserts
#'   that the first column is not numeric.
#'
#' @return A logical scalar: `TRUE` if the key column is valid, otherwise throws
#'   an error with a descriptive message.
#'
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#'
#' @keywords internal
is_key_column_present <- function(data_table, potential_keywords = NULL) {
  assertthat::assert_that(
    "data.frame" %in% class(data_table),
    msg = "The 'data_table' must be a data.frame."
  )

  if (!is.null(potential_keywords)) {
    msg_potential_keywords <- paste(potential_keywords, collapse = "', '")
    assertthat::assert_that(
      any(potential_keywords %in% data_table[, 1]),
      msg = glue::glue(
        "The data_table has no key column containing any of ",
        "'{msg_potential_keywords}'."
      )
    )
  } else {
    assertthat::assert_that(
      !is.numeric(data_table[, 1]),
      msg = "The data_table has no key column (expected non-numeric first col)."
    )
  }
  TRUE
}

#' @title Collapse character vectors
#'
#' @description A wrapper around [base::paste()] that conditionally collapses a
#'   character vector. If the vector length is greater than one, it is collapsed
#'   using the supplied separator. Used internally to create legible error
#'   messages.
#'
#' @param x A character vector.
#' @param collapse A separator string used if `x` has more than one element.
#'
#' @return A character string of length one if `x` has length > 1, otherwise the
#'   original vector unchanged.
#'
#' @keywords internal
chars_collapse <- function(x, collapse = ", ") {
  if (length(x) > 1) x <- paste(x, collapse = collapse)
  x
}
