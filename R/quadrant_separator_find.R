#' @title Locate the end of the inter-industry block (Quadrant I)
#'
#' @description
#' Detects the **column index** of the last inter-industry (Quadrant I) column
#' in a wide symmetric input–output (SIOT) or use table. This separator is used
#' to split the inter-industry block from **final uses** (Quadrant III) and other
#' columns before computing coefficients and inverses.
#'
#' @details
#' The function applies a small set of **case-insensitive heuristics** over the
#' column names (first column is assumed to be the key/label column):
#'
#' 1. If a column named `"total"` or `"cpa_total"` exists, its index is used
#'    as the boundary. If `include_total = FALSE` (default), the **column
#'    before** total is returned.
#' 2. Otherwise, if a column named `"households"` or `"p13_s14"` exists,
#'    the separator is placed **immediately before** that column.
#' 3. Otherwise, if a column named one of `"cpa_u"`, `"cpa_t"`, or `"cpa_s96"`
#'    exists, that column index is used (these are common last CPA aggregates).
#' 4. Otherwise, if `"other_services_group"` exists, its index is used.
#'
#' If none of the above markers are found, the function returns **2** and emits
#' a warning. Returning 2 (i.e. first numeric column only) avoids aborting
#' downstream pipelines but indicates that column metadata should be checked.
#'
#' Notes:
#' - Matching is **case-insensitive**.
#' - If **multiple** potential columns match (e.g. both `"total"` and
#'   `"cpa_total"`), all matching indices are returned. Callers should ensure
#'   uniqueness upstream if a single index is required.
#' - Set `include_total = TRUE` when you explicitly want the index of the total
#'   column itself (e.g. for consistency checks).
#'
#' @param data_table A wide SIOT/use table where the first column is a key
#'   (product/industry label) and subsequent columns are numeric flows.
#' @param include_total Logical. If `TRUE`, and a `"total"`/`"cpa_total"` column
#'   is present, return its **own** index; if `FALSE` (default), return the
#'   **last inter-industry** column (i.e. one before total).
#'
#' @return An **integer** column index (or an integer vector if multiple matches
#'   exist). If no marker is found, returns `2L` with a warning.
#'
#' @examples
#' # Minimal SIOT (wide):
#' small_io <- data.frame(
#'   prod_na = c("CPA_A", "CPA_B", "CPA_C", "output"),
#'   CPA_A   = c(10, 4, 1, 35),
#'   CPA_B   = c(2, 8, 5, 30),
#'   CPA_C   = c(3, 2, 6, 30),
#'   total   = c(20, 16, 18, NA_real_)
#' )
#'
#' # End of Quadrant I (before 'total'):
#' quadrant_separator_find(small_io)
#' # include_total = TRUE returns the index of the 'total' column:
#' quadrant_separator_find(small_io, include_total = TRUE)
#'
#' @seealso \code{\link{coefficient_matrix_create}},
#'   \code{\link{input_coefficient_matrix_create}}
#' @keywords internal

quadrant_separator_find <- function(data_table,
                                    include_total = FALSE) {
  last_column <- 2
  if (any(c("total", "cpa_total") %in% tolower(names(data_table)))) {
    last_column <- which(tolower(names(data_table)) %in% c("total", "cpa_total"))
    if (!include_total) last_column <- last_column - 1 # if total columns are not needed, the last but total
  } else if (any(c("households", "p13_s14") %in% tolower(names(data_table)))) {
    last_column <- which(tolower(names(data_table)) %in% c("households", "p13_s14")) - 1
  } else if ("cpa_u" %in% tolower(names(data_table))) {
    last_column <- which(tolower(names(data_table)) == "cpa_u")
  } else if ("cpa_t" %in% tolower(names(data_table))) {
    last_column <- which(tolower(names(data_table)) == "cpa_t")
  } else if ("cpa_s96" %in% tolower(names(data_table))) {
    last_column <- which(tolower(names(data_table)) == "cpa_s96")
  } else if ("other_services_group" %in% tolower(names(data_table))) {
    last_column <- which(tolower(names(data_table)) == "other_services_group")
  }

  if (last_column == 2) {
    warning("The last column was not found")
  }
  last_column
}


#' @keywords internal
quadrant_separator_find_2 <- function(data_table, col_name = NULL) {
  if (!is.null(col_name)) {

  }
  var_names <- tolower(names(data_table))

  potential_total_columns <- c("cpa_total", "total", "cpa_tot")
  potential_quadrant_2 <- c("^p3", "tfu")
  potential_last_columns <- c("cpa_s96", "cpa_o-t", "cpa_u", "cpa_t")

  total_n <- which(var_names %in% potential_total_columns)
  next_n <- NULL
  prev_n <- max(which(var_names %in% potential_last_columns), na.rm = TRUE)

  ordered_patterns <- potential_quadrant_2[
    unlist(
      lapply(
        lapply(
          potential_quadrant_2, function(x) grepl(x, var_names)
        ), any
      )
    )
  ]

  if (length(ordered_patterns) > 0 && !is.na(ordered_patterns)[1]) {
    next_n <- min(which(grepl(ordered_patterns[1], var_names)), na.rm = TRUE)
  }

  if (length(total_n) == 1 && length(next_n) == 1) {
    if (total_n + 1 == next_n) {
      return(total_n - 1)
    }
  }

  if (length(total_n) == 1 && length(prev_n) == 1) {
    if (prev_n + 1 == total_n) {
      return(prev_n)
    }
  }

  if (length(prev_n) == 1 && length(next_n) == 1) {
    return(prev_n)
  }

  warning("Quadrant separator not found")

  return(NULL)
}
