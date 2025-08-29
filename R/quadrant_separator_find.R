#' @title Determine the end of Quadrant I and III.
#'
#' @description This is an internal function to determine where to separate quadrants if
#' necessary.
#' @param data_table A symmetric input output table, a use table or a supply
#' table.
#' @param include_total Should the total (intermediary) output column be
#' included \code{TRUE} or excluded (\code{FALSE}, default)?
#' @return An integer value with the last column of Quadrant I and III. If
#' the last column is not found, \code{2} is returned with a warning to avoid
#' stopping a pipeline.
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

  ordered_patterns <- potential_quadrant_2[unlist(lapply(lapply(potential_quadrant_2, \(x) grepl(x, var_names)), any))]

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
