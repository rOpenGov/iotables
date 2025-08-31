#' Create a coefficient matrix
#'
#' @description
#' Compute a coefficient matrix from a symmetric input–output table (SIOT),
#' use table, or similar. By default, coefficients are related to industry
#' output, but other totals (if present) can be used as denominator.
#'
#' @details
#' The coefficient matrix \eqn{A} is formed by dividing each row of the
#' inter-industry flows by an output or supply total. By default, the
#' denominator is `"output"` (equivalent to `"P1"` or `"output_bp"`).
#' Alternative totals can be supplied via the `total` argument.
#'
#' Empty rows/columns are removed by default to avoid division by zero,
#' but can be retained with `remove_empty = FALSE`.
#'
#' @param data_table A symmetric input–output table, use table, margins or
#'   tax table retrieved by [iotable_get()].
#' @param total Character. Row label to use as denominator. Defaults to
#'   `"output"`, equivalent to `"P1"` or `"output_bp"`. Other totals such
#'   as `"TS_BP"` may be used if present.
#' @param digits Optional integer. Number of digits for rounding. Default
#'   `NULL` (no rounding).
#' @param return_part Optional. `"products"`, `"industries"`, or
#'   `"primary_inputs"` to select a subset of the matrix. Default `NULL`
#'   returns the full coefficient matrix.
#' @param remove_empty Logical. Defaults to `TRUE`. If `FALSE`, empty
#'   primary-input rows are kept. Empty product/industry rows are always
#'   removed.
#' @param households Logical. If `TRUE`, include household column. Default
#'   `FALSE`.
#'
#' @return A data frame with:
#' - The key column from `data_table`
#' - Numeric columns containing input coefficients
#' Optionally rounded if `digits` is provided.
#'
#' @references
#' Office for National Statistics (UK), *Input–Output Analytical Tables
#' 2010*. [Archived link](https://webarchive.nationalarchives.gov.uk/20160114044923/https://www.ons.gov.uk/ons/rel/input-output/input-output-analytical-tables/2010/index.html)
#'
#' @family indicator functions
#'
#' @examples
#' coefficient_matrix_create(
#'   data_table = iotable_get(source = "germany_1995"),
#'   total = "output",
#'   digits = 4
#' )
#'
#' @importFrom dplyr mutate across left_join where
#' @export


coefficient_matrix_create <- function(data_table,
                                      total = "output",
                                      digits = NULL,
                                      remove_empty = TRUE,
                                      households = FALSE,
                                      return_part = NULL) {
  # Create a coefficient matrix, including primary inputs.
  # For the Leontief matrix, only the inputs part (first quadrant is used)

  if (!is.null(return_part)) {
    if (!return_part %in% c("products", "industry", "primary_inputs")) {
      warning(
        "Parameter return_part='", return_part,
        "' was not recognized, returned all data."
      )
    }
  }

  data_table <- data_table %>%
    mutate(across(where(is.factor), as.character))

  ## Removing all zero columns and rows --------
  if (remove_empty) data_table <- empty_remove(data_table)

  ## See the internal function in the source file quadrant_separator_find.R

  last_column <- quadrant_separator_find(data_table,
    include_total = FALSE
  )

  ## Removing the 2nd and 4th quadrants---
  if (!is.null(households)) {
    if (households == TRUE) {
      household_column <- household_column_get(data_table)
      quadrant <- data_table[, 1:last_column]
      data_table <- dplyr::left_join(quadrant,
        household_column,
        by = names(quadrant)[1]
      )
    } else {
      data_table <- data_table[, 1:last_column]
    }
  } else {
    data_table <- data_table[, 1:last_column]
  }

  key_column <- tolower(as.character(unlist(data_table[, 1])))

  ## Getting the row for division
  if (total %in% c("output", "p1", "output_bp")) {
    if (any(c("output", "p1", "output_bp", "total output") %in% key_column)) {
      total_row <- data_table[which(key_column %in%
        c("output", "p1", "output_bp", "total output"))[1], ]
    } else {
      stop("The output row was not found in the table as 'output',
             'p1' or 'output_bp'")
    }
  } else if (total %in% c("total", "cpa_total")) {
    if (any(c("total", "cpa_total") %in% key_column)) {
      total_row_n <- which(key_column %in% c("total", "cpa_total"))[1]
      total_row <- data_table[total_row_n, ]
    }
  } else {
    total_row <- data_table[which(tolower(key_column) %in% tolower(total)[1]), ]
    if (length(total_row) == 0) stop("The total row was not found.")
  } # end of else


  ## Adjust the total vector --------------------------------------------------------------
  null_to_eps <- function(x) ifelse(x == 0, 0.000001, x)
  total_row <- total_row %>% mutate(across(where(is.factor), as.character))
  total_row <- total_row %>% mutate(across(where(is.factor), null_to_eps)) # avoid division by zero


  ## Make sure that no integers remain in the data table, because they cannot
  ## be divided with numerics.
  coeff_matrix <- data_table %>%
    mutate(across(where(is.numeric), as.numeric))

  if (households == TRUE) last_column <- last_column + 1

  ### The actual creation of the coefficients-----

  for (i in seq_len(nrow(data_table))) {
    coeff_matrix[i, 2:last_column] <- coeff_matrix[i, 2:last_column] / as.numeric(total_row[2:last_column])
  }

  potential_houeshold_earning_names <- c("compensation_employees", "d1")

  earnings_name <- potential_houeshold_earning_names[
    which(potential_houeshold_earning_names %in% key_column)
  ]
  # division by zero
  household_earnings_row <- coeff_matrix[which(earnings_name == key_column), ]

  # If only a part should be returned-----------------------------
  if (!is.null(return_part)) {
    last_row <- which(
      tolower(unlist(data_table[, 1])) %in% c("cpa_total", "total", "total output")
    ) # not last column

    if (return_part == "primary_inputs") {
      coeff_matrix <- coeff_matrix[last_row:nrow(coeff_matrix), ] # households remain anyway
    } else if (return_part %in% c("products", "industries")) {
      coeff_matrix <- coeff_matrix[1:last_row, ]
      if (households == TRUE) { # households re_added if they were removed
        coeff_matrix <- rbind(coeff_matrix, household_earnings_row)
      }
    }
  }

  ### Make rounding if required  --------------------------------------------------------
  if (is.null(digits)) {
    coeff_matrix
  } else {
    round_table(coeff_matrix, digits = digits)
  }
}
