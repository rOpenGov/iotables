#' @title Create a technology (input) coefficient matrix
#'
#' @description
#' Creates an **input (technical) coefficient matrix** from a symmetric
#' input–output table (SIOT) or similar product × product table.  
#' 
#' The function divides every element of the inter-industry use block
#' by an output (or other) total, producing the classical
#' technology matrix \(A\) used for Leontief models.
#'
#' In addition, when `households = TRUE`, the function constructs the
#' **Type-II extension**:  
#' - a *household consumption column* (typically `P3_S14`) rescaled as
#'   coefficients, and  
#' - a *household earnings row* (typically `D1`) identifying labour income.  
#'
#' This produces an augmented coefficient matrix that can be used directly
#' for Type-II multipliers (closed household sector).
#'
#' @details
#' The coefficient matrix is computed as:
#' \deqn{
#' A_{ij} = Z_{ij} / x_j
#' }
#' where \(Z\) is the intermediate use matrix and \(x\) is the chosen
#' denominator row (usually output `P1`).  
#'
#' The denominator row is detected case-insensitively using the vocabulary:
#' `"output"`, `"p1"`, `"output_bp"`, `"total output"`,
#' or `"total"` / `"cpa_total"` depending on the `total` argument.
#'
#' The function supports returning either:
#' - the full matrix (`return_part = NULL`),  
#' - only the technology block (`return_part = "products"` or `"industries"`),  
#' - only the primary inputs (`return_part = "primary_inputs"`).  
#'
#' When `households = TRUE`:
#' - the function identifies the household final-consumption column
#'   (e.g. `P3_S14` or `final_consumption_households`),
#' - divides it by the corresponding output totals,
#' - constructs the appropriate coefficient column,  
#' - includes the household earnings row (e.g. `D1`) for
#'   Type-II multiplier calculations.
#'
#' This behaviour matches traditional Type-II IO modelling practice.
#'
#' @param data_table A symmetric input–output table or any wide-format
#' product × product table returned by [iotable_get()] or built-in datasets.
#' Must contain a key row column (e.g. `prod_na`, `t_rows2`,
#' `uk_row`, or `iotables_row`).
#'
#' @param total Character. Name of the row used for denominator (output).
#' Defaults to `"output"`. Recognised values include:
#' `"output"`, `"P1"`, `"output_bp"`, `"total"`, `"cpa_total"`,
#' and synonyms found in the data.
#'
#' @param digits Integer. Number of digits to round the result to.
#' Defaults to `NULL` (no rounding).
#'
#' @param remove_empty Logical. Remove empty rows/columns (default `TRUE`).
#' Recommended for avoiding division by zero.
#'
#' @param households Logical. When `TRUE`, constructs a Type-II extended
#' coefficient matrix including a household consumption column and
#' household earnings row. Default `FALSE`.
#'
#' @param return_part Optional. One of:
#' - `"products"` or `"industries"` – return only the technology matrix  
#' - `"primary_inputs"` – return only primary input coefficients  
#' - `NULL` – return the full matrix (default)
#'
#' @return
#' A `data.frame` containing:
#' - the key row identifier (first column),  
#' - technology coefficients (columns 2…n),  
#' - and, when `households = TRUE`, the household coefficient column.  
#'
#' If `return_part` is used, only the requested block is returned.
#'
#' @references
#' Beutel, J. (2008). *Eurostat Manual of Supply, Use and Input–Output
#' Tables*, ch. 15. Luxembourg: Publications Office of the European Union.
#'
#' @family indicator functions
#'
#' @examples
#' # Technology matrix (Germany 1995 example)
#' cm <- coefficient_matrix_create(
#'   data_table = iotable_get(source = "germany_1995"),
#'   total = "output",
#'   digits = 4
#' )
#'
#' # Type-II coefficient matrix with household column
#' cm_hh <- coefficient_matrix_create(
#'   data_table = iotable_get(source = "germany_1995"),
#'   households = TRUE
#' )
#'
#' @export


coefficient_matrix_create <- function(data_table,
                                      total = "output",
                                      digits = NULL,
                                      remove_empty = TRUE,
                                      households = FALSE,
                                      return_part = NULL) {
  # Create a coefficient matrix, including primary inputs.
  # For the Leontief matrix, only the inputs part
  # (first quadrant is used)

  if (!is.null(return_part)) {
    if (!return_part %in% c("products", "industry",
                            "primary_inputs")) {
      warning(
        "Parameter return_part='", return_part,
        "' was not recognized, returned all data."
      )
    }
  }

  data_table <- data_table %>%
    mutate(across(where(is.factor), as.character))

  # Removing all zero columns and rows ------------------
  if (remove_empty) data_table <- empty_remove(data_table)

  # See the internal function in the source file
  # quadrant_separator_find.R ---------------------------

  last_column <- quadrant_separator_find(
    data_table,
    include_total = FALSE
  )

  if (length(last_column) != 1) {
    stop("Cannot find the last column of Quadrant 1")
  }

  ## Removing the 2nd and 4th quadrants ---------------------
  if (!is.null(households)) {
    if (households == TRUE) {
      household_column <- household_column_get(data_table)

      quadrant_1 <- data_table[, 1:last_column]
      data_table <- dplyr::left_join(quadrant_1,
        household_column,
        by = names(quadrant_1)[1]
      )
    } else {
      data_table <- data_table[, 1:last_column]
    }
  } else {
    data_table <- data_table[, 1:last_column]
  }

  key_column <- tolower(as.character(unlist(data_table[, 1])))

  # Getting the row for division ------------------------------------
  # Match denominator row case-insensitively but keep original casing
  key_column <- as.character(unlist(data_table[, 1]))
  key_lower <- tolower(key_column)
  total_lower <- tolower(total)

  # main synonyms the function historically supports
  output_synonyms <- c("output", "p1", "output_bp", "total output")
  total_synonyms <- c("total", "cpa_total")

  if (total_lower %in% output_synonyms) {
    idx <- which(key_lower %in% output_synonyms)

    if (length(idx) == 0) {
      stop(
        "The output row was not found in the table under any of: ",
        paste(output_synonyms, collapse = ", ")
      )
    }

    total_row <- data_table[idx[1], ]
  } else if (total_lower %in% total_synonyms) {
    idx <- which(key_lower %in% total_synonyms)

    if (length(idx) == 0) {
      stop(
        "The total row was not found in the table under any of: ",
        paste(total_synonyms, collapse = ", ")
      )
    }

    total_row <- data_table[idx[1], ]
  } else {
    idx <- which(key_lower == total_lower)

    if (length(idx) == 0) {
      stop("The total row '", total, "' was not found.")
    }

    total_row <- data_table[idx[1], ]
  }

  # Adjust the total row
  # to avoid division by zero ------------------------------------
  null_to_eps <- function(x) ifelse(x == 0, 0.000001, x)
  total_row <- total_row %>%
    mutate(across(
      where(is.factor),
      as.character
    ))

  total_row <- total_row %>%
    mutate(across(
      where(is.factor),
      null_to_eps
    )) # avoid division by zero

  # Make sure that no integers remain in the data table,
  # because they cannot be divided with numerics.
  coeff_matrix <- data_table %>%
    mutate(across(where(is.numeric), as.numeric))

  if (households == TRUE) {
    # identify the household column by Eurostat convention
    household_colname <- names(coeff_matrix)[
      grepl("^p3_s14$|consumption_households", names(coeff_matrix), ignore.case = TRUE)
    ]
    
    if (length(household_colname) != 1) {
      stop("Household (P3_S14) column not found, cannot add households = TRUE.")
    }
    
    household_col_index <- which(names(coeff_matrix) == household_colname)
  }

  # The actual creation of the coefficients -------------------------

  for (i in seq_len(nrow(data_table))) {
    coeff_matrix[i, 2:last_column] <- coeff_matrix[i, 2:last_column] /
      as.numeric(total_row[2:last_column])
  }

  
  
  # --- Household column computation ----------------------------------
  if (households == TRUE) {
    
    potential_houeshold_earning_names <- c("compensation_employees", 
                                           "d1", "D1")
    
    earnings_name <- potential_houeshold_earning_names[
      which(potential_houeshold_earning_names %in% key_column)
    ]
    
    if (length(earnings_name) != 1) {
      stop("Could not identify household earnings")
    }
    
    household_earnings_row <- coeff_matrix[
      which(earnings_name == key_column),
    ]
    
    # 1. Create full column
    household_column <- numeric(nrow(coeff_matrix))
    
    # 2. Product rows
    household_column[seq_along(output_row)] <-
      coeff_matrix[seq_along(output_row), household_col_index] / output_row
    
    # 3. CPA_TOTAL row
    household_column[last_row] <- sum(household_column[seq_along(output_row)],
                                      na.rm = TRUE)
    
    # 4. Primary input rows
    household_column[(last_row+1):(nrow(coeff_matrix)-1)] <- 0
    
    # 5. P1 row (denominator row)
    household_column[nrow(coeff_matrix)] <- 0
   
    coeff_matrix[, household_col_index] <- household_column
  }

  # If only a part should be returned ----------------------------
  if (!is.null(return_part)) {
    last_row <- which(
      tolower(unlist(data_table[, 1])) %in% c(
        "cpa_total", "total",
        "total output"
      )
    ) 
    if (return_part == "primary_inputs") {
      coeff_matrix <- coeff_matrix[(last_row + 1):nrow(coeff_matrix), ]
    } else if (return_part %in% c("products", "industries")) {
      coeff_matrix <- coeff_matrix[1:last_row, ]
      }
  }

  # Make rounding if required  --------------------------------------
  if (is.null(digits)) {
    coeff_matrix
  } else {
    round_table(coeff_matrix, digits = digits)
  }
}
