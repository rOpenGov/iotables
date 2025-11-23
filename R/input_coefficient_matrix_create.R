#' @title Create an input coefficient matrix
#'
#' @description Computes the **input (technical) coefficient matrix** from a
#'   symmetric input–output table or use table. Each element expresses the share
#'   of a product’s output used as input in another production activity.
#'
#' @details This is a wrapper around [coefficient_matrix_create()] with `total =
#'   "output"` and `return_part = "products"`. The resulting matrix \(A\)
#'   satisfies \(A_{ij} = z_{ij} / x_j\), where \(z_{ij}\) is intermediate use
#'   of product *i* by industry *j* and \(x_j\) is total output. Terminology
#'   follows the [Eurostat Manual of Supply, Use and Input–Output
#'   Tables](https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF),
#'   where this is also called the *technological coefficients matrix*.
#'
#' @param data_table A symmetric input–output or use table retrieved by
#'   [iotable_get()].
#' @param households Logical; include the household column if available. Default
#'   `FALSE`.
#' @param digits Optional integer precision for rounding the coefficients.
#'   Default `NULL` (no rounding).
#' @references
#' Beutel, J. (2008). *Eurostat Manual of Supply, Use and Input–Output Tables*.
#' Luxembourg: Office for Official Publications of the European Communities,
#' Table 15.6 (page 485). Validation against this reference confirms that
#' `input_coefficient_matrix_create()` reproduces the standard
#' Eurostat input–output coefficients for Germany 1995.
#'
#' United Nations et al. (2009). *System of National Accounts 2008*,
#' §§14.90–14.91.
#'
#' @return A `data.frame` containing the input coefficient matrix (products ×
#'   products), with the key (row label) as the first column. Rows and columns
#'   labelled `"cpa_total"` or `"total"` are removed.
#'
#' @examples
#' cm <- input_coefficient_matrix_create(
#'   iotable_get(source = "germany_1995"),
#'   digits = 4
#' )
#' head(cm)
#'
#' @export
input_coefficient_matrix_create <- function(
    data_table,
    households = FALSE,
    digits = NULL) {
  # --- Inspect original names ---------------------------------------
  original_names <- names(data_table)

  # --- Detect presence of a TOTAL row --------------------------------
  key_column <- as.character(data_table[[1]])

  has_total_row <- any(toupper(key_column) %in% c(
    "TOTAL",
    "CPA_TOTAL",
    "TOTAL OUTPUT"
  ))

  # We always use "output" row as denominator—never TOTAL.
  return_part_arg <- if (has_total_row) "products" else NULL

  # Determine the denominator row name ---------------------
  key <- as.character(data_table[[1]])
  key_upper <- toupper(key) # helper for matching

  if ("OUTPUT" %in% key_upper) {
    denom <- key[key_upper == "OUTPUT"]
  } else if ("P1" %in% key_upper) {
    denom <- key[key_upper == "P1"]
  } else if ("OUTPUT_BP" %in% key_upper) {
    denom <- key[key_upper == "OUTPUT_BP"]
  } else if ("CPA_TOTAL" %in% key_upper) {
    denom <- key[key_upper == "CPA_TOTAL"]
  } else if ("TOTAL" %in% key_upper) {
    denom <- key[key_upper == "TOTAL"]
  } else {
    stop("Could not identify denominator row among: OUTPUT, P1, OUTPUT_BP, CPA_TOTAL, TOTAL.")
  }

  # --- Compute coefficient matrix --------------------------------
  cm <- coefficient_matrix_create(
    data_table   = data_table,
    total        = denom,
    return_part  = return_part_arg,
    households   = households,
    digits       = digits
  )

  # --- Remove TOTAL or CPA_TOTAL rows/columns from cm -------------
  # (They MUST NOT appear in coefficient matrix)
  total_labels <- c("TOTAL", "CPA_TOTAL", "TOTAL OUTPUT")

  # Remove TOTAL rows
  key_cm <- as.character(cm[[1]])
  remove_rows <- which(toupper(key_cm) %in% total_labels)
  if (length(remove_rows) > 0) {
    cm <- cm[-remove_rows, , drop = FALSE]
  }

  # Remove TOTAL columns
  remove_cols <- which(toupper(names(cm)) %in% total_labels)
  if (length(remove_cols) > 0) {
    cm <- cm[, -remove_cols, drop = FALSE]
  }

  # --- Return coefficient matrix with names preserved ------------
  cm
}



#' @keywords internal
input_coefficient_matrix_create_2 <- function(data_table,
                                              households = FALSE,
                                              digits = NULL) {
  # --- Normalize column names (case-insensitive) ------------------
  names(data_table) <- tolower(names(data_table))

  alias_map <- c(
    tfu = "output",
    total = "total",
    cpa_total = "total"
  )
  for (nm in names(alias_map)) {
    if (nm %in% names(data_table) &&
      !(alias_map[[nm]] %in% names(data_table))) {
      names(data_table)[names(data_table) == nm] <- alias_map[[nm]]
    }
  }

  # --- Handle missing total row (toy tables) --------------------
  if (!any(grepl(
    "total|cpa_total|total output",
    tolower(data_table[[1]])
  ))) {
    return_part_arg <- NULL
    warning(
      "No 'total' or 'cpa_total' row found; returning full matrix without subsetting."
    )
  } else {
    return_part_arg <- "products"
  }

  cm <- coefficient_matrix_create(
    data_table = data_table,
    total = "output",
    return_part = return_part_arg,
    households = households,
    digits = digits
  )

  potential_total_names <- c("cpa_total", "total", "TOTAL", "CPA_TOTAL")

  key_column <- as.character(unlist(cm[, 1]))
  remove_col <- which(names(cm) %in% potential_total_names)
  remove_row <- which(key_column %in% potential_total_names)

  if (length(remove_row) > 0) cm <- cm[-remove_row, , drop = FALSE]
  if (length(remove_col) > 0) cm <- cm[, -remove_col, , drop = FALSE]

  cm
}
