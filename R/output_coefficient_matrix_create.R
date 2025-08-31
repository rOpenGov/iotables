#' Create an output coefficient matrix
#'
#' Create an output-coefficient matrix from a symmetric input–output table or
#' a use table. Output coefficients can be interpreted as the market shares
#' of products in total output (row-wise normalization).
#'
#' @details
#' Let \eqn{Z} be the inter-industry flow block and \eqn{x} the vector of
#' product output (or, for final-demand shares, total final use).
#' The output-coefficient matrix \eqn{B} is defined row-wise as
#' \eqn{b_{ij} = z_{ij} / x_i}. In practice, zeros in the denominator can make
#' equations unsolvable; this function replaces zeros with a small epsilon
#' (`1e-6`) to avoid division by zero.
#'
#' Eurostat, *Manual of Supply, Use and Input-Output Tables* (e.g., pp. 495,
#' 507) describes output coefficients and the Ghosh framework you may use
#' these with.
#'
#' @param data_table A symmetric input–output table, use table, margins, or
#'   tax table retrieved by [iotable_get()]. If you request
#'   `total = "tfu"` (total final use), you must supply a full table from
#'   [iotable_get()] because the TFU column is in the second quadrant.
#' @param total Which total to use for normalization. Use `"total"` (or the
#'   present table variant name, e.g. `"CPA_TOTAL"`) for output by product, or
#'   `"tfu"` / `"total_final_use"` / `"final_demand"` for total final use.
#'   Default: `"tfu"`.
#' @param digits Integer number of decimal places for rounding. Default `NULL`
#'   (no rounding).
#'
#' @return A `data.frame` whose first column is the key (product labels) and
#'   the remaining columns form the output-coefficient matrix. Column order
#'   follows the input.
#'
#' @importFrom dplyr mutate across
#'
#' @examples
#' data_table <- iotable_get()
#' output_coefficient_matrix_create(
#'   data_table = data_table,
#'   total = "tfu",
#'   digits = 4
#' )
#'
#' @family analytic object functions
#' @export

output_coefficient_matrix_create <- function(data_table,
                                             total = "tfu",
                                             digits = NULL) {
  check_digits(digits = digits)

  data_table <- data_table %>%
    dplyr::mutate(dplyr::across(where(is.factor), function(x) as.character(x)))

  # Remove empty rows/cols
  data_table <- empty_remove(data_table)

  total_row <- which(tolower(as.character(unlist(data_table[, 1])))
  %in% c("cpa_total", "total"))

  if (length(total_row) == 0) {
    stop("Total row not found")
  } else {
    data_table <- data_table[1:(total_row - 1), ]
  }

  if (total == "total") {
    demand_col <- which(tolower(names(data_table)) %in% c("cpa_total", "total"))
    last_column <- quadrant_separator_find(data_table)
    if (length(demand_col) == 0) {
      stop("Please input a table that has a total column.")
    }
  } else if (tolower(total) %in% c("total_final_use", "tfu", "final_demand")) {
    demand_col <- which(tolower(names(data_table)) %in% c("tfu", "total_final_use"))
    last_column <- quadrant_separator_find(data_table, include_total = FALSE)
  } else {
    stop("Paramter 'output' must be any of 'CPA_TOTAL', 'TOTAL', 'final_demand', 'tfu' or 'total_final_use'.")
  }

  demand <- data_table[, demand_col]
  demand # (no-op line retained to avoid logic changes)

  # Keep only the first quadrant (key + inter-industry block) — use base subsetting
  keep_first_name <- names(data_table)[1]
  data_table <- data_table[, 1:last_column, drop = FALSE]

  # Build the first (key) column for the result
  first_col <- as.data.frame(data_table[, 1])
  names(first_col) <- keep_first_name

  null_to_eps <- function(x) ifelse(x == 0, 0.000001, x)

  demand <- null_to_eps(as.numeric(unlist(demand)))

  # Avoid division by zero with epsilon (row-wise denominator)
  data_table <- vapply(
    data_table[seq_len(nrow(data_table)), 2:last_column, drop = FALSE],
    null_to_eps,
    numeric(nrow(data_table))
  )

  output_coeff <- apply(
    data_table, 2,
    function(i) i / demand
  )

  output_coeff <- as.data.frame(output_coeff)
  output_coeff <- cbind(first_col, output_coeff)

  if (is.null(digits)) {
    return(output_coeff)
  }

  if (digits >= 0) {
    round_eps <- function(x, digits) ifelse(x == 1e-06, x, round(x, digits))
    output_coeff <- output_coeff %>%
      dplyr::mutate(dplyr::across(
        where(is.numeric),
        function(x) round_eps(x, digits)
      ))
  } else {
    stop("Error: not a valid rounding parameter.\nMust be an integer representing the rounding digits.")
  }
  output_coeff
}
