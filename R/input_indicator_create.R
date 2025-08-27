#' Create input indicator(s)
#'
#' Compute input indicators (e.g., GVA, compensation of employees) by selecting
#' specific input rows from the input-coefficient matrix.
#'
#' @details
#' Let \eqn{A} be the input-coefficient matrix (rows are inputs, columns are
#' products/industries). An *input indicator* for a given input row \eqn{r} is
#' simply the row \eqn{A_{r\cdot}}. These indicators are used in Beutel (2012)
#' and the Eurostat *Manual of Supply, Use and Input-Output Tables* (e.g.,
#' pp. 495–498) to derive effects and multipliers.
#'
#' Internally, the function builds \eqn{A} via
#' [coefficient_matrix_create()], then keeps only the requested input
#' rows and renames the key column to `*_indicator`. Optional rounding is
#' applied to numeric columns.
#'
#' @param data_table A symmetric input–output table, use table, margins, or
#'   tax table retrieved by [iotable_get()].
#' @param input_row Character vector of input row names to extract (e.g.,
#'   `"gva"`, `"compensation_employees"`). Matching is case-insensitive.
#' @param digits Integer number of decimal places for rounding. Default
#'   `NULL` (no rounding).
#' @param households Logical; include a households column if available.
#'   Default `FALSE`.
#' @param indicator_names Optional character vector of names for the returned
#'   indicators. If `NULL`, names are taken from the key column in the selected
#'   rows of the coefficient matrix and suffixed with `"_indicator"`.
#'
#' @return A `data.frame` whose first column is a key, followed by the selected
#'   input-indicator rows as numeric columns.
#'
#' @family indicator functions
#'
#' @examples
#' input_indicator_create(
#'   data_table = iotable_get(),
#'   input_row = c("gva", "compensation_employees"),
#'   digits = 4,
#'   indicator_names = c("GVA indicator", "Income indicator")
#' )
#'
#' # Beutel/Eurostat example: GVA indicator (cf. Manual, ~p. 498)
#' ii <- input_indicator_create(
#'   data_table = iotable_get(),
#'   input_row = "gva",
#'   digits = 4
#' )
#' head(ii)
#'
#' @export
input_indicator_create <- function(data_table,
                                   input_row = c("gva_bp", 
                                                 "net_tax_production"),
                                   digits = NULL,
                                   households = FALSE,
                                   indicator_names = NULL) {
  data_table <- data_table %>%
    mutate(across(where(is.factor), as.character))

  cm <- coefficient_matrix_create(
    data_table = data_table,
    households = households
  )

  key_column <- tolower(as.character(unlist(cm[, 1])))

  inputs_present <- which(key_column %in% tolower(input_row))

  if (length(inputs_present) == 0) {
    stop("The inputs were not found")
  } else if (length(inputs_present) < length(input_row)) {
    not_found <- chars_collapse(input_row[!input_row %in% key_column[inputs_present]])
    input_msg <- chars_collapse(input_row)
    warning(glue::glue("In input_indicator_create(data_table, input_row = {input_msg}) the rows {not_found} were not found in the data_table."))
  }

  input_matrix <- cm[inputs_present, ]

  final_names <- NULL

  if (!is.null(indicator_names)) { 
    # adding custom names, if inputed
    if (length(indicator_names) == nrow(input_matrix)) {
      final_names <- indicator_names
    } else {
      warning("The number of new indicator names is different from indicators,
                default names are used")
    }
  }

  if (is.null(final_names)) { # creating default names
    final_names <- paste0(as.character(unlist(input_matrix[, 1])), 
                          "_indicator")
  }

  input_matrix[, 1] <- final_names

  if (!is.null(digits)) matrix_round(input_matrix, digits) else input_matrix
}
