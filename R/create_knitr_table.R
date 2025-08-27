#' @title Create an output-independent, formatted kable table
#'
#' @description Wrapper around [knitr::kable()] with sensible defaults
#' for alignment, borders, and formatting, compatible with HTML and LaTeX
#' output formats.
#'
#' @param data_table A `data.frame`, tibble, named matrix, or `knitr_kable` object.
#' @param caption Optional table caption. Defaults to `NA` (no caption).
#' @param digits Number of digits to display for numeric variables.
#'   Defaults to `getOption("digits")`.
#' @param col.names Column names for the table. If `NULL`, uses
#'   `names(data_table)`.
#' @param width_unit Character. Unit for column widths (default `"cm"`).
#' @param col_width Numeric vector of column widths. If `NULL`, defaults to
#'   2 for the first column and equal share for the rest.
#' @param col_align Column alignment. Defaults to `"l"` for the first column,
#'   `"c"` for others.
#' @param border_right_cols Logical vector for right-hand column borders.
#'   Defaults to `TRUE` for the first column, `FALSE` otherwise.
#' @param bold_cols Logical vector. If `TRUE`, makes the column bold.
#'   Defaults to all `FALSE`.
#' @param bootstrap_options Passed to [kableExtra::kable_styling()] for
#'   HTML output. Defaults to `c("striped", "hover", "condensed")`.
#' @param latex_options Passed to [kableExtra::kable_styling()] for
#'   LaTeX output. Defaults to `c("scale_down", "striped")`.
#' @param output_format Character: `"html"`, `"latex"`, or `"image"`.
#'   If `NULL`, auto-detects from knitr options.
#' @param keep_pdf Logical. Only relevant for `output_format = "image"`.
#'   Defaults to `FALSE`.
#' @param latex_header_includes Character vector of LaTeX headers to include.
#'   Defaults to `c("\\usepackage[magyar]{babel}", "\\usepackage[utf8]{inputenc}")`.
#'
#' @return A `knitr_kable` object with styling applied.
#' @importFrom knitr kable is_latex_output is_html_output
#' @importFrom kableExtra kable_styling column_spec
#' @keywords internal
#'
#' @examples
#' foo <- data.frame(
#'   observation = c("indicator1", "indicator2", "indicator3"),
#'   indicator_1 = c(100, 105, 95),
#'   indicator_2 = c(102, 104, 76)
#' )
#'
#' create_knitr_table(foo, caption = "Demo table")
create_knitr_table <- function(data_table,
                               digits = NULL,
                               caption = NA,
                               col.names = NULL,
                               col_width = NULL,
                               width_unit = "cm",
                               col_align = NULL,
                               border_right_cols = NULL,
                               bold_cols = NULL,
                               bootstrap_options = c("striped", "hover", "condensed"),
                               latex_options = NULL,
                               output_format = NULL,
                               keep_pdf = FALSE,
                               latex_header_includes = c(
                                 "\\usepackage[magyar]{babel}",
                                 "\\usepackage[utf8]{inputenc}"
                               )) {
  if (is.null(ncol(data_table))) {
    stop("Empty data table inputted.")
  }

  # Auto-detect output format if not set
  if (is_latex_output()) {
    output_format <- "latex"
  } else if (is_html_output()) {
    output_format <- "html"
  }
  if (is.null(output_format) || !output_format %in% c("latex", "html")) {
    output_format <- "image"
  }

  if (!inherits(data_table, "knitr_kable")) {
    # Column names
    if (is.null(col.names)) col.names <- names(data_table)

    # Alignment defaults
    if (is.null(col_align)) col_align <- c("l", rep("c", ncol(data_table) - 1))

    # Borders
    if (is.null(border_right_cols)) {
      border_right_cols <- c(TRUE, rep(FALSE, ncol(data_table) - 1))
    }

    # Boldness
    if (is.null(bold_cols)) bold_cols <- rep(FALSE, ncol(data_table))

    # Column widths
    if (is.null(col_width)) {
      col_width <- c(2, 18 / (ncol(data_table) - 1))
    }
    col_width <- paste0(col_width, width_unit)

    # Digits
    if (is.null(digits) || digits < 0) digits <- getOption("digits")

    # Table format
    table_format <- if (output_format == "image") "latex" else output_format

    knitr_table <- knitr::kable(
      data_table,
      digits = digits,
      format = table_format,
      format.args = list(big.mark = " ", decimal.mark = ","),
      caption = caption,
      booktabs = TRUE,
      col.names = col.names,
      align = col_align
    )
  }

  # Apply styling
  if (table_format == "html") {
    if (is.null(bootstrap_options)) {
      bootstrap_options <- c("striped", "hover", "condensed")
    }
    knitr_table <- kableExtra::kable_styling(
      kable_input = knitr_table,
      bootstrap_options = bootstrap_options
    )
  }

  if (table_format %in% c("latex", "image")) {
    if (is.null(latex_options)) {
      latex_options <- c("scale_down", "striped")
    }
    knitr_table <- kableExtra::kable_styling(
      kable_input = knitr_table,
      latex_options = latex_options
    )
  }

  # Column-specific formatting
  for (i in seq_len(ncol(data_table))) {
    knitr_table <- kableExtra::column_spec(
      kable_input = knitr_table,
      column = i,
      width = col_width[i],
      bold = bold_cols[i],
      border_right = border_right_cols[i]
    )
  }

  knitr_table
}
