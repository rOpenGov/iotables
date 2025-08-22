#' Eurostat National Accounts Vocabulary Metadata
#'
#' A reference dataset containing the Eurostat national accounts vocabulary,
#' used to correctly order wide-format rows and columns when reshaping bulk
#' long-form tables.
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{variable}{Eurostat vocabulary source (e.g., `t_rows`, `t_cols`, `prod_na`, `induse`).}
#'   \item{group}{Informal macroeconomic grouping label.}
#'   \item{code}{Eurostat label codes.}
#'   \item{label}{Eurostat label descriptions.}
#'   \item{quadrant}{Indicates where to place the data from a long-form raw data file.}
#'   \item{account_group}{Grouping of accounts (different from Eurostat tables),
#'     values are in thousands of national currency units.}
#'   \item{numeric_label}{Ordering key derived from `quadrant`, `account_group`,
#'     and digit-based codes.}
#'   \item{iotables_label}{Custom machine-readable snake_case variable names.}
#' }
#'
#' @details This dataset provides a controlled vocabulary and ordering scheme
#' for working with Eurostat input–output and national accounts tables. It is
#' used internally by functions that reshape raw Eurostat data into consistent
#' wide-format representations.
#'
#' @family metadata datasets
"metadata"