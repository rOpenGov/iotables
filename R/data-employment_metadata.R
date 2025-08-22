#' Employment Metadata
#'
#' A reference dataset linking Eurostat national accounts vocabulary with
#' employment statistics data.
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{emp_code}{Codes used in the employment statistics.}
#'   \item{code}{Eurostat labels for SIOTs corresponding to `emp_code`.}
#'   \item{label}{Eurostat label descriptions for SIOTs corresponding to
#'     `emp_code`.}
#'   \item{variable}{Eurostat vocabulary source (e.g., `t_rows`, `t_cols`,
#'     `prod_na`, `induse`).}
#'   \item{group}{Grouping of accounts (different from Eurostat tables), in
#'     thousands of national currency units.}
#'   \item{iotables_label}{Custom machine-readable snake_case variable names.}
#' }
#'
#' @details
#' This dataset provides a mapping between employment statistics codes and the
#' vocabulary used in Eurostat input–output tables, ensuring compatibility when
#' joining employment and national accounts data.
#'
#' @family metadata datasets
"employment_metadata"

