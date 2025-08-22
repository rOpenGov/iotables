#' Aggregation Table for Croatian Employment Statistics
#'
#' A mapping table to aggregate detailed Croatian employment statistics into
#' the Croatian (EU-standard) symmetric input–output table (SIOT) format.
#'
#' @format A data frame with 105 rows (including empty rows) and 2 variables:
#' \describe{
#'   \item{employment_label}{Labels from the DZS (Croatian Bureau of Statistics)
#'     English-language export.}
#'   \item{t_cols2}{Labels used in EU/DZS symmetric input–output tables
#'     (SIOTs).}
#' }
#'
#' @details
#' This dataset provides a concordance between Croatian employment
#' classifications and the EU/DZS SIOT framework, enabling consistent
#' integration of employment data into input–output analysis.
#'
#' @family Croatia 2010 datasets
"croatia_employment_aggregation"
