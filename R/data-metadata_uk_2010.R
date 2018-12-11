#' Metadata for the uk_2010 dataset
#'
#' A vocabulary matching and ordering help for the Excel-imported UK data.
#' @usage data(metadata_uk_2010)
#' @format A data frame with 10 variables.
#'\describe{
#'   \item{variable}{Eurostat vocabulary source, i.e. t_rows, t_cols, prod_na, induse}
#'   \item{uk_row}{The UK row identifier. Dots and '&' converted to '-'.} 
#'   \item{uk_row_label}{The original UK row labels.}
#'   \item{uk_col}{The UK row identifier. Dots and '&' converted to '-'.} 
#'   \item{uk_col_label}{The original UK column labels.}
#'   \item{eu_prod_na}{Equivalence to the EU prod_na variable, can be used for grouping. }
#'   \item{row_oder}{Numeric ordeirng of the rows.}
#'   \item{col_order}{Numeric ordering of the columns.}
#'   \item{prod_na}{Eurostat label equivalents for rows}
#'   \item{induse}{Eurostat label equivalents for columns}
#' }
#' @keywords data, datasets, input-output table, metadata, vocabulary, UK

"metadata_uk_2010"