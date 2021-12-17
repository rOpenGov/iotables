#' @title Read input-output tables from temporary directory
#'
#' Validate the \code{source} input parameter and try to load the table
#' from the current sessions' temporary directory.
#'  \itemize{
#'  \item{\code{naio_10_cp1700}}{Symmetric input-output table at basic prices (product by product)}
#'  \item{\code{naio_10_pyp1700}}{Symmetric input-output table at basic prices (product by product) (previous years prices)}
#'  \item{\code{naio_10_cp1750}}{Symmetric input-output table at basic prices (industry by industry)}
#'  \item{\code{naio_10_pyp1750}}{Symmetric input-output table at basic prices (industry by industry) (previous years prices)}
#'  \item{\code{naio_10_cp15}}{Supply table at basic prices incl. transformation into purchasers' prices}
#'  \item{\code{naio_10_cp16}}{Use table at purchasers' prices}
#'  \item{\code{naio_10_cp1610}}{Use table at basic prices}
#'  \item{\code{naio_10_pyp1610}}{Use table at basic prices (previous years prices) (naio_10_pyp1610)}
#'  \item{\code{naio_10_cp1620}}{Table of trade and transport margins at basic prices}
#'  \item{\code{naio_10_pyp1620}}{Table of trade and transport margins at previous years' prices}
#'  \item{\code{naio_10_cp1630}}{Table of taxes less subsidies on products at basic prices}
#'  \item{\code{naio_10_pyp1630}}{Table of taxes less subsidies on products at previous years' prices}
#'  \item{\code{uk_2010_siot}}{United Kingdom Input-Output Analytical Tables data}
#' } 
#' @param source See the available list of sources above in the Description.
#' Defaults to  \code{source = "naio_10_cp1700"}.
#' @return A nested data frame. Each input-output table is in a separate 
#' row of the nested output, where all the metadata are in columns, and the
#' actual, tidy, ordered input-output table is in the data \code{data} column.
#' @family import functions
#' @examples
#' \donttest{
#' # The table must be present in the sessions' temporary directory:
#' iotables_download(source = "naio_10_cp1700")
#' 
#' iotables_read_tempdir (source = "naio_10_cp1700")
#' }
#' @export

iotables_read_tempdir <- function( source = "naio_10_cp1700" ) {
  
  validate_source(source)
  temporary_file <- file.path(tempdir(), paste0(source, '.rds'))
  
  if (file.exists(temporary_file)) {
    readRDS(temporary_file)
  }
}

                   