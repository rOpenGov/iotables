#' Get Metadata from Nested iotables File
#'
#' Remove the data column and return only the metadata information of 
#' input-output (or related tables) from a source.
#' If \code{dat} is not inputed as a nested data frame created by 
#' \code{\link{iotables_download}}, validate the \code{source} input 
#' parameter and try to load the table from the current sessions' 
#' temporary directory.
#'  \itemize{
#'  \item{\code{naio_10_cp1700}}{ Symmetric input-output table at basic prices (product by product)}
#'  \item{\code{naio_10_pyp1700}}{ Symmetric input-output table at basic prices (product by product) (previous years prices)}
#'  \item{\code{naio_10_cp1750}}{ Symmetric input-output table at basic prices (industry by industry)}
#'  \item{\code{naio_10_pyp1750}}{ Symmetric input-output table at basic prices (industry by industry) (previous years prices) }
#'  \item{\code{naio_10_cp15}}{ Supply table at basic prices incl. transformation into purchasers' prices }
#'  \item{\code{naio_10_cp16}}{ Use table at purchasers' prices }
#'  \item{\code{naio_10_cp1610}}{ Use table at basic prices }
#'  \item{\code{naio_10_pyp1610}}{ Use table at basic prices (previous years prices) (naio_10_pyp1610) }
#'  \item{\code{naio_10_cp1620}}{ Table of trade and transport margins at basic prices}
#'  \item{\code{naio_10_pyp1620}}{ Table of trade and transport margins at previous years' prices}
#'  \item{\code{naio_10_cp1630}}{ Table of taxes less subsidies on products at basic prices}
#'  \item{\code{naio_10_pyp1630}}{Table of taxes less subsidies on products at previous years' prices}
#'  \item{\code{uk_2010_siot}}{United Kingdom Input-Output Analytcal Tables data}
#' } 
#' @param dat A nested data file created by \code{\link{iotables_download}}.
#' Defaults to \code{NULL} in which case an attempt is made to find and read
#' in the nested data from the current R sessions' temporary directory. 
#' @param source See the available list of sources above in the Description. 
#' @return A data frame, which contains the metadata of all available 
#' input-output tables from a specific \code{source}.
#' @importFrom tidyr unnest
#' @examples
#' iotables_metadata_get ( source = "naio_10_cp1700" )
#' @export

iotables_metadata_get <- function (dat = NULL, 
                          source = "naio_10_cp1700" ) {
  if ( is.null(dat)) {
    validate_source(source)
    dat <- iotables_read_tempdir(source)
  }
  
  if ( !is.null(dat)) {
    metadata <- dat[, !names(dat) %in% c("data")]
    tidyr::unnest(metadata, cols = c())
  } else {
    message( "The temporary file for source='",source, 
             "' is not found in\ntempdir='", 
             tempdir(), "'")
    }
}
