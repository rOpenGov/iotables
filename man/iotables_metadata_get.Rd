% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iotables_metadata_get.R
\name{iotables_metadata_get}
\alias{iotables_metadata_get}
\title{Get Metadata from Nested iotables File}
\usage{
iotables_metadata_get(dat = NULL, source = "naio_10_cp1700")
}
\arguments{
\item{dat}{A nested data file created by \code{\link{iotables_download}}.
Defaults to \code{NULL} in which case an attempt is made to find and read
in the nested data from the current R sessions' temporary directory.}

\item{source}{See the available list of sources above in the Description.}
}
\value{
A data frame, which contains the metadata of all available 
input-output tables from a specific \code{source}.
}
\description{
Remove the data column and return only the metadata information of 
input-output (or related tables) from a source.
If \code{dat} is not inputed as a nested data frame created by 
\code{\link{iotables_download}}, validate the \code{source} input 
parameter and try to load the table from the current sessions' 
temporary directory.
 \describe{
 \item{\code{naio_10_cp1700}}{ Symmetric input-output table at basic prices (product by product)}
 \item{\code{naio_10_pyp1700}}{ Symmetric input-output table at basic prices (product by product) (previous years prices)}
 \item{\code{naio_10_cp1750}}{ Symmetric input-output table at basic prices (industry by industry)}
 \item{\code{naio_10_pyp1750}}{ Symmetric input-output table at basic prices (industry by industry) (previous years prices) }
 \item{\code{naio_10_cp15}}{ Supply table at basic prices incl. transformation into purchasers' prices }
 \item{\code{naio_10_cp16}}{ Use table at purchasers' prices }
 \item{\code{naio_10_cp1610}}{ Use table at basic prices }
 \item{\code{naio_10_pyp1610}}{ Use table at basic prices (previous years prices) (naio_10_pyp1610) }
 \item{\code{naio_10_cp1620}}{ Table of trade and transport margins at basic prices}
 \item{\code{naio_10_pyp1620}}{ Table of trade and transport margins at previous years' prices}
 \item{\code{naio_10_cp1630}}{ Table of taxes less subsidies on products at basic prices}
 \item{\code{naio_10_pyp1630}}{ Table of taxes less subsidies on products at previous years' prices}
 \item{\code{uk_2010_siot}}{ United Kingdom Input-Output Analytical Tables data}
}
}
\examples{
\donttest{
# The table must be present in the sessions' temporary directory:
iotables_download(source = "naio_10_pyp1750")

# Now you can get the metadata:
iotables_metadata_get(source = "naio_10_pyp1750")
} 
}
\seealso{
Other import functions: 
\code{\link{airpol_get}()},
\code{\link{employment_get}()},
\code{\link{iotables_download}()},
\code{\link{iotables_read_tempdir}()}
}
\concept{import functions}
