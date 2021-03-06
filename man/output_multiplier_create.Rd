% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_multiplier_create.R
\name{output_multiplier_create}
\alias{output_multiplier_create}
\title{Output multipliers}
\usage{
output_multiplier_create(input_coefficient_matrix)
}
\arguments{
\item{input_coefficient_matrix}{A Leontieff inverse matrix created by the
\code{\link{input_coefficient_matrix_create}} function.}
}
\description{
Output multipliers as defined by the Eurostat Manual of Supply, 
Use and Input-Output Tables on p500.
}
\examples{
                           
de_input_coeff <- input_coefficient_matrix_create( 
                          iotable_get(), digits = 4)
                           
output_multiplier_create ( de_input_coeff )
}
