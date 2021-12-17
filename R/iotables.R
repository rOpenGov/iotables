#' iotables
#'
#' Pre-processing and basic analytic tasks related to working with 
#' Eurostat's symmetric input-output tables and provide basic 
#' input-output economics calculations. The package is 
#' a part of rOpenGov <http://ropengov.github.io/> for 
#' open source open government initiatives.
#' @section import functions:
#' The iotables import function help downloading and pre-processing the
#' Eurostat symmetric input-output tables and related tables.
#' 
#' \code{\link{iotable_get}} returns a single table.
#' \code{\link{iotables_read_tempdir}} reads data from the temporary directory of a session.
#' \code{\link{employment_get}} downloads the employment data and processes
#' it to a conforming form.
#' 
#' @section iotables processing functions:
#' These are various helper functions for accessing parts of the symmetric
#' input-output tables and joining them correctly.
#' \code{\link{conforming_vector_create}} is a helper function that creates
#' a named vector that conforms with the analytical objects, such as the 
#' use table, the Leontieff-matrix, etc.
#' \code{\link{household_column_get}} returns the final household 
#' expenditure.
#' \code{\link{primary_input_get}} will retrieve any primary input from 
#' the input-output table. \code{\link{output_get}} is a wrapper function around the 
#' \code{\link{primary_input_get}} function.
#' \code{\link{total_tax_add}} adds taxes to an input-output table.
#' \code{\link{empty_remove}} symmetrically removes columns and rows if they contain missing values, 
#' or each and every value is zero. 
#'
#' @section analytic object functions: 
#' \code{\link{input_flow_get}} returns the use (input flow) matrix; 
#' \code{\link{leontieff_matrix_create}} and the 
#' \code{\link{leontieff_inverse_create}} to create the respective analytic
#' matrixes.
#' 
#' @section indicator functions:
#' \code{\link{input_indicator_create}} The function creates the input indicators from the inputs and
#' the outputs. 
#' \code{\link{direct_effects_create}} for direct effects.
#' \code{\link{coefficient_matrix_create}} The coefficient matrix is
#' related by default to output, but you can change this to total supply 
#' or other total aggregate if it exists in the data table.
#' 
#' @section multiplier functions:
#' \code{\link{multiplier_create}} is a wrapper around \code{\link{equation_solve}}
#' to create multipliers. This is a more generic helper function to calculate
#' various multipliers.
#' 
#' \code{\link{input_multipliers_create}} is a function to create input
#' multipliers (for direct and indirect economic effects.)
#' 
#' @section linkage functions:
#' \code{\link{backward_linkages}} creates the vector of industry 
#' (product) backward linkages in a wide
#' data.frame class, following the column names of the Leontieff 
#' inverse matrix. 
#' 
#' \code{\link{forward_linkages}} creates the vector of industry 
#' (product) forward linkages in a 
#' long-form data.frame, containing the metadata column of the the row
#' names from the \code{output_coefficient_matrix}.
#' 
#' @section Metadata datasets:
#' Data files that contain descriptive metadata for a correct reproduction
#' of the symmetric input-output tables.  The analytic functions use 
#' matrix equations that require a precise column and row order for each
#' table.
#' 
#' @section Validation datasets:
#' Data files that replicate published input-output tables with analysis.
#' These files are used to validate the correct working of the analytic
#' functions.
#'
#' @section Croatia data files:
#' These are Croatia's symmetric input-output tables for the year 2010, when 
#' the country was not yet an EU member state.
#' 
#' @docType package
#' @importFrom utils globalVariables 
#' @name iotables
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if (getRversion() >= "2.15.1") {
  utils::globalVariables(".")
  
  # Fix for tidyselect::where()
  # See https://github.com/r-lib/tidyselect/issues/201
  utils::globalVariables("where")
}