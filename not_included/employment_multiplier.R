#' Create the employment multiplier vector
#' 
#' Create the employment multipliers from the Leontieff-inverse and the employment vector. 
#' @param LI A Leontieff-inverse matrix created by \code{\link{leontieff_inverse_create}} function. 
#' @param employment A character string or a character vector containing the indicator names in the employment satellite account. It defaults to 'employment_total'. 
#' 
#' @examples
#' \dontrun{
#' data (germany_1990)
#' input_flow <- get_input_flow  ( df = germany_1990, technology = "", 
#'                                 year = 1990, geo = "DE", named = FALSE ) 
#' output_vector <- get_output ( germany_1990, geo = 'DE', year = 1990, 
#'                               unit = "M_EUR", named = FALSE)
#' technology_coefficients <- input_coefficient_matrix_create(
#'                            input_flow, output_vector, digits = 4)
#' L <- leontieff_matrix_create( technology_coefficient_matrix = input_coefficients )
#' I <- leontieff_inverse_create (L)   
#'  employment_vector <- employment_get( 
#'                        labelled_io_data = germany_1990, 
#'                        indicator = "employment_total", geo = "DE", 
#'                        year = 1990, named = FALSE ) 
#' Z <- employment_multiplier ( I,employment_vector )   
#' }                    
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename
#' @export

employment_multiplier <- function ( LI = NULL, 
                                    employment = NULL ) {
  time = NULL; t_cols2 = NULL; t_rows2 = NULL; values = NULL #non-standard evaluation creates a varning in build. 
  t(as.vector(employment)) %*% LI

  } #end of function

