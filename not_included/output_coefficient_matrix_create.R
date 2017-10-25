#' Create an output coefficient matrix
#' 
#' @param input_flow An input flow matrix created with \code{\link{input_flow_get}}
#' @param output An output vector with a key column. It can be created with \code{\link{output_get}}.
#' @param digits An integer showing the precision of the technology matrix in digits. If not given, no rounding is applied.
#' @importFrom tidyr spread
#' @importFrom dplyr mutate mutate_if left_join funs select
#' @examples 
#' data (germany_1990)
#' input_flow <- input_flow_get  ( labelled_io_data = germany_1990, 
#'                                 technology = NULL, year = 1990, 
#'                                 geo = "DE",  unit = "M_EUR", named = TRUE ) 
#' output_vector <- output_get ( germany_1990, geo = 'DE', year = 1990, 
#'                               unit = "M_EUR", named = TRUE)
#' technology_coefficients <- output_coefficient_matrix_create(
#'                            input_flow, output_vector, digits = 4)
#' @export 

output_coefficient_matrix_create <- function ( input_flow, 
                                               output, digits = NULL) {
  . = NULL ;  t_rows2 = NULL; funs = NULL #for checking against non-standard evaluation
  
  
  Om <- dplyr::left_join( input_flow, output, by = "t_rows2") %>%
    dplyr::mutate_if(is.numeric, dplyr::funs( . / output )) 
  
  if ( is.null(digits) ) return(Om)
  if ( class(digits) != "numeric") stop ("Error: rounding digits are not given as a numeric input.")
  if ( digits >= 0 ) {
    Om <- Om %>%
      dplyr::mutate_if(is.numeric, dplyr::funs(round(., digits))) %>%
      dplyr::select(-output)
    return(Om)
  } else {
    stop ("Error: not a valid rounding parameter.\nMust be an integer representing the rounding digits.")
  }
}
