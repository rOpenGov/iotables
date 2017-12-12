#' Create an input coefficient matrix
#' 
#' Create an input coefficient matrix from the input flow matrix and the output vector.
#' The two input vectors must have consistent labelling, i.e. the same column names must be 
#' found in the use table (input flow) and the output vector.
#' @param input_flow An input flow matrix created with the \code{\link{use_table_get}} function. 
#' @param output An output vector with a key column, created by \code{\link{output_get}}.
#' @param digits An integer showing the precision of the technology matrix in digits. If not given, no rounding is applied.
#' @importFrom tidyr spread
#' @importFrom dplyr mutate mutate_if full_join funs
#' @examples 
#' de_use <- use_table_get ( source = "germany_1990", geo = "DE",
#'                           year = 1990, unit = "MIO_EUR", 
#'                           households = FALSE, labelling = "iotables")
#' 
#' de_output <- output_get ( source = "germany_1990", geo = "DE",
#'                           year = 1990, unit = "MIO_EUR", 
#'                           households = FALSE, labelling = "iotables")
#' @export 

input_coefficient_matrix_create <- function ( input_flow, output, 
                                              digits = NULL) {
  funs <- t_rows2 <-. <- NULL  #for checking against non-standard evaluation

  if ( ! isTRUE(all.equal (names (input_flow), names (output))) ) {
    stop("Non conforming inputs are given with different column labels.")
  }
  Im <- dplyr::full_join ( input_flow, output, by = names ( input_flow ))
  
  output_row <- nrow(Im)
  for (j in 2:ncol(Im)) {
    for (i in 1:(nrow(Im)-1)) {
      if (Im[output_row, j] == 0 ) {
        Im[output_row, j] <- 0.000001 #avoid division by zero
        warning("Warning: There were zero output elements. They were changed to 0.000001.") }
      Im[i,j] <- Im[i,j] / Im[output_row, j]
    }
  } #this is not elegant but works right now.
  
  Im <- Im[1:output_row-1,]
  if ( is.null(digits) ) return (Im)
  if ( class(digits) != "numeric") stop ("Error: rounding digits are not given as a numeric input.")
  if ( digits >= 0 ) {
    Im <- Im %>%
      dplyr::mutate_if(is.numeric, dplyr::funs(round(., digits)))
    return(Im)
  } else {
    stop ("Error: not a valid rounding parameter.\nMust be an integer representing the rounding digits.")
  }
}
