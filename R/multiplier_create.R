#' Solve a basic equation
#' 
#' This function is in fact a wrapper around the equation_solve () function, adding a name to the multiplier.
#' @param input_vector An input matrix or vector created by the input_indicator_create () function. 
#' @param Im The Leontieff inverse as a named object created by the leontieff_inverse_create () function. 
#' @param multiplier_name A variable name to be given to the returned multipliers. Defaults to 'multiplier'.
#' @param digits Rounding digits, if omitted, no rounding takes place. #' @examples 
#' data (germany_1990)
#' get_input_flow(df = germany_1990, geo = 'DE', year = 1990,
#'                unit = "M_EUR", named = FALSE)
#' @export 

multiplier_create <- function ( input_vector = NULL, 
                                Im = NULL,
                                multiplier_name = "multiplier",
                                digits = NULL) {
  
 if (!is.null(digits)) if (digits < 0) digits <- NULL
  
 multipliers <- equation_solve (input_vector, Im )

 if ( !is.null(digits)) {
       multipliers <- round(multipliers, digits)
  }
 names (multipliers) <- names(Im)[2:ncol(Im)]
 row_name =  as.data.frame(multiplier_name)
 names (row_name)[1] <- names(input_vector)[1]
 
 named_multipliers <- cbind ( row_name, multipliers )
 
 return(named_multipliers)
 }  #end of function  



