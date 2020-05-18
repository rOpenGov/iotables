#' Create multipliers 
#' 
#' This function is in fact a wrapper around the \code{\link{equation_solve}} function, 
#' adding a key column with the name to the multiplier the maintain structural
#' consistency.
#' 
#' As opposed to direct effects, multipliers are expressed per input of
#' product/industry. 
#' 
#' @param input_vector An input matrix or vector created by the 
#' \code{\link{input_indicator_create}} function. 
#' @param Im The Leontieff inverse as a named object created by the 
#' \code{\link{leontieff_inverse_create}} function. 
#' @param multiplier_name A variable name to be given to the returned multipliers. 
#' Defaults to \code{multiplier}.
#' @param digits Rounding digits, if omitted, no rounding takes place. 
#' @family multiplier functions
#' @return A data frame with the vector of multipliers and the an 
#' auxiliary metadata column (for joining with other matrixes.)
#' @examples 
#' data_table <- iotable_get()
#' 
#' coeff_de <- input_coefficient_matrix_create( data_table )
#' 
#' de_gva_indicator <- input_indicator_create (
#'   data_table = data_table, 
#'   input = 'gva')  #this is a correct input
#' 
#' I_de <- leontieff_inverse_create( coeff_de )
#' 
#' de_gva_multipliers <- multiplier_create ( 
#'   input_vector    = de_gva_indicator,
#'   Im              = I_de,
#'   multiplier_name = "employment_multiplier", 
#'   digits = 4 )
#' 
#' @export 

multiplier_create <- function ( input_vector, 
                                Im,
                                multiplier_name = "multiplier",
                                digits = NULL ) {
  
 if (!is.null(digits)) if (digits < 0) digits <- NULL
  
 multipliers <- equation_solve (LHS = input_vector, 
                                Im = Im ) 
 
 if ( !is.null(digits)) {
       multipliers <- round(multipliers, digits)
  }
 names (multipliers) <- names(Im)[2:ncol(Im)]
 row_name =  as.data.frame(multiplier_name)
 names (row_name)[1] <- names(input_vector)[1]
 
 named_multipliers <- cbind ( row_name, multipliers )
 named_multipliers[,1] <- as.character(named_multipliers[,1])
 
 if ( !is.null(digits)) matrix_round (named_multipliers, digits) else  named_multipliers
 
 }  #end of function  



