#' Create multipliers 
#' 
#' This function is in fact a wrapper around the \code{\link{equation_solve}} function, 
#' adding a key column with the name to the multiplier the maintain structural
#' consistency.
#' @param input_vector An input matrix or vector created by the \code{\link{input_indicator_create}} function. 
#' @param Im The Leontieff inverse as a named object created by the  \code{\link{leontieff_inverse_create}}
#' function. 
#' @param multiplier_name A variable name to be given to the returned multipliers. 
#' Defaults to \code{multiplier}.
#' @param digits Rounding digits, if omitted, no rounding takes place. 
#' @examples 
#' de_use <- use_table_get ( source = "germany_1990", geo = "DE",
#' year = 1990, unit = "MIO_EUR", 
#' households = FALSE, labelling = "iotables")
#' 
#' de_output <- output_get ( source = "germany_1990", geo = "DE",
#'                          year = 1990, unit = "MIO_EUR",
#'                          households = FALSE, labelling = "iotables")
#' 
#' de_emp <- primary_input_get ( input = "employment_total",
#'            source = "germany_1990", geo = "DE",
#'            year = 1990,  
#'            households = FALSE, labelling = "iotables")
#' 
#' de_emp_indicator <- input_indicator_create (de_emp, de_output)
#' 
#' de_coeff <- input_coefficient_matrix_create( de_use,
#'  de_output, digits = 4)
#'
#' L_de <- leontieff_matrix_create( technology_coefficients_matrix =
#'                                    de_coeff )
#' I_de <- leontieff_inverse_create(L_de)
#' 
#' employment_multipliers <- multiplier_create ( 
#'            input_vector    = de_emp_indicator,
#'            Im              = I_de,
#'            multiplier_name =  "employment_multiplier", 
#'            digits = 4 )
#' 
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
 named_multipliers[,1] <- as.character(named_multipliers[,1])
 
 named_multipliers
 }  #end of function  



