#' @title Create input multipliers
#' 
#' @description The function creates the multipliers (direct + indirect effects).
#' @param input_requirements A matrix or vector created by 
#' \code{\link{input_indicator_create}}
#' @param Im A Leontief-inverse created by \code{\link{leontief_inverse_create}}.
#' @param multiplier_name An optional name to be placed in the key column of the multiplier.
#' Defaults to \code{NULL}.
#' @param digits Rounding digits, defaults to \code{NULL}, in which case 
#' no rounding takes place. Rounding is important if you replicate examples from the literature,
#' rounding differences can add up to visible differences in matrix equations.
#' @importFrom dplyr select mutate across
#' @importFrom rlang .data
#' @return A data frame with the vector of multipliers and the an 
#' auxiliary metadata column, containing an automatically given row identifier (for joining with other matrixes) 
#' which can be  overruled with setting \code{multiplier_name}.  
#' @family multiplier functions
#' @examples  
#' nl <- netherlands_2006
#'
#' input_coeff_nl <- input_coefficient_matrix_create(
#'  data_table  = netherlands_2006, 
#'  households = FALSE) 
#'
#' compensation_indicator <- input_indicator_create(netherlands_2006, 'compensation_employees')
#'
#' I_nl <- leontief_inverse_create(input_coeff_nl)
#'
#' input_multipliers_create(input_requirements = compensation_indicator, 
#'                         Im = I_nl)
#' @export

input_multipliers_create <- function ( input_requirements,
                                       Im,
                                       multiplier_name = NULL,
                                       digits = NULL) { 

  names_direct <- names (input_requirements)
  
  
  if (is.null(multiplier_name)) {
    multiplier_name <- gsub("_coefficients|_coefficient|_effect", "_multiplier", input_requirements[1])
  }
  
  
  new_key_column <- key_column_create(names(Im)[1], multiplier_name)
  
  
  col_n <- ncol(input_requirements)
 
  #columns of the left matrix must be the same as the number of rows of 
  #the right matrix
  #Remove key column------
  key_column                <- subset ( input_requirements, select = 1)
  input_requirements_matrix <- input_requirements[,-1]
  inverse                   <- Im[, -1]

  inverse                   <- as.matrix ( inverse )
  input_requirements_matrix <- as.matrix ( input_requirements_matrix )
  

  effects <- input_requirements_matrix %*% inverse 
  multipliers <- effects
  
  for ( i in seq_len(nrow(effects))) {
    multipliers[i, ] <- effects[i, ] /  input_requirements_matrix[i,]
  }
  
  ## Rounding is important when you compare with peer-reviewed literature sources. 
  ## You want the same rounding to be able to replicate the results.
  
  if ( !is.null(digits) ) {
    if ( digits>=0 ) 
      multipliers <- round ( multipliers, digits )
   }
  
  cbind (new_key_column, multipliers)
 
}
