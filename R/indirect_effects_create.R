#' @title Create indirect effects
#' 
#' @description The function creates the indirect effects vector.
#' @param input_requirements A matrix or vector created by 
#' \code{\link{input_indicator_create}}
#' @param inverse A Leontief-inverse created by \code{\link{leontief_inverse_create}}.
#' @param digits Rounding digits, defaults to \code{NULL}, in which case 
#' no rounding takes place.  
#' @importFrom dplyr select mutate across
#' @return A data.frame containing the indirect effects and the necessary
#' metadata to sort them or join them with other matrixes.
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
#' indirect_effects_create(input_requirements = compensation_indicator, 
#'                         inverse = I_nl)
#' @export

indirect_effects_create <- function ( input_requirements,
                                      inverse,
                                      digits = NULL) { 
  names_direct <- names(input_requirements)
  col_n <- ncol(input_requirements)
 
  #columns of the left matrix must be the same as the number of rows of 
  #the right matrix
  #Remove key column------
  key_column                <- subset ( input_requirements, select = 1)
  
  new_key_column <- gsub(pattern ="_indicator", replacement = "", key_column[,1])
  new_key_column <- paste0(new_key_column, "_indirect_effect")
  new_key_column <- data.frame( key_colum = as.character(key_column))
  names(new_key_column)[1] <- names(input_requirements)[1]
  
  
  input_requirements_matrix <- input_requirements[,-1]
  inverse                   <- inverse[, -1]

  inverse                   <- as.matrix ( inverse )
  input_requirements_matrix <- as.matrix ( input_requirements_matrix )
  

  effects <- input_requirements_matrix %*% inverse 
  multipliers <- effects
  indirect_effects <- effects
  
  for ( i in seq_len(nrow(effects))) {
    multipliers[i, ] <- effects[i, ] /  input_requirements_matrix[i,]
    indirect_effects[i, ] <- multipliers[i,] - effects[i,]
  }
  
  if ( !is.null(digits)) {
    if ( digits>=0 ) 
      multipliers <- round ( indirect_effects, digits )
   }
  
  cbind (new_key_column, indirect_effects)
 
}
