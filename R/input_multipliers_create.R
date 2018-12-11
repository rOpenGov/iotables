#' Create input indicators
#' 
#' The function creates the multipliers (direct + indirect effects).
#' @param input_requirements A matrix or vector created by 
#' \code{\link{input_indicator_create}}
#' @param inverse A Leontieff-inverse created by \code{\link{leontieff_inverse_create}}.
#' @param digits Rounding digits, defaults to \code{NULL}, in which case 
#' no rounding takes place.  
#' @importFrom dplyr select one_of mutate_at
#' @examples  
#' nl <- netherlands_2006
#'
#' input_coeff_nl <- input_coefficient_matrix_create(
#'  data_table  = netherlands_2006, 
#'  households = FALSE) 
#'
#' compensation_indicator <- input_indicator_create(netherlands_2006, 'compensation_employees')
#'
#' I_nl <- leontieff_inverse_create( input_coeff_nl )
#'
#' input_multipliers_create(input_requirements = compensation_indicator, 
#'                         inverse = I_nl)
#' @export

input_multipliers_create <- function ( input_requirements,
                                    inverse,
                                    digits = NULL) { 
  . <- NULL
  
  names_direct <- names ( input_requirements )
  new_key_column <- input_requirements %>%
    dplyr::select (1:2) %>%
    dplyr::mutate_at ( vars(1), funs(gsub(pattern ="_indicator",
                                          replacement = "", 
                                          x =. )) ) %>%
    dplyr::mutate_at ( vars(1), funs(paste0(., "_multiplier")))
  
  
  col_n <- ncol(input_requirements)
 
  #columns of the left matrix must be the same as the number of rows of 
  #the right matrix
  #Remove key column------
  key_column                <- subset ( input_requirements, select = 1)
  input_requirements_matrix <- input_requirements[,-1]
  inverse                   <- inverse[, -1]

  inverse                   <- as.matrix ( inverse )
  input_requirements_matrix <- as.matrix ( input_requirements_matrix )
  

  effects <- input_requirements_matrix %*% inverse 
  multipliers <- effects
  
  for ( i in 1:nrow(effects)) {
    multipliers[i, ] <- effects[i, ] /  input_requirements_matrix[i,]
  }
  
  if ( !is.null(digits)) {
    if ( digits>=0 ) 
      multipliers <- round ( multipliers, digits )
   }
  
  cbind (new_key_column[,1], multipliers)
 
}
