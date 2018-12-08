#' Create input multipliers
#' 
#' The function creates the input multipliers.  See Eurostat Manual p500-501..
#' @param direct_effects A direct effects matrix created by 
#' \code{\link{direct_supply_effects_create}}
#' @param inverse A Leontieff-inverse created by \code{\link{leontieff_inverse_create}}.
#' @param digits Rounding digits, defaults to \code{NULL}, in which case 
#' no rounding takes place.  
#' @importFrom dplyr select one_of mutate_at
#' @examples  
#' de_use <- use_table_get()
#' L_de  <- leontieff_matrix_create( de_use )
#' I_de <- leontieff_inverse_create( L_de )
#' 
#' io_table <- iotable_get () 
#' #Total column should not be missing
#' io_table <- io_table [, 1:7] 
#' io_table$total <- rowSums(io_table[, 2:7])
#' 
#' labelled_io_table <- io_table
#' direct_effects_de <- direct_effects_create ( io_table ) 
#' 
#' multipliers <- input_multipliers_create(
#'       direct_effects = direct_effects_de [, -8],
#'       inverse = I_de)
#' @export

input_multipliers_create <- function ( direct_effects,
                                       inverse,
                                       digits = NULL, 
                                       multiplier_names = NULL) { 
 
  
  names_direct <- names ( direct_effects )
  if ( is.null(multiplier_names)) {
    direct_effects <- direct_effects %>%
      dplyr::mutate_at ( vars(1), funs(gsub(pattern ="_indicator",
                                       replacement = "_multiplier", 
                                       x =. )))
  }
  
  if ( all ( names (inverse) %in% names ( direct_effects ) ) ) {
    direct_effects <- dplyr::select ( direct_effects, 
                                      dplyr::one_of (names(inverse)))
  }
  
  col_n <- ncol(direct_effects)
  if ( col_n != ncol(inverse)) {
    stop("The direct effects matrix and the Leontieff inverse must have the same number of columns.")
  }
  
  #columns of the left matrix must be the same as the number of rows of 
  #the right matrix
  #Remove key column------
  first_column   <- subset ( direct_effects, select = 1)
  direct_effects <- direct_effects[,-1]
  inverse        <- inverse[, -1]

  inverse        <- as.matrix ( inverse )
  direct_effects <- as.matrix (direct_effects)
  
  if (ncol ( direct_effects) != nrow ( inverse ) ) {
    stop ( "The matrix is not symmetric") #review later
  }
  
  multipliers <- direct_effects %*% inverse 
  
  multipliers <-  cbind (first_column, as.data.frame(multipliers))
  if ( !is.null(digits) ) {
    if (! class(digits) %in% c("numeric", "integer")) {
      stop("Digits must be a number.")
    }
    multipliers[, 2:ncol(multipliers)] <- round(
      multipliers[, 2:ncol(multipliers)], digits)
  }
  
  as.data.frame ( multipliers )
}
