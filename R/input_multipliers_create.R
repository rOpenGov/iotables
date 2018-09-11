#' Create input multipliers
#' 
#' The function creates the input multipliers.  See Eurostat Manual p500-501..
#' @param direct_effects A direct effects matrix created by 
#' \code{\link{direct_effects_create}}
#' @param inverse A Leontieff-inverse created by \code{\link{leontieff_inverse_create}}.
#' @param labelled Defaults to \code{TRUE}. If you use not labelled matrixes, i.e. 
#' both inputed matrixes are numerical only without the first key row, select 
#' \code{FALSE}.
#' @param digits Rounding digits, defaults to \code{NULL}, in which case 
#' no rounding takes place.  
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
#' direct_effects_create ( io_table ) 
#' 
#' multipliers <- input_multipliers_create(
#'       direct_effects = direct_effects_de [, -8],
#'       inverse = I_de, 
#'       labelled = TRUE)
#' @export

input_multipliers_create <- function ( direct_effects,
                                       inverse, 
                                       labelled = TRUE,
                                       digits = NULL ) { 
 
  col_n <- ncol(direct_effects)
  #View ( inverse )
  if ( col_n != ncol(inverse)) {
    stop("The direct effects matrix and the Leontieff inverse must have the same number of columns.")
  }
  
  #columns of the left matrix must be the same as the number of rows of the right matrix

  if ( labelled == TRUE) { 
    first_column <- subset ( direct_effects, select = 1)
    direct_effects <- direct_effects[,-1]
    inverse <- inverse[, -1]
  } 
  
  inverse <- as.matrix ( inverse )
  direct_effects <- as.matrix (direct_effects)
  
  if (ncol ( direct_effects) != nrow ( inverse ) ) {
    stop ( "The columns of 'direct_effects' must equal the rows of the 'inverse', 
           they must be both labelled or not labelled.")
  }
  
  multipliers <- direct_effects %*% inverse 
  
 if ( labelled == TRUE ) { 
    multipliers <-  cbind (first_column, as.data.frame(multipliers))
    if ( !is.null(digits) ) {
      multipliers[, 2:ncol(multipliers)] <- round(
        multipliers[, 2:ncol(multipliers)], digits)
    }
    } else {
   if ( !is.null(digits) ) {
     multipliers[, 1:ncol(multipliers)] <- round(
       multipliers[, 1:ncol(multipliers)], digits)
     }
  }
  
  as.data.frame ( multipliers )
}
