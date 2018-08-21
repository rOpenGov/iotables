#' Create an input coefficient matrix
#' 
#' Create an input coefficient matrix from the input flow matrix and the
#' output vector. The two input vectors must have consistent labelling, i.e
#' the same column names must be  found in the use table (input flow) and the
#' output vector.
#'
#' If there are zero values in the output vector, they will be changed to 
#' 0.000001 and you will get a warning. Some analyitcal equations cannot be 
#' solved with zero elements. You either have faulty input data, or you have 
#' to use some sort of data modification to carry on your analysis. 
#' 
#' An alternative that is not implemented here, because it requires analytical
#' judgment, is the aggregation of elements into larger ones that are no longer
#' equal to zero, i.e. merging an industry or product class that has a positive 
#' value with another industry or product class that is zero.
#' 
#' @param input_flow An input flow matrix created with the \code{\link{use_table_get}} function. 
#' @param output An output vector with a key column, created by \code{\link{output_get}}.
#' @param digits An integer showing the precision of the technology matrix in digits. 
#' If not given, no rounding is applied.
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

input_coefficient_matrix_create <- function ( input_flow, 
                                              output, 
                                              digits = NULL) {
  funs <- t_rows2 <-. <- NULL  #for checking against non-standard evaluation

  if ( ! isTRUE(all.equal (names (input_flow), names (output))) ) {
    stop("Non conforming inputs are given with different column labels.")
  }
  input_flow <- dplyr::mutate_if (input_flow, is.factor, as.character )
  
  non_zero <- function (x) {
    if ( class ( x ) %in% c("factor", "character") ) return ( TRUE )
    ifelse (  all ( as.numeric ( unlist (x) ) == 0) , FALSE, TRUE )
  }
  
  non_zero_cols <- vapply ( input_flow[, 1:ncol(input_flow)], non_zero, logical ( 1 ))
  #non_zero_rows <- which ( rowSums( input_flow[, 2:ncol(input_flow)] ) != 0 )
  #input_flow <- input_flow[non_zero_rows, non_zero_cols] #should be improved 
  non_zero_rows <- as.logical (non_zero_cols[-1] ) 

  input_flow <- input_flow [non_zero_rows, non_zero_cols ]
  output <- dplyr::mutate_if ( output, is.factor, as.character )
  output <- output [ names (output) %in% names (input_flow )]
  output  <- dplyr::mutate_if (output, is.factor, as.character )
  Im <- dplyr::full_join ( input_flow, output, by = names ( input_flow ))
  keep_first_name <- names(Im)[1]
  
  output_row <- nrow(Im)
  last_col <- ncol (Im)
  is_last_cols <- FALSE
   
  if ( names (Im)[last_col] %in% c("P3_S14", "households")) {
    last_col <- last_col -1 
    last_name <- names (Im)[last_col+1]
    last_cols <- data.frame (
      total = Im[, which( names ( Im )%in% c("P3_S14", "households") )]
    )
    names (last_cols ) <- last_name 
    is_last_cols <- TRUE
  }
  
  if ( names (Im)[last_col] %in% c("TOTAL", "CPA_TOTAL") ) {
    last_col <- last_col -1  
    total_name <- names (Im)[last_col+1]
    total_col <- data.frame (
      total = Im[, which( names ( Im )%in% c("TOTAL", "CPA_TOTAL") )]
      )
   
    names(total_col) <- total_name 
    if ( is_last_cols ) {
      last_cols <- cbind(total_col, last_cols ) #last col already exists 
    } else {
      is_last_cols <- TRUE
      last_cols <- total_col #this will be the last col
    }
  }
  
  null_to_eps <- function(x) ifelse( x==0, 0.000001, x )
   
  first_col <- as.data.frame( Im[-output_row,1] )
  names (first_col ) <- keep_first_name
  Im <- vapply ( Im[1:output_row-1, c(2:last_col)],
                 null_to_eps, numeric ( output_row - 1) )
  for ( i in 1:(output_row-1)) {
    for ( j in 1:(last_col-1)) {  
      Im[i, j ] <- as.numeric(Im [i, j ]) / as.numeric(output [j+1] )
    }
  #Im's first col is removed, so last_col is last_col-1
  }
  Im <- as.data.frame (Im)

  Im <- cbind ( first_col, Im)
  if ( is_last_cols ) {
    keep_name <- names ( last_cols )
    last_cols <- as.data.frame(last_cols [-output_row, ])
    names ( last_cols ) <- keep_name 
    Im <- cbind ( Im, last_cols )
  } 
  if ( is.null(digits) ) return (Im)
  
  if ( class(digits) != "numeric") stop ("Error: rounding digits are not given as a numeric input.")
  
  if ( digits >= 0 ) {
    
    round_eps <- function ( x, digits ) {
      ifelse ( x == 1e-06, x, round ( x, digits ))
    }
    Im <- Im %>%
      dplyr::mutate_if(is.numeric, dplyr::funs(round_eps (., digits)))
  } else {
    stop ("Error: not a valid rounding parameter.\nMust be an integer representing the rounding digits.")
  }
  Im
}
