#' Create an input coefficient matrix
#' 
#' Create an output coefficient matrix from the input flow matrix. If there
#' are zero values in the output vector, they will be changed to 
#' 0.000001 and you will get a warning. Some analytical equations cannot be 
#' solved with zero elements. You either have faulty input data, or you have 
#' to use some sort of data modification to carry on your analysis. 
#' 
#' An alternative that is not implemented here, because it requires analytical
#' judgment, is the aggregation of elements into larger ones that are no longer
#' equal to zero, i.e. merging an industry or product class that has a positive 
#' value with another industry or product class that is zero.
#' 
#' @param input_flow An input flow matrix created with the \code{\link{use_table_get}} function. 
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @importFrom dplyr mutate_if funs
#' @examples 
#' de_use <- use_table_get ( source = "germany_1990", geo = "DE",
#'                           year = 1990, unit = "MIO_EUR", 
#'                           households = FALSE, labelling = "iotables")
#'                           
#' output_coefficient_matrix_create ( input_flow = de_use )
#' @export 

output_coefficient_matrix_create <- function ( input_flow, 
                                               digits = NULL ) {
  funs <- t_rows2 <-. <- NULL  #for checking against non-standard evaluation
  
  if (!is.null(digits)) { ##rounding digits must be numeric, if given
    if ( class(digits) != "numeric") {
      stop ("Error: rounding digits are not given as a numeric input.") }
    }
 
  input_flow <- dplyr::mutate_if (input_flow, is.factor, as.character )
  
  non_zero <- function (x) {
    if ( class ( x ) %in% c("factor", "character") ) return ( TRUE )
    ifelse (  all ( as.numeric ( unlist (x) ) == 0) , FALSE, TRUE )
  }
  
  non_zero_cols <- vapply ( input_flow[, 1:ncol(input_flow)], 
                            non_zero, logical (1) )
  non_zero_rows <- as.logical (non_zero_cols[-1] ) 
  
  remove_cols <- names (input_flow )[! non_zero_cols]
  siot_rows <- as.character ( unlist ( input_flow[,1]) )
  siot_rows
  
  input_flow <- input_flow [! siot_rows %in% remove_cols , 
                            ! names ( input_flow ) %in% remove_cols  ]
  input_flow <- dplyr::mutate_if ( input_flow, is.factor, as.character )
  
  
  keep_first_name <- names(input_flow)[1]  #keep the first name of the table for further use, i.e. prod_na, t_rows, induse

  last_col <- ncol (input_flow)
  is_last_cols <- FALSE
  
  ###Households are not needed to calculate the output coefficients------
  households_column <- which (names(input_flow) %in% c("P3_S14", "households") )
  if ( length(households_column) > 0 ) {
    input_flow <- input_flow [, -households_column]
    if (last_col == households_column) last_col <- last_col-1 
  }
  
  
  ###If total column exists, use it, otherwise create it------
  total_col_number <- which (tolower(names(input_flow)) %in% c("total", "cpa_total"))
  if ( length(total_col_number) == 0 ) {
    total <- rowSums(input_flow[,-1], na.rm=TRUE)   #total column is created and stored separately
    total_col_number <- which (tolower(names(input_flow)) %in% c("total", "cpa_total"))
    total_name <- "TOTAL"  #total does not exists so it gets a name 
  } else {
    total <- input_flow [ , total_col_number ] #store total
    total_name <- names(input_flow)[total_col_number]  #store name
    input_flow <- input_flow [ , -total_col_number] #remove total for futher operation
    }

  ###Create the return data.frame from first column------
  first_col <- as.data.frame( input_flow[ ,1] )
  names (first_col) <- keep_first_name
  
  null_to_eps <- function(x) ifelse( x==0, 0.000001, x )

  ##Avoid division by zero with epsilon-----
  input_flow <- vapply ( input_flow[1:nrow(input_flow), c(2:last_col)],
                 null_to_eps, numeric (nrow(input_flow)) )
  
  check <- input_flow[,1]/total
  
  input_flow <- apply (  input_flow, 2,
                         function(x)x/total)

  input_flow <- as.data.frame (input_flow)
  input_flow <- cbind ( first_col, input_flow)

  if ( is.null(digits) ) return (input_flow)
  
  if ( digits >= 0 ) {
    round_eps <- function ( x, digits ) {
      ifelse ( x == 1e-06, x, round ( x, digits ))
    }
    input_flow <- input_flow %>%
      dplyr::mutate_if(is.numeric, dplyr::funs(round_eps (., digits)))
  } else {
    stop ("Error: not a valid rounding parameter.\nMust be an integer representing the rounding digits.")
  }
  input_flow
}
