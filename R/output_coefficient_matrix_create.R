#' Create an output coefficient matrix
#' 
#' Create an output coefficient matrix from the input flow matrix or a symmetric
#' input-output table. If there are zero values in present, they will be changed to 
#' 0.000001 and you will get a warning. Some analytical equations cannot be 
#' solved with zero elements. You either have faulty input data, or you have 
#' to use some sort of data modification to carry on your analysis. 
#' 
#' @param io_table A symmetric input-output table or use table created with the  
#' \code{\link{iotable_get}} function which contains the 'total' column. 
#' In case you use \code{type="tfu"} you need to input a
#' full iotable, create by the \code{\link{iotable_get}}, because you will need
#' the final demand column.
#' @param total The \code{output='total'} (or CPA_TOTAL, depending on the 
#' names in your table, default) returns the output coefficients
#' for products (intermediates) while the \code{final_demand} returns output 
#' coefficients for final demand. See Eurostat Manual, p495 and p507.
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @importFrom dplyr mutate_if funs
#' @examples 
#' io_table <- iotable_get () 
#' 
#' output_coefficient_matrix_create ( io_table  = io_table, 
#'                                    total = 'tfu',
#'                                    digits = 4 )
#' @export 

output_coefficient_matrix_create <- function ( io_table,
                                               total = "tfu",
                                               digits = NULL ) {
  funs <-. <- NULL  #for checking against non-standard evaluation
  iotables:::check_digits ( digits = digits)
  
  io_table <- dplyr::mutate_if (io_table, is.factor, as.character )
  
  ###Find non-zero cols and rows and remove them---- 
  io_table <- iotables:::empty_remove ( io_table )
  
  total_row <- which ( tolower(as.character(unlist(io_table[, 1])))
                       %in% c("cpa_total", "total") )
  
  if ( length(total_row) == 0 ) stop ("Total row not found") else {
    io_table <- io_table [1:(total_row-1), ]
  }
  
  if ( total == "total" ) { 
    demand_col <- which (tolower(names(io_table)) %in% c("cpa_total", "total") )
    last_column <- quadrant_separator_find ( io_table  )
    if ( length(demand_col) == 0 ) { 
      stop ("Please input a table that has a total column.")
    } #end of finding total column if originally missing
    
  } else if ( tolower(total) %in% c("total_final_use", "tfu", "final_demand")  ) {
    demand_col <- which (tolower(names(io_table)) %in% 
                           c("tfu", "total_final_use") )
    last_column <- iotables:::quadrant_separator_find ( io_table, 
                                                        include_total = FALSE )
   }  else {
      stop ("Paramter 'output' must be either total (CPA_TOTAL) or final_demand.")
    }
  
  demand <- io_table [, demand_col ]
  demand
  io_table <- dplyr::mutate_if ( io_table, is.factor, as.character ) %>%
    dplyr::select ( 1:last_column )
  
  keep_first_name <- names(io_table)[1]  #keep the first name of the table for further use, i.e. prod_na, t_rows, induse

  io_table <- io_table[, 1:last_column ]
  
  ###Create the return data.frame from first column------
  first_col <- as.data.frame( io_table[ ,1] )
  names (first_col) <- keep_first_name
  
  null_to_eps <- function(x) ifelse( x==0, 0.000001, x )
  
  demand <- null_to_eps(as.numeric(unlist(demand)))

  #forward linkeages on p507
  ##Avoid division by zero with epsilon-----
  io_table <- vapply ( io_table[1:nrow(io_table), c(2:last_column)],
                 null_to_eps, numeric (nrow(io_table)) )
  
  output_coeff <- apply (  io_table, 2,
                         function(i)i/demand)

  output_coeff <- as.data.frame (output_coeff)
  output_coeff <- cbind ( first_col, output_coeff)

  if ( is.null(digits) ) return (output_coeff)
  
  if ( digits >= 0 ) {
    round_eps <- function ( x, digits ) {
      ifelse ( x == 1e-06, x, round ( x, digits ))
    }
    output_coeff<- output_coeff %>%
      dplyr::mutate_if(is.numeric, dplyr::funs(round_eps (., digits)))
  } else {
    stop ("Error: not a valid rounding parameter.\nMust be an integer representing the rounding digits.")
  }
  output_coeff
}
