#' Create an output coefficient matrix
#' 
#' Create an output coefficient matrix from the input flow matrix or a symmetric
#' input-output table. If there are zero values in present, they will be changed to 
#' 0.000001 and you will get a warning. Some analytical equations cannot be 
#' solved with zero elements. You either have faulty input data, or you have 
#' to use some sort of data modification to carry on your analysis. 
#' 
#' @param io_table An input flow matrix created with the 
#' \code{\link{use_table_get}} function which contains the 'total' column 
#' is sufficient if type=\code{products} is used. 
#' In case you use \code{type="final_demand"} you need to input a
#' full iotable, create by the \code{\link{iotable_get}}, because you will need
#' the final demand column.
#' @param type The type=\code{products} (default) returns the output coefficients
#' for products (intermediates) while the \code{final_demand} returns output 
#' coefficients for final demand. See Eurostat Manual, p495 and p507.
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @importFrom dplyr mutate_if funs
#' @examples 
#' #You need a table that has a total column and either the total 
#' #intermediate use or final use
#' #This is usually the case with Eurostat tables, but with the Germany data
#' #file total must be added.
#' 
#' io_table <- iotable_get () 
#' io_table <- io_table [1:which(tolower(io_table[,1]) =="total" ), ]

#' output_bp <- dplyr::select ( io_table, output_bp )
#' io_table <- io_table [, 1:7] 
#' io_table$total <- rowSums(io_table[, 2:7])
#' io_table <- cbind (io_table, output_bp)
#' 
#' output_coefficient_matrix_create ( io_table = io_table, 
#'                                     type = 'final_demand',
#'                                     digits = 4 )
#' @export 

output_coefficient_matrix_create <- function ( io_table,
                                               type = "product",
                                               digits = NULL ) {
  funs <- t_rows2 <-. <- NULL  #for checking against non-standard evaluation
  
  if (!is.null(digits)) { ##rounding digits must be numeric, if given
    if ( class(digits) != "numeric") {
      stop ("Error: rounding digits are not given as a numeric input.") }
    }
 
  io_table <- dplyr::mutate_if (io_table, is.factor, as.character )
  
  non_zero <- function (x) {
    if ( class ( x ) %in% c("factor", "character") ) return ( TRUE )
    ifelse (  all ( as.numeric ( unlist (x) ) == 0) , FALSE, TRUE )
  }
  
  non_zero_cols <- vapply ( io_table[, 1:ncol(io_table)], 
                            non_zero, logical (1) )
  non_zero_rows <- as.logical (non_zero_cols[-1] ) 
  
  remove_cols <- names (io_table )[! non_zero_cols]
  siot_rows <- as.character ( unlist ( io_table[,1]) )
  siot_rows
  
  io_table <- io_table [! siot_rows %in% remove_cols , 
                            ! names ( io_table ) %in% remove_cols  ]
  io_table <- dplyr::mutate_if ( io_table, is.factor, as.character )
  
  total_row <- which ( tolower(io_table[,1 ]) %in% c("cpa_total", "total") )
  
  if ( length(total_row) == 0 ) stop ("Total row not found") else {
    io_table <- io_table [1:(total_row-1), ]
  }
  
  if ( type == "product") { 
    demand_col <- which (tolower(names(io_table)) %in% c("cpa_total", "total") )
    
    if ( length(demand_col) == 0 ) { 
      stop ("Please input a table that has a total column.")
    } #end of finding total column if originally missing
    
  } else if ( type == "final_demand" ) {
    demand_col <- which (tolower(names(io_table)) %in% c("tfu", "output_bp", "total_final_use") )
  }  else {
      stop ("Paramter type must be either product or final_demand.")
    }
  
  demand_col 
  demand <- io_table [, demand_col ]
  keep_first_name <- names(io_table)[1]  #keep the first name of the table for further use, i.e. prod_na, t_rows, induse

  last_col <- which ( tolower(names(io_table)) %in% c("cpa_total", "total"))
  if ( length(last_col) == 0 ) stop ("Did not find the total column") else {
    last_col <- last_col-1 
  }
  is_last_cols <- FALSE
  
  ###Households are not needed to calculate the output coefficients------
  households_column <- which (names(io_table) %in% c("P3_S14", "households") )
  if ( length(households_column) > 0 ) {
    input_flow <- input_flow [, -households_column]
    if (last_col == households_column) last_col <- last_col-1 
  }
  
  io_table <- io_table[, 1:last_col ]
  
  ###Create the return data.frame from first column------
  first_col <- as.data.frame( io_table[ ,1] )
  names (first_col) <- keep_first_name
  
  null_to_eps <- function(x) ifelse( x==0, 0.000001, x )
  
  demand <- null_to_eps(demand)

  #forward linkeages on p507
  ##Avoid division by zero with epsilon-----
  io_table <- vapply ( io_table[1:nrow(io_table), c(2:last_col)],
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
