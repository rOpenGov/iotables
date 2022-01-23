#' @title Create an output coefficient matrix
#' 
#' @description Create an output coefficient matrix from the input flow matrix or a symmetric
#' input-output table. 
#' 
#' @details The output coefficients may be interpreted as the market shares of products
#' in total output. If there are zero values in present, they will be changed to 
#' 0.000001 and you will get a warning. Some analytical equations cannot be 
#' solved with zero elements. You either have faulty input data, or you have 
#' to use some sort of data modification to carry on your analysis. 
#' @param data_table A symmetric input-output table, a use table, 
#' a margins or tax table retrieved by the \code{\link{iotable_get}}. 
#' In case you use \code{type="tfu"} you need to input a
#' full iotable, create by the \code{\link{iotable_get}}, because 
#' the final demand column is in the second quadrant of the IOT.
#' @param total The \code{output='total'} (or CPA_TOTAL, depending on the 
#' names in your table, default) returns the output coefficients
#' for products (intermediates) while the \code{final_demand} returns output 
#' coefficients for final demand. See 
#' \href{https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/b0b3d71e-3930-4442-94be-70b36cea9b39}{Eurostat Manual of Supply, Use and Input-Output Tables}
#' p495 and p507.
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @return An output coefficient matrix of data.frame class. 
#' The column names are ordered, and the row names are in the 
#' first, auxiliary metadata column.
#' @importFrom dplyr mutate across
#' @examples 
#' data_table <- iotable_get() 
#' 
#' output_coefficient_matrix_create (data_table = data_table, 
#'                                   total = 'tfu',
#'                                   digits = 4)
#' @export 

output_coefficient_matrix_create <- function (data_table,
                                              total = "tfu",
                                              digits = NULL) {
  check_digits ( digits = digits)
  
  data_table <- data_table %>% mutate(across(where(is.factor), as.character))
  
  ###Find non-zero cols and rows and remove them---- 
  data_table <- empty_remove ( data_table )
  
  total_row <- which ( tolower(as.character(unlist(data_table[, 1])))
                       %in% c("cpa_total", "total") )
  
  if ( length(total_row) == 0 ) stop ("Total row not found") else {
    data_table <- data_table [1:(total_row-1), ]
  }
  
  if ( total == "total" ) { 
    demand_col <- which (tolower(names(data_table)) %in% c("cpa_total", "total") )
    last_column <- quadrant_separator_find ( data_table  )
    if ( length(demand_col) == 0 ) { 
      stop ("Please input a table that has a total column.")
    } #end of finding total column if originally missing
    
  } else if ( tolower(total) %in% c("total_final_use", "tfu", "final_demand")  ) {
    demand_col <- which (tolower(names(data_table)) %in% 
                           c("tfu", "total_final_use") )
    last_column <- quadrant_separator_find ( data_table, 
                                             include_total = FALSE )
   }  else {
      stop ("Paramter 'output' must be any of 'CPA_TOTAL', 'TOTAL', 'final_demand', 'tfu' or 'total_final_use'.")
    }
  
  demand <- data_table [, demand_col ]
  demand
  
  data_table <- data_table %>% 
    mutate(across(where(is.factor), as.character)) %>%
    select( 1:last_column ) # selection should be explicit?
  # The solution suggested by tidyselect 
  # all_of(last_column)` instead of `last_column` is not a good solution 
  
  keep_first_name <- names(data_table)[1]  #keep the first name of the table for further use, i.e. prod_na, t_rows, induse

  data_table <- data_table[, 1:last_column ]
  
  ###Create the return data.frame from first column------
  first_col <- as.data.frame( data_table[ ,1] )
  names (first_col) <- keep_first_name
  
  null_to_eps <- function(x) ifelse( x==0, 0.000001, x )
  
  demand <- null_to_eps(as.numeric(unlist(demand)))

  #forward linkeages on p507
  ##Avoid division by zero with epsilon-----
  data_table <- vapply ( data_table[seq_len(nrow(data_table)), c(2:last_column)],
                 null_to_eps, numeric (nrow(data_table)) )
  
  output_coeff <- apply (data_table, 2,
                         function(i)i/demand)

  output_coeff <- as.data.frame (output_coeff)
  output_coeff <- cbind (first_col, output_coeff)

  if ( is.null(digits) ) return (output_coeff)
  
  if ( digits >= 0 ) {
    round_eps <- function ( x, digits ) {
      ifelse ( x == 1e-06, x, round ( x, digits ))
    }
    output_coeff<- output_coeff %>%
      mutate(across(where(is.numeric), round_eps, digits))
  } else {
    stop ("Error: not a valid rounding parameter.\nMust be an integer representing the rounding digits.")
  }
  output_coeff
}
