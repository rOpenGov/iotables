#' Create a coefficient matrix
#' 
#' Create a coefficient matrix from a Symmetric Input-Output Table. The
#' coefficient matrix is related by default to output, but you can change this
#' to total supply or other total aggregate if it exists in your table.
#' 
#' @param data_table A symmetric input-output table, a use table, 
#' a margins or tax table retrieved by the  \code{\link{iotable_get}}
#'  function. 
#' @param total Usually an output vector with a key column, defaults to 
#' \code{"output"} which equals \code{"P1"} or \code{"output_bp"}.
#' You can use other rows for comparison, for example \code{"TS_BP"} 
#' if it exists in the matrix. 
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @param return_part Defaults to \code{NULL}. You can chooce \code{"product"} or
#' \code{"industry"} to return an input coefficient matrix or \code{"primary_inputs"}
#' to receive only the total intermediate use and proportional primary inputs.
#' @param remove_empty Defaults to \code{TRUE}. If you want to keep empty 
#' primary input rows, choose \code{FALSE}. Empty product/industry rows are always 
#' removed to avoid division by zero error in the analytical functions.
#' @param households Defaults to \code{NULL}. Household column can be added 
#' with \code{TRUE}.
#' @return A data.frame that contains the matrix of  \code{data_table} divided by \code{total}
#' with a key column. Optionally the results are rounded to given \code{digits}. 
#' @importFrom dplyr mutate mutate_if funs left_join
#' @references See \href{https://webarchive.nationalarchives.gov.uk/20160114044923/http://www.ons.gov.uk/ons/rel/input-output/input-output-analytical-tables/2010/index.html}{United Kingdom Input-Output Analytical Tables 2010}
#' for explanation on the use of the Coefficient matrix.
#' @examples 
#' coefficient_matrix_create ( data_table = iotable_get ( source = "germany_1990"), 
#'                             total = "output", 
#'                             digits = 4 )
#' @export 

coefficient_matrix_create <- function ( data_table, 
                                        total = "output", 
                                        digits = NULL, 
                                        remove_empty = TRUE,
                                        households = FALSE,
                                        return_part = NULL) {
  
  #Create a coefficient matrix, including primary inputs.  
  #For the Leontieff matrix, only the inputs part (first quadrant is used)
  
  funs <-. <- NULL  #for checking against non-standard evaluation

  data_table <- dplyr::mutate_if (data_table, is.factor, as.character )
  
  ###Removing all zero columns and rows --------
  if ( remove_empty ) data_table <- iotables:::empty_remove ( data_table )
  
  last_column <- iotables:::quadrant_separator_find( data_table, 
                                                     include_total = FALSE)

  #####removing the 2nd and 4th quadrants--- 
  if ( !is.null(households) ) { 
    if ( households == TRUE ) { 
      household_column <- household_column_get( data_table )
      quadrant <- data_table [, 1:last_column]
      data_table <- dplyr::left_join ( quadrant, household_column, 
                                 by = names(quadrant)[1])
    } else {
      data_table <- data_table [, 1:last_column]
    }
      } else {
    data_table <- data_table [, 1:last_column]
      }
  
  key_column <- tolower(as.character(unlist(data_table[,1])))
  key_column
  
  ##Getting the row for division
  if ( total %in%  c("output", "p1", "output_bp")  ) { 
    if ( any( c("output", "p1", "output_bp")  %in%  key_column )) {
      total_row <- data_table[which ( key_column  %in%
                                  c("output", "p1", "output_bp"))[1],]
    } else {
      stop ( "The output row was not found in the table as 'output', 
             'p1' or 'output_bp'")
      } 
    } else if ( total %in%  c("total", "cpa_total")  ) { 
    if ( any( c("total", "cpa_total") %in%  key_column ) ) {
      total_row_n <- which (  key_column  %in% c("total", "cpa_total"))[1]
      total_row <- data_table[total_row_n,]
      }
    } else {
      total_row <- data_table[which ( key_column %in% c(total))[1],]
      if ( length(total_row) == 0) stop("The total row was not found.")
    } #end of else

  
  ##Adjust the total vector---- 
  null_to_eps <- function(x) ifelse (x == 0, 0.000001, x )
  total_row <- dplyr::mutate_if ( total_row, is.factor, as.character )
  total_row <- dplyr::mutate_if ( total_row, is.numeric, null_to_eps ) #avoid division by zero

  coeff_matrix <- data_table
  
  ###The actual creation of the coefficients-----
  for ( i in 1:nrow(data_table) ) {
    coeff_matrix[i,2:last_column] <-  coeff_matrix[i,2:last_column] / as.numeric(total_row[2:last_column])
  }
 
  potential_houeshold_earning_names <- c("compensation_employees", 'd1')
  
  earnings_name <- potential_houeshold_earning_names [which ( potential_houeshold_earning_names  %in% key_column )]

  household_earnings_row <- coeff_matrix[which( earnings_name == key_column), ]
    

  ###If only a part should be returned----
  if ( ! is.null(return_part) )  {
    
    last_row <- which ( tolower(unlist(data_table[,1])) %in% c("cpa_total", "total")) #not last column
    
    if ( return_part == "primary_inputs" ) {
      coeff_matrix <- coeff_matrix[last_row:nrow(coeff_matrix), ]  #households remain anyway
    } else if ( return_part %in% c("products", "industries") ) {
      coeff_matrix <- coeff_matrix[1:last_row, ]
      if ( households == TRUE ) {  #households re_added if they were removed
        coeff_matrix <- rbind ( coeff_matrix, household_earnings_row )
      }
    }
  } 
  
  ###If rounding should happen-----
  if ( is.null(digits) ) return (coeff_matrix)
  
  iotables:::round_table ( coeff_matrix, digits = digits  )
}
