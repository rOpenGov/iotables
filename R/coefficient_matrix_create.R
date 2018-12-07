#' Create a coefficient matrix
#' 
#' Create a coefficient matrix from a Symmetric Input-Output Table. The
#' coefficient matrix is related by default to output, but you can change this
#' to total supply or other total aggregate if it exists in your table.
#' 
#' @param siot A symmetric input-output table retrieved by the  
#' \code{\link{iotable_get}} function. 
#' @param total Usually an output vector with a key column, defaults to 
#' \code{"output"} which equals \code{"P1"} or \code{"output_bp"}.
#' You can use other rows for comparison, for example \code{"TS_BP"} 
#' if it exists in the matrix. 
#' @param digits An integer showing the precision of the technology matrix in 
#' digits. Default is \code{NULL} when no rounding is applied.
#' @param return Defaults to \code{NULL}. You can chooce \code{"product"} or
#' \code{"industry"} to return an input coefficient matrix or \code{"primary_inputs"}
#' to receive only the total intermediate use and proportional primary inputs.
#' @param empty_rows_remove Defaults to \code{TRUE}. If you want to keep empty 
#' primary input rows, choose \code{FALSE}. Empty product/industry rows are always 
#' removed to avoid division by zero error in the analytical functions.
#' @param households Defaults to \code{NULL}. Household column can be added 
#' with \code{TRUE}.
#' @return A data.frame that contains the matrix of  \code{siot} devided by \code{total}
#' with a key column. Optionally the results are rounded to given \code{digits}. 
#' @importFrom dplyr mutate mutate_if full_join funs left_join
#' @references See \href{https://webarchive.nationalarchives.gov.uk/20160114044923/http://www.ons.gov.uk/ons/rel/input-output/input-output-analytical-tables/2010/index.html}{United Kingdom Input-Output Analytical Tables 2010}
#' for explanation on the use of the Coefficient matrix.
#' @examples 
#' coefficient_matrix_create ( siot = iotable_get ( source = "germany_1990"), 
#'                             total = "output", 
#'                             digits = 4 )
#' @export 

coefficient_matrix_create <- function ( siot, 
                                        total = "output", 
                                        digits = NULL, 
                                        empty_rows_remove = TRUE,
                                        households = FALSE,
                                        return = NULL) {
  
  #Create a coefficient matrix, including primary inputs.  
  #For the Leontieff matrix, only the inputs part (first quadrant is used)
  
  funs <- t_rows2 <-. <- NULL  #for checking against non-standard evaluation

  siot <- dplyr::mutate_if (siot, is.factor, as.character )
  
  ###Removing all zero columns and rows --------
  
  #Determine if a column is all zero
  non_zero <- function (x) {
    if ( class ( x ) %in% c("factor", "character") ) return ( TRUE )
    ifelse (  sum ( as.numeric ( unlist (x) ), na.rm=TRUE) == 0, FALSE, TRUE )
  }
  
  #Examine which columns are filled with zeros
  non_zero_cols <- vapply ( siot[, 1:ncol(siot)], 
                            non_zero, logical (1) )
  non_zero_rows <- as.logical (non_zero_cols[-1] ) 
  
  #Remove columns that are filled with zeros
  remove_cols <- names (siot )[! non_zero_cols]
  
  if ( length( remove_cols) > 0 ) {
    message ("Columns and rows of ", paste(remove_cols, collapse =', '), " are all zeros and will be removed.")
  }
  
  siot_rows <- as.character ( unlist ( siot[,1]) )
  #names ( input_flow) [! names ( input_flow ) %in% remove_cols ]
  # siot_rows [! siot_rows %in% remove_cols ]
  
  ##Now remove all zero corresponding rows
  siot <- siot [! siot_rows %in% remove_cols , 
                            ! names ( siot ) %in% remove_cols  ]
  siot <- dplyr::mutate_if ( siot, is.factor, as.character )
  
  last_column <- quadrant_separator_find( siot )

  
  if ( !is.null(households)) {
    if ( households == TRUE) { household_column <- household_column_get( siot )
    quadrant <- siot [, 1:last_column]
    siot <- dplyr::left_join ( quadrant, household_column, 
                               by = names(quadrant)[1])
    } else {
      siot <- siot [, 1:last_column]
    }
      } else {
    siot <- siot [, 1:last_column]
  }
  
 
  if ( any( c("output", "p1", "output_bp")  %in%  tolower (siot[,1])) ) {
    total_row <- siot[which ( tolower (unlist(tolower(siot[,1])))  %in%
                                c("output", "p1", "output_bp"))[1],]
  } else {
    total_row <- siot[which ( tolower (unlist(siot[,1]))  %in% c(total))[1],]
  }
  
    
  #Adjust the output vector 
  null_to_na <- function(x) ifelse (x == 0, NA, x )
  total_row <- dplyr::mutate_if ( total_row, is.factor, as.character )
  total_row <- dplyr::mutate_if ( total_row, is.numeric, null_to_na ) #avoid division by zero

  for (j in 2:ncol(siot)) {
    siot[ ,j] <-  siot[ ,j] / unlist(total_row [, j])
    }
  
   
  if ( ! is.null(return) )  {
    
    last_row <- which ( tolower(unlist(siot[,1])) %in% c("cpa_total", "total"))
    
    if ( return == "primary_inputs" ) {
      siot <- siot[last_row:nrow(siot), ]
    } else if ( return %in% c("products", "industries") ) {
      siot <- siot[1:last_row, ]
    }
  } 
  
  if ( empty_rows_remove == TRUE ) { 
    
    if (length(which( rowSums(is.na(siot)) >= ncol(siot)-1)) > 0) {
      message ( "Removed empty rows ",
                paste ( unlist(siot[which( rowSums(is.na(siot)) >= ncol(siot)-1),1]), 
                        collapse = ', '), '.')
    }
    
    siot <- siot[which( rowSums(is.na(siot)) != ncol(siot)-1 ), ]
    
    }
  
  
  if ( is.null(digits) ) return (siot)
  
  if ( class(digits) != "numeric") stop ("Error: rounding digits are not given as a numeric input.")
  
  if ( digits >= 0 ) {
    
    round_eps <- function ( x, digits ) {
      ifelse ( x == 1e-06, x, round ( x, digits ))
    }
    siot <- siot %>%
      dplyr::mutate_if(is.numeric, dplyr::funs(round_eps (., digits)))
  } else {
    stop ("Error: not a valid rounding parameter.\nMust be an integer representing the rounding digits.")
  }
  siot
}
