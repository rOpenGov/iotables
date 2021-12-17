#' @title Solve a basic (matrix) equation
#' 
#' The function matches to parts of the matrix equation, using the named
#' formats with row names and solves the matrix equation.
#' This function is used in wrapper functions, such as \code{\link{multiplier_create}}.
#' to solve particular problems, but it can be used directly, too.
#' The function only performs the lhs %*% im matrix equation, but after 
#' pairing industries and checking for exceptions.
#' 
#' @param LHS A left-hand side vector with a key column containing the 
#' industry or product names for matching, for example the employment coefficients. 
#' @param Im A Leontieff-inverse with a key column containing the industry or 
#' product names for matching.
#' @importFrom dplyr select mutate mutate across full_join
#' @importFrom tidyr spread
#' @importFrom tidyselect one_of
#' @return A data.frame with auxiliary metadata to conform the symmetric
#' input-output tables.
#' @examples
#' Im = data.frame (
#' a = c("row1", "row2"), 
#' b = c(1,1), 
#' c = c(2,0))
#' LHS = data.frame (
#' a = "lhs", 
#' b = 1, 
#' c = 0.5)
#' equation_solve (Im = Im, LHS = LHS)
#' @export 

equation_solve <- function (LHS = NULL, Im = NULL) {

  if (is.null(LHS) | is.null(Im)) stop (
      "Error: matrix equation inputs are not given.")

  LHS <- LHS %>%
    mutate(across(where(is.factor), as.character)) 
  Im <- Im %>%
    mutate(across(where(is.factor), as.character)) 
  
  if (ncol (Im) < ncol(LHS)) {
    
   not_found <-  names(LHS)[ which (! names(LHS) %in% names ( Im )) ]
   
   if ( all ( not_found %in% c("CPA_T", "CPA_U", "CPA_L68A",
                               "TOTAL", "CPA_TOTAL"))) {
     warning ( paste ( not_found, collapse = ','),  
                ' from the input vector is removed. These are likely zero values, 
               and cannot be found in the Leontieff-inverse.'
              )
     LHS <- dplyr::select ( LHS, -dplyr::one_of ( not_found ) )   
     } else if  ( any( not_found  %in%  c("households", "P3_S14"))  )  {
       stop ("The input vector has households but the Leontieff-inverse has not.")
     } else {
     stop ("Non conforming input vector and Leontieff-inverse.")
   }
  }

  ###Joining matrixes to find out if all data is present ---------------------   

  joined <- tryCatch(
      full_join (LHS, Im, by = names(LHS)), 
      error = function(e) {
        message ( "The technology columns are not matching.")
        return (NULL)
      }
    )
  
  if ( is.null(joined)) stop("Error: no result is returned.")  #early termination if not
  
  ###Joining matrixes to find out if all data is present ---------------------   
  
  lhs <- joined[1,]
  lhs <- as.numeric(lhs[1,2:ncol(lhs)])  #numeric left-hand side in conforming order
  
  im <- joined[2:nrow(joined),]
  im <- as.matrix(im[,2:ncol(im)])  #numeric Leontieff inverse in conforming order
   

  ###Try to solve the matrix equation  ---------------------   
  
  solution <- tryCatch(
    lhs %*% im, 
    error = function(e) {
      message ( "Violoation of the matrix operation.")
      return (NULL)}
  )
  
  solution
 }  #end of function  



