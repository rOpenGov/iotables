#' Solve a basic equation
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
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join mutate_if
#' @importFrom tidyr spread
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

equation_solve <- function ( LHS = NULL, Im = NULL ) {

  if ( is.null(LHS)| is.null(Im) ) stop (
      "Error: matrix equation inputs are not given.")

  LHS <- LHS %>%
    mutate_if (is.factor, as.character) 
  Im <- Im %>%
    mutate_if (is.factor, as.character) 
  
  joined <- tryCatch(
      full_join (LHS, Im, by = names(LHS)), 
      error = function(e) {
        message ( "The technology columns are not matching.")
        return (NULL)
      }
    )
    if ( is.null(joined)) stop("Error: no result is returned.")
    lhs <- joined[1,]
    lhs <- as.numeric(lhs[1,2:ncol(lhs)])
    im <- joined[2:nrow(joined),]
    im <- as.matrix(im[,2:ncol(im)])
   

  solution <- tryCatch(
    lhs %*% im, 
    error = function(e) {
      message ( "Violoation of the matrix operation.")
      return (NULL)}
  )
  
  return(solution) 
 }  #end of function  



