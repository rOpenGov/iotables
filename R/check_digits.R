#' @title Check digits parameter
#' 
#' This is an internal function to determine if the rounding can go ahead.
#' @param digits Digit input to check for validity.
#' @return An error if the digits are not \code{NULL} or an integer value.
#' @keywords internal

check_digits <- function(digits) {
  
  if (!is.null(digits)) { ##rounding digits must be numeric, if given
    if ( class(digits) != "numeric") {
      stop ("Error: rounding digits are not given as a numeric input.") }
  }
  
}
