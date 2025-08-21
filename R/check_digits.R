#' @title Check digits parameter
#' @description This is an internal function to determine if the rounding can go ahead.
#' @param digits Digit input to check for validity.
#' @return An error if the digits are not \code{NULL} or an integer value.
#' @keywords internal
check_digits <- function(digits) {
  if (!is.null(digits)) {
    if (!inherits(digits, "numeric")) {
      stop(
        paste(
          "Error in check_digits(digits):",
          "rounding 'digits' must be numeric or NULL."
        )
      )
    }
  }
  invisible(TRUE)
}
