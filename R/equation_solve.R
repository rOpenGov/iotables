#' Solve a basic matrix equation
#'
#' Match a left-hand side (LHS) vector to a Leontief inverse by column names
#' and compute the matrix product \eqn{\text{LHS} \times \text{Im}}.
#'
#' @details
#' This helper is used by higher-level wrappers such as
#' [multiplier_create()]. It assumes both inputs have a first key column,
#' followed by numeric columns whose names define the alignment. The function
#' multiplies the numeric row of `LHS` by the numeric block of `Im` after a
#' basic conformity check.
#'
#' @param LHS A one-row data frame (or matrix) with a key column first and
#'   numeric columns named to match the Leontief inverse.
#' @param Im A Leontief inverse with a key column first and a square numeric
#'   block whose column names match the `LHS` numeric names.
#'
#' @return A numeric 1×N matrix containing the solution
#'   \eqn{\text{LHS} \times \text{Im}}.
#'
#' @importFrom dplyr select mutate across full_join any_of
#'
#' @examples
#' Im <- data.frame(
#'   a = c("row1", "row2"),
#'   b = c(1, 1),
#'   c = c(2, 0)
#' )
#' LHS <- data.frame(
#'   a = "lhs",
#'   b = 1,
#'   c = 0.5
#' )
#' equation_solve(Im = Im, LHS = LHS)
#'
#' @export

equation_solve <- function(LHS = NULL, Im = NULL) {
  if (is.null(LHS) | is.null(Im)) {
    stop(
      "Error: matrix equation inputs are not given."
    )
  }

  LHS <- LHS %>%
    mutate(across(where(is.factor), as.character))

  Im <- Im %>%
    mutate(across(where(is.factor), as.character))

  if (ncol(Im) < ncol(LHS)) {
    not_found <- names(LHS)[which(!names(LHS) %in% names(Im))]

    if (all(not_found %in% c(
      "CPA_T", "CPA_U", "CPA_L68A",
      "TOTAL", "CPA_TOTAL"
    ))) {
      warning(
        paste(not_found, collapse = ","),
        " from the input vector is removed. These are likely zero values,
               and cannot be found in the Leontief-inverse."
      )
      LHS <- dplyr::select(LHS, -dplyr::any_of(not_found))
    } else if (any(not_found %in% c("households", "P3_S14"))) {
      stop("The input vector has households but the Leontief-inverse has not.")
    } else {
      stop("Non conforming input vector and Leontief-inverse.")
    }
  }

  ### Joining matrices to find out if all data is present ---------------------

  names_lhs <- names(LHS)
  names_Im <- names(Im)

  names_lhs
  names_Im

  joined <- tryCatch(
    full_join(LHS, Im, by = names(LHS)),
    error = function(e) {
      message("The technology columns are not matching.")
      return(NULL)
    }
  )

  if (is.null(joined)) {
    stop("Error: no result is returned.") # early termination if not
  }

  ### Joining matrices to find out if all data is present -----------------

  lhs <- joined[1, ]
  lhs <- as.numeric(lhs[1, 2:ncol(lhs)]) # numeric left-hand side in conforming order

  # lhs <- LHS[ ,which ( vapply(LHS,is.numeric,  logical(1)))]
  # lhs <- lhs %>% select ( any_of(names(Im))) %>% as.matrix()

  im <- joined[2:nrow(joined), ]
  im <- as.matrix(im[, 2:ncol(im)]) # numeric Leontief inverse in conforming order

  # im <- Im[, which ( vapply(LHS,is.numeric,  logical(1)))]


  ### Try to solve the matrix equation  ---------------------

  solution <- tryCatch(
    lhs %*% im,
    error = function(e) {
      message("Violation of the matrix operation.")
      return(NULL)
    }
  )

  solution
} # end of function
