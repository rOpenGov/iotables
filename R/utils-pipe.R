#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' This re-exports the pipe operator from **magrittr** so that it can be
#' used within the package without explicitly attaching magrittr.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
