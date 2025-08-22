#' @title Forward linkages
#'
#' @description Forward linkages capture how the increased output of a sector
#' provides additional inputs to other sectors, enabling them to expand
#' production.
#'
#' @details Defined as the row sums of the Ghosh inverse, in line with the
#' Eurostat Manual of Supply, Use and Input-Output Tables (pp. 506–507) and
#' the United Nations *Handbook on Supply and Use Tables and Input-Output
#' Tables with Extensions and Applications* (p. 637).
#'
#' @param output_coefficient_matrix An output coefficient matrix created with
#'   [output_coefficient_matrix_create()].
#' @param digits Integer. Number of decimals for rounding. Defaults to `NULL`
#'   (no rounding).
#'
#' @return A `data.frame` with two columns:
#' * The metadata column from the input matrix (sector/product names)
#' * `forward_linkages`: the forward linkage indicator values
#'
#' @importFrom dplyr mutate across
#' @family linkage functions
#'
#' @examples
#' data_table <- iotable_get()
#'
#' de_out <- output_coefficient_matrix_create(
#'   data_table, "tfu",
#'   digits = 4
#' )
#'
#' forward_linkages(
#'   output_coefficient_matrix = de_out,
#'   digits = 4
#' )
#' @export
forward_linkages <- function(output_coefficient_matrix,
                             digits = NULL) {
  output_coefficient_matrix <- mutate(
    output_coefficient_matrix,
    across(where(is.factor), as.character)
  )

  G <- ghosh_inverse_create(output_coefficient_matrix)

  FLm <- G
  FLm$forward_linkages <- rowSums(G[, -1])

  if (!is.null(digits)) {
    FLm[, 2:ncol(FLm)] <- round(FLm[, 2:ncol(FLm)], digits)
  }

  FLm[, c(1, ncol(FLm))]
}
