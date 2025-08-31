#' Backward Linkages
#'
#' Compute the backward linkages of each industry or product sector from a
#' Leontief inverse matrix. Backward linkages indicate how strongly a sector is
#' interconnected on the demand side: when a sector increases its output, it
#' will increase intermediate demand on all other sectors.
#'
#' @details Backward linkages are defined as the column sums of the Leontief
#'   inverse, in line with the *Eurostat Manual of Supply, Use and Input–Output
#'   Tables* (pp. 506–507) and the *United Nations Handbook on Supply and Use
#'   Tables and Input–Output Tables with Extensions and Applications* (p. 636).
#'
#' @param Im A Leontief inverse matrix created by [leontief_inverse_create()].
#'
#' @return A one-row `data.frame` containing the backward linkage values for
#'   each column (industry or product) of the Leontief inverse. The first column
#'   is the sector key column, and the remaining columns correspond to the
#'   linkage values.
#'
#' @family linkage functions
#'
#' @examples
#' de_coeff <- input_coefficient_matrix_create(iotable_get(), digits = 4)
#' I <- leontief_inverse_create(de_coeff)
#' backward_linkages(I)
#'
#' # Trivial example: identity matrix gives linkages = 1
#' I <- diag(3)
#' colnames(I) <- rownames(I) <- c("A", "B", "C")
#' I_df <- data.frame(sector = rownames(I), I, check.names = FALSE)
#' backward_linkages(I_df)
#'
#' @importFrom dplyr mutate across
#' @importFrom tidyselect where
#' @export
backward_linkages <- function(Im) {
  
  Im <- dplyr::mutate(Im, dplyr::across(where(is.factor), 
                                        function(x) as.character(x))
                      )

  total_row <- data.frame(
    name = "backward linkages"
  )

  names(total_row)[1] <- names(Im[1])

  total_row <- cbind(
    total_row,
    t(colSums(Im[, 2:ncol(Im)]))
  )

  total_row
}
