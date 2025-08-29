#' Create a stacked symmetric input–output table (SIOT)
#'
#' @description
#' Build a SIOT with the Eurostat/Beutel layout:
#' - Quadrant 1: intermediate uses (**Z**), industries × industries.
#' - Quadrant 2: final uses (**Y**), industries × final-demand categories.
#' - Quadrant 3: primary inputs (**V**), primary-input rows × industries.
#' - Quadrant 4: empty block (primary-input rows × final-demand cols),
#'   filled with `NA_real_`.
#'
#' @param q1 Numeric `n × n` matrix (Quadrant 1). Row/col names are ids.
#' @param q2 Numeric `n × k` matrix (Quadrant 2). Row names = `q1` ids.
#' @param q3 Numeric `m × n` matrix (Quadrant 3). Col names = `q1` ids.
#' @param q1_id Character vector of industries/products (length = n).
#' @param final_use_id Character vector of final-demand categories (len = k).
#' @param primary_id Character vector of primary inputs (length = m).
#' @param id_col Name of the identifier column in the output tibble.
#'
#' @returns
#' A tibble with rows = industries + primary inputs,
#' cols = `id_col`, industries, final demand. Quadrant 4 is `NA_real_`.
#'
#' @examples
#' ids <- c("p1","p2")
#' final_ids <- c("hfce","gfcf")
#' prim_ids <- c("comp_lab","os_mi")
#'
#' q1 <- matrix(c(10,2, 3,15), 2, byrow = TRUE,
#'              dimnames = list(ids, ids))
#' q2 <- matrix(c(5,7, 8,9), 2,
#'              dimnames = list(ids, final_ids))
#' q3 <- matrix(c(12,14, 6,11), 2,
#'              byrow = TRUE,
#'              dimnames = list(prim_ids, ids))
#'
#' siot <- iotable_create(q1,q2,q3,
#'                        q1_id=ids,
#'                        final_use_id=final_ids,
#'                        primary_id=prim_ids)
#' siot
#'
#' @export
iotable_create <- function(q1, q2, q3,
                           q1_id, final_use_id, primary_id,
                           id_col = "prod_na") {
  n <- length(q1_id)
  k <- length(final_use_id)
  m <- length(primary_id)

  # --- Validate and assign names ---
  if (is.null(dimnames(q1))) dimnames(q1) <- list(q1_id, q1_id)
  if (is.null(dimnames(q2))) dimnames(q2) <- list(q1_id, final_use_id)
  if (is.null(dimnames(q3))) dimnames(q3) <- list(primary_id, q1_id)

  stopifnot(identical(rownames(q1), q1_id),
            identical(colnames(q1), q1_id),
            identical(rownames(q2), q1_id),
            identical(colnames(q2), final_use_id),
            identical(colnames(q3), q1_id),
            identical(rownames(q3), primary_id))

  # --- Quadrant 1 + 2 block (industries) ---
  upper <- cbind(q1, q2)

  # --- Quadrant 3 + 4 block (primary inputs) ---
  q4 <- matrix(NA_real_, nrow = m, ncol = k,
               dimnames = list(primary_id, final_use_id))
  lower <- cbind(q3, q4)

  # --- Stack vertically ---
  mat <- rbind(upper, lower)

  # --- Build tibble ---
  out <- tibble::tibble(!!id_col := rownames(mat),
                        !!!as.data.frame(mat))

  # Attributes: column indices for quadrants
  q1_idx <- match(q1_id, names(out))
  q2_idx <- match(final_use_id, names(out))
  # Quadrant 3 are *rows*, not cols — mark by id
  attr(out, "quadrants") <- list(q1 = q1_idx,
                                 q2 = q2_idx,
                                 q3 = primary_id)
  attr(out, "id_col") <- id_col
  out
}


