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
#' ids <- c("p1", "p2")
#' final_ids <- c("hfce", "gfcf")
#' prim_ids <- c("comp_lab", "os_mi")
#'
#' q1 <- matrix(c(10, 2, 3, 15), 2,
#'   byrow = TRUE,
#'   dimnames = list(ids, ids)
#' )
#' q2 <- matrix(c(5, 7, 8, 9), 2,
#'   dimnames = list(ids, final_ids)
#' )
#' q3 <- matrix(c(12, 14, 6, 11), 2,
#'   byrow = TRUE,
#'   dimnames = list(prim_ids, ids)
#' )
#'
#' siot <- iotable_create(q1, q2, q3,
#'   q1_id = ids,
#'   final_use_id = final_ids,
#'   primary_id = prim_ids
#' )
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

  stopifnot(
    identical(rownames(q1), q1_id),
    identical(colnames(q1), q1_id),
    identical(rownames(q2), q1_id),
    identical(colnames(q2), final_use_id),
    identical(colnames(q3), q1_id),
    identical(rownames(q3), primary_id)
  )

  # --- Quadrant 1 + 2 block (industries) ---
  upper <- cbind(q1, q2)

  # --- Quadrant 3 + 4 block (primary inputs) ---
  q4 <- matrix(NA_real_,
    nrow = m, ncol = k,
    dimnames = list(primary_id, final_use_id)
  )
  lower <- cbind(q3, q4)

  # --- Stack vertically ---
  mat <- rbind(upper, lower)

  # --- Build tibble ---
  out <- tibble::tibble(
    !!sym(id_col) := rownames(mat),
    !!!as.data.frame(mat)
  )

  # Attributes: column indices for quadrants
  q1_idx <- match(q1_id, names(out))
  q2_idx <- match(final_use_id, names(out))

  # Quadrant 3 are *rows*, not cols — mark by id
  attr(out, "quadrants") <- list(
    q1 = q1_idx,
    q2 = q2_idx,
    q3 = primary_id
  )
  attr(out, "id_col") <- id_col
  out
}

#' Create a stacked SIOT with optional Q1 "fence" totals
#'
#' @description
#' Build a stacked SIOT:
#' - Q1 (**Z**): industries × industries.
#' - Q2 (**Y**): industries × final demand.
#' - Q3 (**V**): primary inputs × industries (stacked as rows).
#' - Q4: primary inputs × final demand (filled with `NA_real_`).
#'
#' Optionally "fence" Q1 by adding:
#' - a total **column** with `colSums(Z)` (label `fence_col_label`);
#' - a total **row** with `rowSums(Z)` (label `fence_row_label`).
#'
#' @param q1 Numeric `n×n` matrix (Z). Row/col names are `q1_id`.
#' @param q2 Numeric `n×k` matrix (Y). Row names = `q1_id`.
#' @param q3 Numeric `m×n` matrix (V). Col names = `q1_id`.
#' @param q1_id Character vector of industries/products (len `n`).
#' @param final_use_id Character vector of final-use cols (len `k`).
#' @param primary_id Character vector of primary-input rows (len `m`).
#' @param id_col Name of the identifier column in the tibble.
#' @param fence_q1 Logical; add Q1 total row/col when `TRUE`. Default `FALSE`.
#' @param fence_row_label Label for Q1 total row (default
#'   `"intermediate_consumption"`).
#' @param fence_col_label Label for Q1 total column (default `"total"`).
#'
#' @returns
#' A tibble with rows = industries + (optional fence row) + primary inputs;
#' columns = `id_col`, industries + (optional fence col) + final demand.
#' Quadrant 4 is `NA_real_`. Attributes:
#' - `quadrants$q1`, `quadrants$q2` (column indices),
#' - `quadrants$q3` (row labels of primary inputs),
#' - `fence$row_label`, `fence$col_label` when present,
#' - `id_col`.
#'
#' @examples
#' ids <- c("p1", "p2")
#' fin <- c("hfce", "gfcf")
#' pri <- c("va", "taxes")
#' q1 <- matrix(c(10, 2, 3, 15), 2, byrow = TRUE, dimnames = list(ids, ids))
#' q2 <- matrix(c(5, 7, 8, 9), 2, dimnames = list(ids, fin))
#' q3 <- matrix(c(12, 14, 6, 11), 2, byrow = TRUE, dimnames = list(pri, ids))
#' siot <- iotable_create_stacked(q1, q2, q3, ids, fin, pri,
#'   fence_q1 = TRUE
#' )
#' @export
iotable_create_stacked <- function(q1, q2, q3,
                                   q1_id, final_use_id, primary_id,
                                   id_col = "prod_na",
                                   fence_q1 = FALSE,
                                   fence_row_label =
                                     "intermediate_consumption",
                                   fence_col_label = "total") {
  n <- length(q1_id)
  k <- length(final_use_id)
  m <- length(primary_id)

  if (is.null(dimnames(q1))) dimnames(q1) <- list(q1_id, q1_id)
  if (is.null(dimnames(q2))) dimnames(q2) <- list(q1_id, final_use_id)
  if (is.null(dimnames(q3))) dimnames(q3) <- list(primary_id, q1_id)

  stopifnot(
    identical(rownames(q1), q1_id),
    identical(colnames(q1), q1_id),
    identical(rownames(q2), q1_id),
    identical(colnames(q2), final_use_id),
    identical(rownames(q3), primary_id),
    identical(colnames(q3), q1_id)
  )

  # Q1+Q2 block (industries)
  upper <- cbind(q1, q2)

  # Q3+Q4 block (primary inputs; Q4 = NA_real_)
  q4 <- matrix(NA_real_,
    nrow = m, ncol = k,
    dimnames = list(primary_id, final_use_id)
  )
  lower <- cbind(q3, q4)

  # Optional fence: add Q1 totals
  fence_col_idx <- integer()
  fence_row_present <- FALSE
  if (isTRUE(fence_q1)) {
    # total column = colSums(Z), placed after industry columns
    fence_col <- matrix(colSums(q1),
      nrow = n, ncol = 1,
      dimnames = list(q1_id, fence_col_label)
    )
    upper <- cbind(q1, fence_col, q2)
    lower <- cbind(
      q3, matrix(NA_real_,
        nrow = m, ncol = 1,
        dimnames = list(
          primary_id,
          fence_col_label
        )
      ),
      q4
    )
    fence_col_idx <- match(
      fence_col_label,
      colnames(cbind(q1, fence_col, q2))
    )
    # total row = rowSums(Z), placed under industries (before Q3)
    fence_row <- matrix(rowSums(q1),
      nrow = 1, byrow = TRUE,
      dimnames = list(fence_row_label, q1_id)
    )
    fence_row_Q4 <- matrix(NA_real_,
      nrow = 1, ncol = k,
      dimnames = list(fence_row_label, final_use_id)
    )
    upper <- rbind(upper, cbind(fence_row, fence_row_Q4))
    fence_row_present <- TRUE
  }

  mat <- rbind(upper, lower)

  out <- tibble::tibble(
    !!sym(id_col) := rownames(mat),
    !!!as.data.frame(mat)
  )

  # Attributes
  q1_cols <- colnames(q1)
  all_cols <- names(out)
  q1_idx <- match(q1_cols, all_cols)
  q2_idx <- match(final_use_id, all_cols)

  attr(out, "quadrants") <- list(
    q1 = q1_idx,
    q2 = q2_idx,
    q3 = primary_id
  )
  attr(out, "id_col") <- id_col
  if (isTRUE(fence_q1)) {
    attr(out, "fence") <- list(
      row_label = fence_row_label,
      col_label = fence_col_label,
      row_present = fence_row_present,
      col_index = fence_col_idx
    )
  }
  out
}

#' Remove Q1 fence totals before matrix algebra
#'
#' @description
#' Drop the optional Q1 total row/column that fence Quadrant 1, using
#' labels stored by `iotable_create_stacked()`. If labels are not found,
#' the input is returned unchanged.
#'
#' @param x A stacked SIOT tibble made by `iotable_create_stacked()`.
#' @returns The same tibble without the fence row/column (if present).
#' @examples
#' x <- iotable_create_stacked(q1, q2, q3, ids, fin, pri, fence_q1 = TRUE)
#' x2 <- iotable_unfence(x) # safe to feed into matrix equations
#' @export
iotable_unfence <- function(x) {
  id_col <- attr(x, "id_col", exact = TRUE)
  fence <- attr(x, "fence", exact = TRUE)
  if (is.null(fence)) {
    return(x)
  }

  # drop fence column if present
  if (!is.null(fence$col_label) && fence$col_label %in% names(x)) {
    x <- dplyr::select(x, -dplyr::all_of(fence$col_label))
  }

  # drop fence row if present
  if (!is.null(fence$row_label)) {
    rn <- x[[id_col]]
    drop_rows <- which(rn == fence$row_label)
    if (length(drop_rows)) x <- x[-drop_rows, , drop = FALSE]
  }

  # keep attributes consistent (q1/q2 indices shift if col dropped)
  q_attr <- attr(x, "quadrants", exact = TRUE)
  if (!is.null(q_attr)) {
    q_attr$q1 <- match(setdiff(q_attr$q1, NA_integer_), names(x))
    q_attr$q2 <- match(setdiff(q_attr$q2, NA_integer_), names(x))
    attr(x, "quadrants") <- q_attr
  }
  attr(x, "fence") <- NULL
  x
}
