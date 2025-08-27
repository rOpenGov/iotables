test_that("Leontief inverse works for a minimal matrix", {
  minimal_matrix <- data.frame(
    iotables_row = c("A", "B"),
    A = c(0.2, 0.4),
    B = c(0.1, 0.2)
  )

  L <- leontief_inverse_create(minimal_matrix, digits = 3)

  expected <- matrix(c(1.333, 0.667, 0.167, 1.333),
    nrow = 2,
    byrow = FALSE
  )
  colnames(expected) <- c("A", "B")
  rownames(expected) <- c("1", "2")

  expect_equal(
    as.matrix(L[, -1]),
    expected,
    tolerance = 1e-3
  )
})
