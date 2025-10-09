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


test_that("leontief_inverse_create() reproduces Beutel Table 15.10
          on page 488", {
  cm_de <- input_coefficient_matrix_create(
    data_table = iotable_get(source = "germany_1995")
  )
  L <- leontief_inverse_create(cm_de)

  # Table 15.10, diagonal values
  # ~1.0339, 1.4292, 1.0289 for the same sectors
  expect_equal(round(L[1, 2], 3), 1.0339, tolerance = 1e-3)
  expect_equal(round(L[2, 3], 3), 1.4292, tolerance = 1e-3)
  expect_equal(round(L[3, 4], 3), 1.0289, tolerance = 1e-3)
})
