test_that("Rounding takes place and key column is retained", {
  de_coeff <- input_coefficient_matrix_create(
    iotable_get()
  )
  expect_equal(
    matrix_round(de_coeff, digits = 2)[, 2],
    round(de_coeff[, 2], 2)
  )
  expect_equal(matrix_round(de_coeff, 2)[, 1], de_coeff[, 1])
})


test_that("matrix_round works with zero digits", {
  df <- data.frame(key = c("a", "b"), x = c(1.234, 5.678))
  out <- matrix_round(df, 0)
  expect_equal(out$x, c(1, 6))
  expect_identical(out$key, df$key)
})

test_that("matrix_round works with negative digits", {
  df <- data.frame(key = c("a", "b"), x = c(123, 678))
  out <- matrix_round(df, -1)
  expect_equal(out$x, c(120, 680))
})
