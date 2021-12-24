context ("Rounding a named matrix")


de_coeff <- input_coefficient_matrix_create (
  iotable_get())

test_that("Rounding takes place and key column is retained", {
  expect_equal(matrix_round(de_coeff, digits = 2)[,2], round(de_coeff[,2], 2))
  expect_equal(matrix_round(de_coeff, 2)[,1], de_coeff[,1])
})
