test_that("Netherlands 2000 input coefficients are correctly computed", {
  nl_coeff <- input_coefficient_matrix_create(netherlands_2000, digits = 3)

  # Must be a data frame with correct dimensions (6x6 + key col)
  expect_s3_class(nl_coeff, "data.frame")
  expect_equal(nrow(nl_coeff), 6L)
  expect_equal(ncol(nl_coeff), 7L) # 1 key column + 6 industries

  # Spot checks against Spicosa/Beutel values
  # Agriculture → Agriculture ~ 0.125
  expect_equal(as.numeric(nl_coeff[1, "agriculture_group"]), 0.125, tolerance = 1e-3)

  # Mining → Mining ~ 0.023
  expect_equal(as.numeric(nl_coeff[2, "mining_group"]), 0.023, tolerance = 1e-3)

  # Manufacturing → Manufacturing ~ 0.191
  expect_equal(as.numeric(nl_coeff[3, "manufacturing_group"]), 0.191, tolerance = 1e-3)
})
