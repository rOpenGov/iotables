test_that("incorrect spelling is warned", {
  de_coeff <- input_coefficient_matrix_create(
    data_table = iotable_get(),
    households = FALSE
  )
  expect_warning(leontieff_matrix_create(
    technology_coefficients_matrix =
      de_coeff
  ))
})

test_that("Leontief matrix matches Eurostat Manual (Germany 1995)", {
  de_coeff <- input_coefficient_matrix_create(
    data_table = iotable_get(),
    households = FALSE,
    digits = 4
  )

  L_de <- leontief_matrix_create(de_coeff)

  # Agriculture diagonal (Eurostat Table 15.9, p. 487)
  expect_equal(
    L_de$agriculture_group[L_de$iotables_row == "agriculture_group"],
    0.9742,
    tolerance = 1e-4
  )

  # Industry diagonal (Eurostat Table 15.9, p. 487)
  expect_equal(
    L_de$industry_group[L_de$iotables_row == "industry_group"],
    0.7178,
    tolerance = 1e-4
  )

  # Construction diagonal
  expect_equal(
    L_de$construction[L_de$iotables_row == "construction"],
    0.9842,
    tolerance = 1e-4
  )

  # Spot-check off-diagonal value:
  # Industry row, Construction column ~ -0.2613 (Eurostat Table 15.9)
  expect_equal(
    L_de$construction[L_de$iotables_row == "industry_group"],
    -0.2613,
    tolerance = 1e-4
  )
})


test_that("Netherlands 2000 Leontief matrix is correctly formed", {
  nl_coeff <- input_coefficient_matrix_create(netherlands_2000, digits = 3)
  nl_L <- leontief_matrix_create(nl_coeff)

  # Structure checks
  expect_s3_class(nl_L, "data.frame")
  expect_equal(nrow(nl_L), 6L)
  expect_equal(ncol(nl_L), 7L) # 1 key column + 6 industries

  # Spot checks against known I - A values
  # Agriculture → Agriculture should be ~0.875
  expect_equal(as.numeric(nl_L[1, "agriculture_group"]), 0.875, tolerance = 1e-3)

  # Mining → Mining should be ~0.977
  expect_equal(as.numeric(nl_L[2, "mining_group"]), 0.977, tolerance = 1e-3)

  # Services → Services should be ~0.755
  expect_equal(as.numeric(nl_L[6, "services_group"]), 0.755, tolerance = 1e-3)
})
