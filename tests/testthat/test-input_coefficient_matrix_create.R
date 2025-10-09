test_that("input_coefficient_matrix_create works on Eurostat-style SIOT structure", {
  eurostat_like <- data.frame(
    prod_na = c("CPA_A01", "CPA_B", "CPA_C", "cpa_total", "output"),
    CPA_A01 = c(100, 20, 10, 130, 260),
    CPA_B   = c(10, 80, 30, 120, 300),
    CPA_C   = c(5, 10, 90, 105, 240),
    TOTAL   = c(5, 10, 90, 105, 240),
    P3_S14  = c(50, 40, 60, 150, NA_real_) # final use (household consumption)
  )

  cm <- input_coefficient_matrix_create(eurostat_like)

  # 1. Matrix shape
  expect_s3_class(cm, "data.frame")
  expect_true(all(sapply(cm[-1], is.numeric)))
  expect_true(nrow(cm) >= 3)

  # 2. Coefficients match known ratios
  # A_ij = Z_ij / x_j (here, output row = c(260, 300, 240))
  expect_equal(round(cm[1, "cpa_a01"], 3), round(100 / 260, 3))
  expect_equal(round(cm[2, "cpa_b"], 3), round(80 / 300, 3))
  expect_equal(round(cm[3, "cpa_c"], 3), round(90 / 240, 3))

  # 3. Check totals removed
  expect_false(any(grepl("total", names(cm), ignore.case = TRUE)))
})

test_that("Netherlands 2000 input coefficients are correctly computed", {
  nl_coeff <- input_coefficient_matrix_create(
    netherlands_2000,
    digits = 3
  )

  # Must be a data frame with correct dimensions (6x6 + key col)
  expect_s3_class(nl_coeff, "data.frame")
  expect_equal(nrow(nl_coeff), 6L)
  expect_equal(ncol(nl_coeff), 7L) # 1 key column + 6 industries

  # Spot checks against Spicosa/Beutel values
  # Agriculture → Agriculture ~ 0.125
  expect_equal(as.numeric(nl_coeff[1, "agriculture_group"]),
    0.125,
    tolerance = 1e-3
  )

  # Mining → Mining ~ 0.023
  expect_equal(as.numeric(nl_coeff[2, "mining_group"]),
    0.023,
    tolerance = 1e-3
  )

  # Manufacturing → Manufacturing ~ 0.191
  expect_equal(as.numeric(nl_coeff[3, "manufacturing_group"]),
    0.191,
    tolerance = 1e-3
  )
})

test_that("input_coefficient_matrix_create reproduces Beutel (2008)
          Table 15.6, page 485", {
  cm_de <- input_coefficient_matrix_create(
    iotable_get(source = "germany_1995"),
    digits = 4
  )

  # First cell (Agriculture → Agriculture)
  # Beutel Table 15.5, value = 0.0258
  expect_equal(round(as.numeric(cm_de[1, 2]), 4), 0.0258)

  # Agriculture → Manufacturing (approx 0.2822)
  expect_equal(as.numeric(cm_de[2, 3]), 0.2822, tolerance = 1e-3)

  # Construction → Construction (approx  0.0158)
  expect_equal(as.numeric(cm_de[3, 4]), 0.0158, tolerance = 1e-3)

  # Check matrix consistency
  expect_true(all(is.finite(as.matrix(cm_de[-1]))))
  expect_true(all(cm_de[-1] >= 0))
})
