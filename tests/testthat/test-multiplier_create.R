test_that("Eurostat manual (§5.0.1): DE multipliers match published", {
  data_table <- iotable_get()

  coeff_de <- input_coefficient_matrix_create(data_table)

  de_gva_indicator <- input_indicator_create(
    data_table = data_table,
    input = "gva"
  )

  I_de <- leontief_inverse_create(coeff_de)

  de_emp_indicator <- input_indicator_create(
    data_table = data_table,
    input = "employment_domestic_total"
  )

  de_gva_multipliers <- multiplier_create(
    input_vector = de_gva_indicator,
    Im = I_de,
    multiplier_name = "employment_multiplier",
    digits = 4
  )

  gva_de_published <- c(0.8450, 0.7647, 0.8615, 0.9019, 0.9393, 0.9199)

  employment_multipliers <- multiplier_create(
    input_vector = de_emp_indicator,
    Im = I_de,
    multiplier_name = "employment_multiplier",
    digits = 4
  )

  emp_de_published <- c(0.0326, 0.0162, 0.0207, 0.0237, 0.0112, 0.0242)

  expect_equal(
    object = as.numeric(de_gva_multipliers[2:7]),
    expected = gva_de_published,
    tolerance = 1e-3
  )

  expect_equal(
    object = as.numeric(employment_multipliers[2:7]),
    expected = emp_de_published,
    tolerance = 1e-3
  )
})





later <- function() {
  nl_use_2 <- input_flow_get(netherlands_2006,
    households = TRUE
  )

  nl_coeff_2 <- input_coefficient_matrix_create(netherlands_2006,
    households = TRUE,
    digits = NULL
  )

  nl_coeff_2$final_consumption_households <- c(0, 0, 0.08, 0.03, 0, 0.59, 0) # for the time being


  L_nl_2 <- leontief_matrix_create(
    technology_coefficients_matrix =
      nl_coeff_2
  )
  I_nl_2 <- leontief_inverse_create(
    technology_coefficients_matrix =
      nl_coeff_2
  )


  gva_indicator_nl_2 <- input_indicator_create(netherlands_2006,
    "value_added_bp",
    households = TRUE
  )

  income_indicator_nl_2 <- input_indicator_create(netherlands_2006,
    "compensation_employees",
    households = TRUE
  )

  income_nl_multipliers_2 <- multiplier_create(
    input_vector = income_indicator_nl_2[1, 1:7],
    Im = I_nl_2,
    multiplier_name = "income_multipliers_2"
  )

  income_multipliers_2_published <- c(
    3.6377,
    3.4423,
    2.7144,
    3.4996,
    2.8813,
    2.0907
  )



  test_that("correct data is returned from netherlands_2006", {
    expect_equal(as.numeric(income_nl_multipliers_2)[1:6],
      income_multipliers_2_published,
      tolerance = 1e-3
    )
  })
}
