nl <- netherlands_2006
input_coeff_nl <- input_coefficient_matrix_create(
 data_table  = netherlands_2006, 
 households = FALSE) 
compensation_indicator <- input_indicator_create(netherlands_2006, 'compensation_employees')
I_nl <- leontief_inverse_create(input_coeff_nl)

test_that("indirect_effects_create works", {
  expect_equal(names(indirect_effects_create(input_requirements = compensation_indicator, 
                                             inverse = I_nl))[1], "prod_na")
})
