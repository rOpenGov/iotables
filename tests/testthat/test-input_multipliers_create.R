library (testthat)

context ("Creating input multipliers")

nl <- netherlands_2006

input_coeff_nl <- input_coefficient_matrix_create(
  data_table  = netherlands_2006, 
  households = FALSE) 

compensation_indicator <- input_indicator_create(netherlands_2006, 'compensation_employees')

I_nl <- leontieff_inverse_create(input_coeff_nl)
mult <- input_multipliers_create(
                      input_requirements = compensation_indicator, 
                      Im = I_nl)

published_multipliers <- c(2.466, 2.333, 1.84, 2.372, 1.953, 1.417 )


test_that("get_input_flow correct input coefficients are returned", {
  expect_equal(as.numeric(mult[2:7]), published_multipliers, 
               tolerance = .0005)
})


