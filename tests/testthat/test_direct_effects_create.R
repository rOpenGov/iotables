library (testthat)

require (dplyr)
context ("Creating an  input coefficient matrix")

nl <- netherlands_2006

input_coeff_nl <- input_coefficient_matrix_create(
  data_table  = netherlands_2006, 
  households = FALSE) 

compensation_indicator <- input_indicator_create(netherlands_2006, 'compensation_employees')

I_nl <- leontieff_inverse_create( input_coeff_nl )

dir <- direct_effects_create(input_requirements = compensation_indicator, 
                             inverse = I_nl)
published_effects <- c(0.263, 0.099, 0.306, 0.212, 0.465, 0.493)


test_that("get_input_flow correct input coefficients are returned", {
  expect_equal(as.numeric(dir[2:7]), published_effects, 
               tolerance = .0005)
})


