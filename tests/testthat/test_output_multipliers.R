library (testthat)
library (iotables)
context ("Creating output multipliers")

de_input_coeff <- input_coefficient_matrix_create( 
                           de_use, de_output, digits = 4)
                           
output_multipliers <- output_multiplier_create ( de_input_coeff )

test_that("correct data is returned", {
  expect_equal(as.numeric ( output_multipliers[, 2:ncol(output_multipliers)] ),
               c(1.7048, 1.8413, 1.8136, 1.6035, 1.5951, 1.3782),
               tolerance=1e-2)
})
