library (testthat)
library (iotables)
context ("Input indicator create")

data_table = iotable_get()

test_that("correct data is returned", {
  expect_equal(input_indicator_create( data_table = iotable_get(), 
                                      input_vector = "compensation_employees",
                                      digits = 4)[1,2], 
               expected = 0.21, tolerance = .004)
})

##needs to be checked again with better examples. 


