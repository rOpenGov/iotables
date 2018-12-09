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

de_gva <- primary_input_get ( data_table,
                              primary_input = "gva") 

de_gva_indicator  <- input_indicator_create( 
          data_table  = data_table,
          input_vector = "gva", 
          digits = 4)

test_that("correct data is returned", {
  expect_equal(as.numeric(de_gva_indicator[2:7]), 
               expected = c(0.4934, 0.3659, 0.4708, 0.5766, 0.5999, 0.7172),
               tolerance = .004)
})
#Eurostat manual p498


