library (testthat)
library (iotables)
context ("Retreieving a primary input vector")

siot <- iotable_get ( source = "germany_1990", 
                      geo = 'DE', year = 1990, 
                      unit = "MIO_EUR",
                      labelling = "short")

test_that("correct data is returned", {
  expect_equal(primary_input_get(data_table = siot,
                                 primary_input = "D1")[,3], 296464 )
})




