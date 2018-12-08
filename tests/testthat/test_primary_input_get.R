library (testthat)
library (iotables)
context ("Retreieving a primary input vector")


test_that("correct data is returned", {
  expect_equal(primary_input_get(siot = iotable_get ( source = "germany_1990", 
                                                      geo = 'DE', year = 1990, 
                                                      unit = "MIO_EUR",
                                                      labelling = "short"),
                                 primary_input = "d1")[,3], 296464 )
})



