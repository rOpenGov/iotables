library (testthat)
library (iotables)
context ("Input indicator create")

de_emp <- primary_input_get ( input = "compensation_employees",
                              source = "germany_1990", geo = "DE",
                              year = 1990, unit = "MIO_EUR", 
                              households = FALSE, labelling = "iotables")

de_output <- output_get ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          households = FALSE, labelling = "iotables")


test_that("correct data is returned", {
  expect_equal(input_indicator_create(de_emp, de_output, digit = 4)[1,2], 
               expected = 0.21, tolerance = .004)
})

##needs to be checked again with better examples. 


