library (testthat)
library (iotables)
context ("Retreieving a primary input vector")

test_that("primary_input_get errors ", {
  expect_error(primary_input_get(input = "b2n_b3n", source = "germany_1990", 
                              geo = 'DE', year = 1990, unit = "MIO_NAC")) 
  expect_error(primary_input_get(input = "D1", source = "germany_1990", 
                                 geo = 'DE', year = 1990, unit = "MIO_NAC")) #case sensitive
  expect_error(primary_input_get(input = "b2n_b3n", source = "germany_1990", 
                              geo = 'DE', year = 1787, unit = "MIO_EUR")) 
  expect_error(primary_input_get(input = "b2n_b3n", source = "germany_1990", 
                              geo = 'BE', year = 1990, unit = "MIO_EUR"))
  expect_error(primary_input_get(input = "b2n_b3n", source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "biotables"))
  expect_error(primary_input_get(input = "false_input", source = "germany_1990", 
                                 geo = 'DE', year = 1990, 
                                 unit = "MIO_EUR", labelling = "MIO_EUR"))
})

test_that("correct data is returned", {
  expect_equal(as.numeric(primary_input_get(input = "compensation_employees", 
                   source = "croatia_2010_1700", geo = "HR",unit = "T_NAC", 
                   year = 2010, households = TRUE)[1,68]),0) 
  expect_equal(as.numeric(primary_input_get(input = "d1", source = "germany_1990", 
                                 geo = 'DE', year = 1990, 
                                 unit = "MIO_EUR", households = TRUE, 
                                 labelling = "short")[1,3]),296464 )
  expect_equal(as.numeric(primary_input_get(input = "compensation_employees", source = "germany_1990", 
                                 geo = 'DE', year = 1990, 
                                 unit = "MIO_EUR", households = TRUE, 
                                 labelling = "iotables")[1,3]),296464 ) #alternate labelling
  })



