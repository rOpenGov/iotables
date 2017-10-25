library (testthat)
library (iotables)
context ("Retreieving a primary input vector")

data(germany_1990)

test_that("get_iotable errors ", {
  expect_error(primary_input_get(input = "b2n_b3n", source = "germany_1990", 
                              geo = 'DE', year = 1990, unit = "MIO_NAC")) 
  expect_error(primary_input_get(input = "D1", source = "germany_1990", 
                                 geo = 'DE', year = 1990, unit = "MIO_NAC")) #case sensitive
  expect_error(primary_input_get(input = "b2n_b3n", source = "germany_1990", 
                              geo = 'DE', year = 1787, unit = "MIO_EUR")) 
  expect_error(primary_input_get(input = "b2n_b3n", source = "germany_1990", 
                              geo = 'BE', year = 1990, unit = "MIO_EUR"))
  #expect_warning(use_table_get(source = "germany_1990", 
  #                           geo = 'de', year = 1990, unit = "MIO_EUR"))
  expect_error(primary_input_get(input = "b2n_b3n", source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "biotables"))
  expect_error(primary_input_get(input = "false_input", source = "germany_1990", 
                                 geo = 'DE', year = 1990, 
                                 unit = "MIO_EUR", labelling = "MIO_EUR"))
})

test_that("correct data is returned", {
  expect_equal(primary_input_get(input = "d1", source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "iotables")[1,2], 9382)
  expect_equal(as.character(primary_input_get(input = "d1", source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "short")[1,1]), "d1")
  expect_equal(primary_input_get(input = "b2n_b3n", source = "germany_1990", 
                 geo = 'DE', year = 1990, 
                 unit = "MIO_EUR", households = TRUE, 
                 labelling = "iotables")[1,8], 0)
  })



