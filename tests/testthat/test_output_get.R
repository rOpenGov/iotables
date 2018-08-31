library (testthat)
library (iotables)
context ("Creating an output vector")

output_get(source = "germany_1990", 
           geo = 'DE', year = 1990, unit = "MIO_EUR")

test_that("output_get errors ", {
  expect_error(output_get(source = "germany_1990", 
                              geo = 'DE', year = 1990, unit = "MIO_NAC")) 
  expect_error(output_get(source = "germany_1990", 
                              geo = 'DE', year = 1787, unit = "MIO_EUR")) 
  expect_error(output_get(source = "germany_1990", 
                              geo = 'BE', year = 1990, unit = "MIO_EUR"))
  #expect_warning(use_table_get(source = "germany_1990", 
  #                           geo = 'de', year = 1990, unit = "MIO_EUR"))
  expect_error(output_get(source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "biotables"))
  expect_error(output_get(source = "croatia_2010_1900", 
                          geo = 'HR', year = 2010, unit = "T_NAC"))  #this is an import table
})

test_that("correct data is returned", {
  expect_equal(as.character(output_get(source = "germany_1990", 
                          geo = 'DE', year = 1990, 
                          unit = "MIO_EUR", 
                          labelling = "iotables")[[1]][1]), "output_bp")
  expect_equal(as.numeric(output_get(source = "germany_1990", 
                 geo = 'DE', year = 1990, 
                 unit = "MIO_EUR", households = TRUE, 
                 labelling = "iotables")[1,8]), 1001060)
  })

