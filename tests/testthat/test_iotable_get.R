library (testthat)
library (iotables)
context ("Creating an IO Table")

test_that("get_iotable errors ", {
  expect_error(iotable_get(source = "germany_1990", 
                              geo = 'DE', year = 1990, unit = "MIO_NAC")) 
  expect_error(iotable_get(source = "germany_1990", 
                              geo = 'DE', year = 1787, unit = "MIO_EUR")) 
  expect_error(iotable_get(source = "germany_1990", 
                              geo = 'BE', year = 1990, unit = "MIO_EUR"))
  expect_warning(iotable_get(source = "germany_1990", 
                             geo = 'de', year = 1990, unit = "MIO_EUR", 
                             labelling = "short"))
  expect_error(iotable_get(source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "biotables") )
})

test_that("correct data is returned", {
  expect_equal(iotable_get(source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "iotables")[1,2], 1131)
  expect_equal(as.character(iotable_get(source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "short")[4,1]), "cpa_g_i")
  })

#Slovakia A01, A01 shoud be 497.37

