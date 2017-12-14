library (testthat)
library (iotables)
context ("Creating a use table")

test_that("use_table_get errors are correct", {
  expect_error(use_table_get(source = "germany_1990", 
                              geo = 'DE', year = 1990, unit = "MIO_NAC")) 
  expect_error(use_table_get(source = "germany_1990", 
                              geo = 'DE', year = 1787, unit = "MIO_EUR")) 
  expect_error(use_table_get(source = "germany_1990", 
                              geo = 'BE', year = 1990, unit = "MIO_EUR"))
  #expect_warning(use_table_get(source = "germany_1990", 
  #                           geo = 'de', year = 1990, unit = "MIO_EUR"))
  expect_error(use_table_get(source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "biotables"))
})

test_that("correct data is returned", {
  expect_equal(use_table_get(source = "germany_1990", 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "iotables")[1,2], 1131)
  expect_equal(as.character(use_table_get(source = "croatia_2010_1900", geo = "HR",
                            year = 2010, unit = "T_NAC", labelling = 'iotables')[4,1]), "mining")
  expect_equal(as.character(use_table_get(source = "croatia_2010_1900", geo = "HR",
                                          year = 2010, unit = "T_NAC", labelling = 'short')[4,1]), "CPA_B")
  })


