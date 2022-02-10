context ("Getting the year from a SIOT")
germany_years <- iotable_year_get ( source = "germany_1995", geo = 'DE', 
                                     unit = "MIO_EUR")

test_that("get_year_iotable errors", {
  expect_error(iotable_year_get(source = "germany_1995", 
                                geo = 'DE', unit = "MIO_NAC"))
  
 })

test_that("correct data is returned", {
  expect_equal(iotable_year_get(source = "germany_1995", 
                           geo = 'DE', unit = "MIO_EUR"),
               as.Date('1990-01-01'))
  
  })

