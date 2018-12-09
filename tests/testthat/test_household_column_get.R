library (testthat)
library (iotables)
context ("Returning the household final expenditure column")

household_column_find( iotable_get ( source = 'germany_1990') )

households <- household_column_get( iotable_get ( source = 'germany_1990') )

test_that("correct data is returned", {
  expect_equal(as.numeric(unlist (households[1:2, 2] )), 
               c(8500,197792), tolerance=1e-3)
})





