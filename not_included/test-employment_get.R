library (testthat)
library (iotables)
context ("Retrieveing the employment satellite accounts")

data(germany_1990)
employment_vector <- employment_get(labelled_io_data = germany_1990, geo = "DE",
                                    year  = "1990", named = TRUE)

#Eurostat manual Table 15.3, p. 481
AAE = employment_vector[, "CPA_agriculture"]

test_that("input_flow_create input and output works", {
  expect_equal(employment_vector[, "CPA_agriculture"],expected= 1096,
               tolerance = .001) 
  expect_equal(employment_vector[, "CPA_trade"],expected=  9251,
               tolerance = .001)
   })
