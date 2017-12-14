library (testthat)
library (iotables)
context ("Creating the Leontieff matrix and its inverse")

de_use <- use_table_get ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          households = FALSE, labelling = "short")

de_output <- output_get ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          households = FALSE, labelling = "short")

names (de_use)[1] <- names(de_output)[1]
de_coeff <- input_coefficient_matrix_create( input_flow = de_use, 
                                             output =  de_output, digits = 4)

L <- leontieff_matrix_create( technology_coefficients_matrix =
                                          de_coeff )
I <- leontieff_inverse_create(L)

AAL <- L %>%
  filter ( t_rows2 ==  "agriculture_group" ) %>%
  select (  agriculture_group ) %>%
  as.numeric(.)

#test against 15.9 p 487

TBI <- I %>%
  filter ( t_rows2 ==  "trade_group" ) %>%
  select (  business_services_group ) %>%
  as.numeric(.)

#test against 15.10 p 488

test_that("Leontieff matrix values are correct", {
  expect_equal(AAL,expected= 0.9742, tolerance = .0001)
   })

test_that("Leontieff inverse values are correct", {
  expect_equal(TBI,expected= 0.035494905, tolerance = .0001)
  expect_equal(sum(sapply(I, function(x) sum(is.nan(x)))), expected = 0) #should not be NaNs
})

