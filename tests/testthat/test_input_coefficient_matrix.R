library (testthat)
library (iotables)
require (dplyr)
context ("Creating an  input coefficient matrix")

de_use <- use_table_get ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          households = FALSE, labelling = "iotables")

nl <- netherlands_2006
nl_use <- use_table_get ( labelled_io_table = nl, source  = 'custom')

de_misuse <- de_use
names(de_misuse)[2] <- 'wrong_name'

test_that("nonconforming input error works ", {
  expect_error(input_coefficient_matrix_create (
    de_misuse, de_output, digits = 1))
  })

de_output <- output_get ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          households = FALSE, labelling = "iotables")

nl_output <- output_get ( labelled_io_table = nl )

input_flow = de_use
output = de_output 
digits = 5

nl_coeff   <- input_coefficient_matrix_create( input_flow = nl_use,
                                               output = nl_output, 
                                               digits = 2)

nl_services_row <- nl_coeff %>%
  dplyr::filter ( prod_na == "services_group" ) %>%
  dplyr::select ( -prod_na ) %>%
  unlist(.) %>% as.numeric (.)

input_coefficients <- input_coefficient_matrix_create (
                           de_use, de_output, digits = 5)

business_agriculture_input <- input_coefficients %>%
  dplyr::filter ( iotables_row == "business_services_group") %>%
  dplyr::select ( agriculture_group) %>%
  as.numeric(.)

BSBS <- input_coefficients %>%
  dplyr::filter ( iotables_row == "business_services_group") %>%
  dplyr::select ( business_services_group ) %>%
  as.numeric(.)

test_that("get_input_flow correct input coefficients are returned", {
  expect_equal(business_agriculture_input, 0.0828, tolerance = .00005)
  expect_equal(BSBS, 0.279, tolerance = .00005)
  expect_equal(nl_services_row, c(
    0.13, 0.09, 0.13, 0.08, 0.16, 0.25))
  })
