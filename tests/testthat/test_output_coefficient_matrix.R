library (testthat)
library (iotables)
require (dplyr)
context ("Creating an  output coefficient matrix")

io_table <- iotable_get () 
io_table <- io_table [1:which(tolower(io_table[,1]) =="total" ), ]

output_bp <- dplyr::select ( io_table, output_bp )
io_table <- io_table [, 1:7] 
io_table$total <- rowSums(io_table[, 2:7])
io_table <- cbind (io_table, output_bp)

test_that("wrong paramter error", {
  expect_error(output_coefficient_matrix_create (
    io_table, type = "wrong", digits = 1))
  })


output_coefficients <- output_coefficient_matrix_create (
                           io_table, "final_demand", digits = 4)

business_agriculture_input <- output_coefficients %>%
  dplyr::filter ( iotables_row == "business_services_group") %>%
  dplyr::select ( agriculture_group) %>%
  as.numeric(.)

BSBS <- output_coefficients %>%
  dplyr::filter ( iotables_row == "business_services_group") %>%
  dplyr::select ( business_services_group ) %>%
  as.numeric(.)

test_that("correct output coefficients are returned", {
  expect_equal(business_agriculture_input, 0.0053, tolerance = .00005)
  expect_equal(BSBS, 0.2790, tolerance = .00005)
  })
