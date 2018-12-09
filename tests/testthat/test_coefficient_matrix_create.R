library (testthat)
library (iotables)
context ("Creating forward linkages")

cm_de <-  coefficient_matrix_create ( siot = iotable_get ( source = "germany_1990"), 
                             total = "output", 
                             digits = 4 )

ncol(coefficient_matrix_create ( siot = iotable_get ( source = "germany_1990"), 
                            total = "output", 
                            digits = 4 ))



#The Eurostat Manual uses a different rounding. There is a slight mismatch)

test_that("correct data is returned", {
  expect_equal(as.numeric(unlist (cm_de[1, 2] )), 
               c(0.0258), tolerance=1e-3)
  expect_equal(nrow(coefficient_matrix_create( io_table, return = "primary_inputs" )), 13 )
})

test_that("households are treated correctly", {
  expect_equal(ncol(coefficient_matrix_create ( siot = iotable_get ( source = "germany_1990"), 
                                                total = "output",
                                                households = TRUE,
                                                digits = 4 )), 
               8)
  expect_equal(ncol(coefficient_matrix_create ( siot = iotable_get ( source = "germany_1990"), 
                                                total = "output",
                                                digits = 4 )), 
               7)
  expect_equal(ncol(coefficient_matrix_create ( siot = iotable_get ( source = "germany_1990"), 
                                                total = "output",
                                                households = NULL,
                                                digits = 4 )), 
               7)
  
  })




