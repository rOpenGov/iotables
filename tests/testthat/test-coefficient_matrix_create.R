
cm_de <-  coefficient_matrix_create ( 
  data_table = iotable_get ( source = "germany_1995"), 
  total = "output", 
  digits = 4 )

# The Eurostat Manual uses a different rounding. There is a slight mismatch.

test_that("correct data is returned by coefficient_matrix_create()", {
  expect_equal(as.numeric(unlist (cm_de[1, 2] )), 
               c(0.0258), tolerance=1e-3)
  expect_equal(nrow(coefficient_matrix_create( iotable_get(), return = "primary_inputs" )), 13 )
})

test_that("Households are treated correctly in coefficient_matrix_create", {
  expect_equal(ncol(coefficient_matrix_create ( data_table = iotable_get ( source = "germany_1995"), 
                                                total = "output",
                                                households = TRUE,
                                                digits = 4 )), 
               8)
  expect_equal(ncol(coefficient_matrix_create ( data_table = iotable_get ( source = "germany_1995"), 
                                                total = "output",
                                                digits = 4 )), 
               7)
   })




