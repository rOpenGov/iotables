library (testthat)
library (iotables)
context ("Creating an empty conforming vector")

de_input_flow <- input_flow_get ( data_table = iotable_get())

conforming_vector_create (de_input_flow)

test_that("Dimension and names are correct", {
  expect_equal(ncol(de_input_flow), 
               ncol(conforming_vector_create (de_input_flow))
  )
  expect_equal(names(de_input_flow), 
               names(conforming_vector_create (de_input_flow))
  )
})