
om <- output_coefficient_matrix_create( 
  data_table = iotable_get(), 
  total = 'tfu'
  )

G <- ghosh_inverse_create( output_coefficients_matrix = om )

test_that("The agriculture column of the Ghosh inverse matches with the Eurostat Manual", {
  expect_true(all (round(G$agriculture_group,4) == c(1.0339, 0.0118, 0.0037, 0.0103, 0.0117, 0.0043)))
})
