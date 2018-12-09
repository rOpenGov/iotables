library (testthat)
library (iotables)
context ("Creating forward linkages")

de_out <- output_coefficient_matrix_create ( 
  data_table, "final_demand", digits = 4
)

fw <- forward_linkages ( output_coefficient_matrix = de_out, 
                   digits = 4 )

#The Eurostat Manual uses a different rounding. There is a slight mismatch)

test_that("correct data is returned", {
  expect_equal(fw$forward,
               c(2.1126, 1.6691, 1.3558, 1.5848, 2.1037,1.2106),
               tolerance=1e-2)
})
