library (testthat)
library (iotables)
context ("Creating forward linkages")

io_table <- iotable_get () 
io_table <- io_table [1:which(tolower(io_table[,1]) =="total" ), ]

output_bp <- dplyr::select ( io_table, output_bp )
io_table <- io_table [, 1:7] 
io_table$total <- rowSums(io_table[, 2:7])
io_table <- cbind (io_table, output_bp)

de_out <- output_coefficient_matrix_create ( io_table = io_table, 
                                    type = 'final_demand',
                                    digits = 4)

fw <- forward_linkages ( output_coefficient_matrix = de_out, 
                   digits = 4 )

#The Eurostat Manual uses a different rounding. There is a slight mismatch)

test_that("correct data is returned", {
  expect_equal(fw$forward,
               c(2.1126, 1.6691, 1.3558, 1.5848, 2.1037,1.2106),
               tolerance=1e-2)
})
