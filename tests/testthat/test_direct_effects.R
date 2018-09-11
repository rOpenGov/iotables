library (testthat)
library (iotables)
library (dplyr)

context ("Creating direct effects")
io_table <- iotable_get () 
#Total column should not be missing
io_table <- io_table [, 1:7] 
io_table$total <- rowSums(io_table[, 2:7])

labelled_io_table <- io_table

direct_effects <- direct_effects_create ( labelled_io_table = io_table ) 

value_added_row <- direct_effects [ direct_effects[,1] == "gva_bp", ]

emp_row <- which ( direct_effects[,1] == "compensation_employees")
agr_col <- which ( names(direct_effects) == "agriculture_group")

agr_employees <- direct_effects[emp_row, agr_col ]


test_that("correct data is returned", {
  expect_equal(as.numeric (agr_employees), 0.2137, tolerance=1e-3)
  expect_equal(as.numeric(value_added_row[2:7]), 
               c(0.4934,0.3659,0.4708,0.5766,0.5999,0.7172), 
               tolerance=1e-3)
})
