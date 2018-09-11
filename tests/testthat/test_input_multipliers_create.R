library (testthat)
library (iotables)
library (dplyr)

context ("Creating input multipliers")
io_table <- iotable_get () 
#Total column should not be missing
io_table <- io_table [, 1:7] 
io_table$total <- rowSums(io_table[, 2:7])

labelled_io_table <- io_table

direct_effects_de <- direct_effects_create ( labelled_io_table = io_table ) 
direct_effects <- direct_effects_de [, -8] 

multipliers <- input_multipliers_create(
  direct_effects = direct_effects_de [, -8],
  inverse = I_de, 
  labelled = TRUE)

value_added_row <- multipliers [ direct_effects[,1] == "gva_bp", ]

emp_row <- which ( multipliers[,1] == "compensation_employees")
agr_col <- which ( names(multipliers) == "agriculture_group")

agr_employees <- multipliers [emp_row, agr_col ]


test_that("correct data is returned", {
  expect_equal(as.numeric (agr_employees), 0.4172, tolerance=1e-3)
  expect_equal(as.numeric(value_added_row[2:7]), 
               c(0.845,0.7647,0.8615, 0.9019,0.9393, 0.9199), 
               tolerance=1e-3)
})
