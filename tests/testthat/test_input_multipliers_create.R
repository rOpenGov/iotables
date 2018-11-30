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

de_use <- use_table_get()
de_output <- output_get()
de_icoeff <- input_coefficient_matrix_create( de_use, de_output )

L_de  <- leontieff_matrix_create( de_icoeff )
I_de <- leontieff_inverse_create( L_de )


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



###Netherlands

nl_use_2 <- use_table_get  ( labelled_io_table = netherlands_2006,
                             source = 'custom', households = TRUE)
nl_output_2 <- output_get ( labelled_io_table = netherlands_2006, households = TRUE )

nl_coeff_2   <- input_coefficient_matrix_create( input_flow = nl_use_2,
                                               output = nl_output_2, 
                                               digits = NULL)

nl_coeff_2$final_consumption_households <- c(0, 0, 0.08, 0.03, 0, 0.59, 0) #for the time being

View  (nl_coeff_2)
L_nl_2 <- leontieff_matrix_create( technology_coefficients_matrix =
                                   nl_coeff_2 )
I_nl_2 <- leontieff_inverse_create(L_nl_2)

households <- data.frame ( 
  final_consumption_households = 0)

gva_nl_2 <- cbind(nl[12,1:7], households)
income_nl_2 <- cbind(nl[11,1:7], households)

gva_indicator_nl_2   <- input_indicator_create( input_matrix = gva_nl_2, 
                                              output_vector = nl_output_2)

income_indicator_nl_2   <- input_indicator_create( input_matrix = income_nl_2, 
                                                 output_vector = nl_output_2)

income_nl_effects_2 <- multiplier_create ( 
                 input_vector = income_indicator_nl_2,
                 Im = I_nl_2, 
                 multiplier_name = 'income_multiplier_2')

View ( income_nl_effects_2)

income_effects_2_published <- c(0.3887,
 0.1456,
 0.4515,
 0.3124,
 0.6858,
 0.7279)

income_multipliers_2_published  <- c(
   3.6377,
   3.4423,
   2.7144,
   3.4996,
   2.8813,
   2.0907)

nl_income_multiplier_2 <- income_nl_multipliers_2[ , 2:7] /income_indicator_nl_2[ , 2:7]

test_that("correct data is returned from netherlands_2006", {
  expect_equal(round(as.numeric (I_nl_2[2,2:8]),3), 
               c(0.024, 1.028, 0.024, 0.300, 0.018, 0.014, 0.019), tolerance=1e-3)
  expect_equal(as.numeric(income_nl_effects_2[,2:7]), 
               income_effects_2_published, 
               tolerance=1e-3)
  expect_equal(as.numeric(nl_income_multiplier_2), 
               income_multipliers_2_published, 
               tolerance=1e-3)
})



