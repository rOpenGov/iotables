library (testthat)
library (iotables)
context ("Retreieving a primary input vector")

de_use <- use_table_get ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          households = FALSE, labelling = "iotables")

de_output <- output_get ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR",
                          households = FALSE, labelling = "iotables")

de_coeff <- input_coefficient_matrix_create( input_flow = de_use,
                                             output = de_output, 
                                             digits = 4 )
L_de <- leontieff_matrix_create( technology_coefficients_matrix =
                                   de_coeff )
I_de <- leontieff_inverse_create(L_de)

de_emp <- primary_input_get ( input = "employment_total",
                              source = "germany_1990", geo = "DE",
                              year = 1990,  
                              households = FALSE, labelling = "iotables")

de_emp_indicator <- input_indicator_create (de_emp, de_output, digits = 5)

employment_effects <- effects_create ( 
  input_vector    = de_emp_indicator,
  Im              = I_de,
  effect_name  = "employment_effect", 
  digits = 4 )


test_that("correct data is returned", {
  expect_equal( employment_effects$agriculture_group ,0.0326 )
  })

nl_use_2 <- use_table_get  ( labelled_io_table = netherlands_2006,
                             source = 'custom', households = TRUE)
nl_output_2 <- output_get ( labelled_io_table = netherlands_2006, households = TRUE )

nl_coeff_2   <- input_coefficient_matrix_create( input_flow = nl_use_2,
                                                 output = nl_output_2, 
                                                 digits = NULL)

nl_coeff_2$final_consumption_households <- c(0, 0, 0.08, 0.03, 0, 0.59, 0) #for the time being


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

income_nl_effects_2 <- effects_create ( 
  input_vector = income_indicator_nl_2,
  Im = I_nl_2, 
  effect_name = 'income_effects_2')

income_effects_2_published <- c(0.3887,
                                0.1456,
                                0.4515,
                                0.3124,
                                0.6858,
                                0.7279)

