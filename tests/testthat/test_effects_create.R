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



