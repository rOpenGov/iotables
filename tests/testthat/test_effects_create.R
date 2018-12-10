library (testthat)
library (iotables)
context ("Retreieving a primary input vector")

de_coeff <- input_coefficient_matrix_create( iotable_get(), 
                                             digits = 4 )
L_de <- leontieff_matrix_create( technology_coefficients_matrix =
                                   de_coeff )
I_de <- leontieff_inverse_create(technology_coefficients_matrix =
                                   de_coeff)

de_emp <- primary_input_get ( data_table = iotable_get(),
                              primary_input = "employment_domestic_total" )

de_emp_indicator <- input_indicator_create (iotable_get(), "employment_domestic_total" , digits = 5)

employment_effects <- effects_create ( 
  input_vector    = de_emp_indicator,
  Im              = I_de,
  effect_name  = "employment_effect", 
  digits = 4 )


test_that("correct data is returned", {
  expect_equal( employment_effects$agriculture_group ,0.0326 )
  })

nl_use_2 <- input_flow_get  ( netherlands_2006,
                              households = TRUE)

nl_coeff_2   <- input_coefficient_matrix_create( netherlands_2006,
                                                 households = TRUE, 
                                                 digits = NULL)

nl_coeff_2$final_consumption_households <- c(0, 0, 0.08, 0.03, 0, 0.59, 0) #for the time being


L_nl_2 <- leontieff_matrix_create( technology_coefficients_matrix =
                                     nl_coeff_2 )
I_nl_2 <- leontieff_inverse_create(technology_coefficients_matrix =
                                     nl_coeff_2)


gva_indicator_nl_2   <- input_indicator_create(netherlands_2006, 
                                               "value_added_bp", 
                                               households = TRUE)

income_indicator_nl_2 <- input_indicator_create(netherlands_2006, 
                                                "compensation_employees", 
                                                households = TRUE)

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

