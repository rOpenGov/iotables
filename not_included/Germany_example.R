library ( iotables)
data (germany_1990)
input_flow <- input_flow_get  ( labelled_io_data = germany_1990, 
                                technology = NULL, year = 1990, 
                                geo = "DE",  unit = "M_EUR", named = TRUE ) 
output_vector <- output_get ( germany_1990, geo = 'DE', year = 1990, 
                              unit = "M_EUR", named = TRUE)
output_coefficients <- output_coefficient_matrix_create(
  input_flow, output_vector, digits = 4)
input_coefficients_de <- input_coefficient_matrix_create(
  input_flow, output_vector, digits = 4)

value_added_de <- input_value_added_get( labelled_io_data = germany_1990, 
      technology = NULL, geo = "DE", 
      year = 1990, unit = "M_EUR", named = TRUE) 

L_de <- leontieff_matrix_create(input_coefficients_de)
I_de <- leontieff_inverse_create(L_de)
