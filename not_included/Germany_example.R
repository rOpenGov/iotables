library ( iotables)
data (germany_1990)
de_use <- use_table_get ( source = "germany_1990", geo = "DE",
                                         year = 1990, unit = "MIO_EUR", 
                                         households = FALSE, labelling = "iotables")

de_output <- output_get ( source = "germany_1990", geo = "DE",
                                    year = 1990, unit = "MIO_EUR",
                                    households = FALSE, labelling = "iotables")

de_coeff <- input_coefficient_matrix_create( de_use, de_output, digits = 4)

value_added_de <- input_value_added_get( labelled_io_data = germany_1990, 
      technology = NULL, geo = "DE", 
      year = 1990, unit = "M_EUR", named = TRUE) 

L_de <- leontieff_matrix_create(input_coefficients_de)
I_de <- leontieff_inverse_create(L_de)
