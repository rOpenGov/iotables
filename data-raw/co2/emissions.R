
data(germany_1995)
germany_io <- iotable_get (source= "germany_1995", labelling = 'iotables' )
data_table <- germany_airpol %>%
  filter ( .data$airpol == "CO2") %>%
  select ( .data$iotables_col, .data$value) 

col_names <- names(data_table)

co2_emission <- pivot_wider(data_table, 
                            names_from = col_names[1] ) %>%
  bind_cols ( tibble ( iotables_row = col_names[1])) %>%
  relocate ( .data$iotables_row, .before = everything())

co2_emission
de_input_coeff <- input_coefficient_matrix_create( 
  data_table = germany_io, 
  digits = 4)

output_de <- output_get(germany_io) 

emission_coeff <- cbind ( data.frame (iotables_row = "emission_coefficients"), 
                          co2_emission[, 2:7] / output_de[,2:7] )

emission_coeff 

data.frame ( iotables_row = "emission_coefficients", 
             agriculture_group =  0.2379, 
             industry_group    =  0.5172, 
             trade             = 0.0456)

input_requirements = emission_coeff
input_multipliers_create(input_requirements = emission_coeff, inverse = I_de)
names(emission_coeff)
names ( output_de)

I_de   <- leontief_inverse_create(de_input_coeff)

emissions_de <- germany_airpol %>%
  select ( -.data$induse ) %>%
  vector_transpose_wider( names_from = "iotables_col", 
                          values_from = "value")


emissions_de <- germany_airpol[, -3] %>%
  vector_transpose_wider( names_from = "iotables_col", 
                          values_from = "value")

output_bp <- output_get ( iotable_get() )




coeffs <- output_coefficients_create(data_table = emissions_de, output = output_bp)

names(coeffs)[1] <- names(I_de)[1]

bind_cols (
  key_column_create(names(coeffs)[1], 
                    gsub("_coefficient", "_multiplier", unlist(coeffs[,1]))), 
  do.call( rbind, lapply ( 1:nrow(coeffs), function(x) equation_solve (coeffs[x, ], I_de) )) %>%
    as_tibble()
)




