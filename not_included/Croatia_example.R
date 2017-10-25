##Croatia example
library(iotables) ; library (dplyr) ; library (tidyr)

labelled_io_table <- croatia_2010_1700

tech_hr <- get_technology_data(labelled_io_data = croatia_2010_1700)

input_flow_hr <- input_flow_get(labelled_io_table, 
                             technology = tech_hr, geo = 'HR', 
                             unit = "T_NAC") %>%
  tidyr::gather ( t_cols2, values, !! 2:ncol(.) ) %>%
  tidyr::spread ( t_cols2, values )


output_vector_hr <- output_get(labelled_io_table, technology = tech_hr, 
                            geo = 'HR', unit = "T_NAC", named = TRUE)
data ("croatia_employment_2013")
data ("croatia_employment_aggregation")

employment_vector <- employment_aggregate (
  employment_df = croatia_employment_2013,
  matching = croatia_employment_aggregation ) 

employment_indicator_hr <- iotables::input_indicator_create (
  employment_vector, output_vector_hr, digits = 4)

input_coefficients <- iotables::input_coefficient_matrix_create(
  input_flow_hr, output_vector_hr, digits = 4)

L_hr <- iotables::leontieff_matrix_create( technology_coefficients_matrix =
                                          input_coefficients)

I_hr <- iotables::leontieff_inverse_create(L_hr)

employment_multipliers_hr <- multiplier_create ( 
  input_vector  =   employment_indicator_hr, Im = I_hr, digits = 4 ) %>%
  tidyr::gather ( t_cols2, values, !! 2:ncol(.)) %>%
  mutate ( values = values * 1000 )

value_added_hr <- input_value_added_get( labelled_io_data = croatia_2010_1700, 
                           technology = tech_hr, geo = "HR", 
                           year = 2010, unit = "T_NAC", named = TRUE) 

va_indicator_hr <- input_indicator_create (
  value_added_hr, output_vector_hr, digits = 4) 

va_multiplier_hr <- multiplier_create ( 
  input_vector  =   va_indicator_hr, Im = I_hr, digits = 4 ) %>%
  tidyr::gather ( t_cols2, values, !! 2:ncol(.))  %>%
  mutate ( group = 'services') %>%
  mutate ( group = ifelse(grepl("CPA_A", t_cols2), "agriculture", group )) %>%
  mutate ( group = ifelse(grepl("CPA_B", t_cols2), "mining", group )) %>%
  mutate ( group = ifelse(grepl("CPA_C", t_cols2), "manufacturing", group )) %>%
  mutate ( group = ifelse(grepl("CPA_F", t_cols2), "construction", group )) %>%
  mutate ( group = ifelse(grepl("CPA_R90.R92", t_cols2), "live music & arts", group )) %>%
  mutate ( group = ifelse(grepl("CPA_J59_J60", t_cols2), "music & audiovisual", group )) %>%
  filter ( t_cols2 != "CPA_L68A")

#there is a naming problem 90.92

 data (croatia_2010_1700)
 labelled_io_data = croatia_2010_1700
 primary_input_get <- primary_input_get( 
                       labelled_io_data = croatia_2010_1700, technology = NULL,
                       input = "compensation_employees", geo = "HR", 
                       unit = "T_NAC", 
                       year = 2010, named = TRUE) 
 