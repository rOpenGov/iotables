source = "germany_1990"; geo = "de";
year = 1990; unit = "MIO_EUR"; labelling = "short"
input = "b2n_b3n"

check <- germany_long %>%
  select ( t_rows2, t_cols2, values ) %>%
  spread ( t_cols2, values )

check <- iotable_labelled  %>%
  select ( t_rows2, t_cols2, values ) %>%
  spread ( t_cols2, values )

check <- iotable_get( source = "germany_1990", geo = 'de', year = 1990,
             unit = "MIO_EUR", labelling  = 'iotables')
check <- iotable_get( source = "germany_1990", geo = 'DE', year = 1990,
                      unit = "MIO_EUR", labelling  = 'short')
check <- iotable_get2( source = "germany_1990", geo = 'DE', year = 1990,
                      unit = "MIO_EUR", labelling  = 'short')

check <- use_table_get( source = "germany_1990", geo = 'DE', year = 1990,
                      unit = "MIO_EUR", labelling  = 'iotables')
check <- use_table_get( source = "germany_1990", geo = 'DE', year = 1990,
                      unit = "MIO_EUR", households, labelling  = 'short')

check_primary <- primary_input_get ( input = "b2n_b3n",
                                     source = "germany_1990", geo = 'DE', year = 1990,
                                     unit = "MIO_EUR", 
                                     households = TRUE, labelling  = 'short')
check_primary <- primary_input_get ( input = "b2n_b3n",
                                     source = "germany_1990", geo = 'DE', year = 1990,
                                     unit = "MIO_EUR", 
                                     households = TRUE, labelling  = 'iotables')


primary_input_get <- primary_input_get(
                      input = "compensation_employees", 
                      source = "germany_1990", geo = "DE", 
                      unit = "MIO_EUR", 
                      year = 1990, labelling = "short" ) 

check_primary <- primary_input_get ( input = "D11",
                                     source = "naio_cp17_r2", geo = 'CZ', year = 2010,
                                     unit = "MIO_EUR", 
                                     households = TRUE, labelling  = 'iotables')
check_output_de <- output_get( source = "germany_1990", geo = 'DE', year = 1990,
                        unit = "MIO_EUR", labelling  = 'iotables')
check_output_de <- output_get( source = "germany_1990", geo = 'DE', year = 1990,
                               unit = "MIO_EUR", households = TRUE,
                               labelling  = 'short')

check <- use_table_get( source = "germany_1990", geo = 'DE', year = 1990,
                        unit = "MIO_EUR", labelling  = 'short')

check_2 <- use_table_get( source = "germany_1990", geo = 'DE', year = 1990,
                        unit = "MIO_EUR", households = TRUE, labelling  = 'short')

check_sk <- iotable_get( source = "naio_cp17_r2", geo = 'SK', year = 2010,
                      unit = "MIO_EUR", labelling  = 'iotables')

check_sk_2 <- use_table_get( source = "naio_cp17_r2", geo = 'SK', year = 2010,
                         unit = "MIO_EUR", households = TRUE, labelling  = 'iotables')

check_sk <- use_table_get( source = "naio_cp17_r2", geo = 'SK', year = 2010,
                         unit = "MIO_EUR", labelling  = 'iotables')

check_output <- output_get (source = "naio_cp17_r2", geo = 'SK', year = 2010,
                            unit = "MIO_EUR", labelling  = 'iotables')

source = "naio_cp17_r2"; geo = "SK";
year = 2010; unit = "MIO_EUR"; labelling = 'iotables'
households = TRUE

source = "naio_cp17_r2"; geo = "SK";
year = 2010; unit = "MIO_EUR"; labelling = 'short'


 Im = data.frame (
 a = c("row1", "row2"), 
 b = c(1,1), 
 c = c(2,0))
 LHS = data.frame (
 a = "lhs", 
 b = 1, 
 c = 0.5)
 
 equation_solve (Im = Im, LHS = LHS)
 