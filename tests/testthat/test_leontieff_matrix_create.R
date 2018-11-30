library (testthat)
library (iotables)
context ("Creating the Leontieff matrix and its inverse")

de_use <- use_table_get ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          households = FALSE, labelling = "short")

de_output <- output_get ( source = "germany_1990", geo = "DE",
                          year = 1990, unit = "MIO_EUR", 
                          households = FALSE, labelling = "short")


nl_use <- use_table_get ( labelled_io_table = netherlands_2006, source  = 'custom')
nl_output <- output_get ( labelled_io_table = netherlands_2006 )
names (de_use)[1] == names(de_output)[1]

nl_coeff_3   <- input_coefficient_matrix_create( input_flow = nl_use,
                                               output = nl_output, 
                                               digits = 3)

nl_coeff   <- input_coefficient_matrix_create( input_flow = nl_use,
                                                 output = nl_output, 
                                                 digits = NULL)

de_coeff <- input_coefficient_matrix_create( input_flow = de_use, 
                                             output =  de_output, digits = 4)

L_de <- leontieff_matrix_create( technology_coefficients_matrix =
                                          de_coeff )
I_de <- leontieff_inverse_create(L_de)

L_nl <- leontieff_matrix_create( technology_coefficients_matrix =
                                   nl_coeff )
I_nl <- leontieff_inverse_create(L_nl)




require(dplyr)
AAL <- L_de %>%
  dplyr::filter ( t_rows2 == "cpa_a") %>%
  dplyr::select (  agriculture_group ) %>%
  unlist () %>%  as.numeric(.)


#test against 15.9 p 487

TBI <- I_de %>%
  dplyr::filter ( t_rows2 == 'cpa_g_i') %>%
  dplyr::select ( business_services_group ) %>%
  unlist () %>%
  as.numeric(.)

#test against 15.10 p 488

test_that("Leontieff matrix values are correct", {
  expect_equal(AAL[1],expected= 0.9742, tolerance = .0001)
  expect_equal(as.numeric(L_nl[1, 2:7]), 
               expected = c(0.875, 0.000, -0.039, -0.002, -0.001, -0.001), 
               tolerance = 0.0005)
  expect_equal(round(as.numeric(I_nl[1,2:7]), 3), 
               expected = c(1.154, 0.002, 0.057, 0.006, 0.012, 0.006), 
               tolerance = 0.0005)
   })

test_that("Leontieff inverse values are correct", {
  expect_equal(TBI, expected= 0.035494905, tolerance = .0001)
  expect_equal(sum(sapply(I_de, function(x) sum(is.nan(x)))), expected = 0) #should not be NaNs
})

