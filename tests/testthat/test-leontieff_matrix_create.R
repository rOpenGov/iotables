context ("Creating the Leontief matrix and its inverse")

de_use <- input_flow_get( iotable_get (labelling = "short" ))
nl_use <- input_flow_get(netherlands_2006)


nl_coeff_3   <- input_coefficient_matrix_create( data_table = netherlands_2006, 
                                               digits = 3)

nl_coeff   <- input_coefficient_matrix_create( data_table = netherlands_2006, 
                                                 digits = NULL)

de_coeff <- input_coefficient_matrix_create( iotable_get(), digits = 4)

L_de <- leontief_matrix_create( technology_coefficients_matrix =
                                          de_coeff )
I_de <- leontief_inverse_create( technology_coefficients_matrix =
                                    de_coeff )

L_nl <- leontief_matrix_create( technology_coefficients_matrix =
                                   nl_coeff )
I_nl <- leontief_inverse_create(technology_coefficients_matrix =
                                   nl_coeff)




require(dplyr)
AAL <- L_de %>%
  dplyr::filter ( iotables_row == "agriculture_group") %>%
  dplyr::select (  agriculture_group ) %>%
  unlist () %>%  as.numeric(.)

# Test are against the Eurostat manual
# test against 15.9 p 487

TBI <- I_de %>%
  dplyr::filter ( iotables_row == "trade_group") %>%
  dplyr::select ( business_services_group ) %>%
  unlist () %>%
  as.numeric()

# test against 15.10 p 488

test_that("Leontief matrix values are correct", {
  expect_equal(AAL[1],expected= 0.9742, tolerance = .0001)
  expect_equal(as.numeric(L_nl[1, 2:7]), 
               expected = c(0.875, 0.000, -0.039, -0.002, -0.001, -0.001), 
               tolerance = 0.0005)
  expect_equal(round(as.numeric(I_nl[1,2:7]), 3), 
               expected = c(1.154, 0.002, 0.057, 0.006, 0.012, 0.006), 
               tolerance = 0.0005)
   })

test_that("Leontief inverse values are correct", {
  expect_equal(TBI, expected= 0.035494905, tolerance = .0001)
  expect_equal(sum(sapply(I_de, function(x) sum(is.nan(x)))), expected = 0) #should not be NaNs
})

