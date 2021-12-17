context ("Creating an  input coefficient matrix")

de_input_flow <- input_flow_get ( iotable_get() )

nl <- netherlands_2006
nl_input_flow <- input_flow_get ( data_table = nl )


nl_coeff   <- input_coefficient_matrix_create( data_table = nl,
                                               households = FALSE,
                                               digits = 2)

nl_services_row <- nl_coeff %>%
  dplyr::filter ( prod_na == "services_group" ) %>%
  dplyr::select ( -prod_na ) %>%
  unlist(.) %>% as.numeric (.)

de_input_coefficients <- input_coefficient_matrix_create (
  iotable_get(), digits = 5)


business_agriculture_input <- de_input_coefficients %>%
  dplyr::filter ( iotables_row == "business_services_group") %>%
  dplyr::select ( agriculture_group) %>%
  as.numeric(.)

BSBS <- de_input_coefficients %>%
  dplyr::filter ( iotables_row == "business_services_group") %>%
  dplyr::select ( business_services_group ) %>%
  as.numeric(.)

test_that("get_input_flow correct input coefficients are returned", {
  expect_equal(business_agriculture_input, 0.0828, tolerance = .00005)
  expect_equal(BSBS, 0.279, tolerance = .00005)
  expect_equal(nl_services_row, c(
    0.13, 0.09, 0.13, 0.08, 0.16, 0.25))
  })

