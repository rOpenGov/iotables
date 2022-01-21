context ("Adding a supplementary row")
de_io <- iotable_get()


CO2_coefficients <- data.frame(agriculture_group = 0.2379,
                               industry_group    = 0.5172, 
                               construction = 0.0456,
                               trade_group = 0.1320, 
                               business_services_group = 0.0127,
                               other_services_group = 0.0530)

CH4_coefficients <- data.frame(agriculture_group = 0.0349,
                               industry_group    = 0.0011, 
                               construction = 0,
                               trade_group = 0, 
                               business_services_group = 0,
                               other_services_group = 0.0021)
CO2 <- cbind ( 
  data.frame ( iotables_row = "CO2_coefficients"),
  CO2_coefficients
  )

CH4 <- cbind(
  data.frame ( iotables_row = "CH4_coefficients"),
  CH4_coefficients
)

de_coeff <- input_coefficient_matrix_create ( iotable_get() )

emissions <- rbind ( CO2, CH4 )
supplementary_data <- emissions

extended <- supplementary_add  ( data_table = de_io, 
                                 supplementary_data =   emissions)

# Check against The Eurostat Manual page 494

test_that("correct data is returned", {
  expect_equal(extended$construction [ which ( extended[,1] == "CO2_coefficients") ], 
               0.0456, tolerance=1e-6)
  expect_equal(extended$other_services_group[ which ( extended[,1] == "CO2_coefficients" ) ], 
               0.0530, tolerance=1e-6)
  expect_equal(extended$other_services_group[ which ( extended[,1] == "CH4_coefficients" ) ], 
               0.0021, tolerance=1e-6)
})




