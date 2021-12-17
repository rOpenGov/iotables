library (testthat)

context ("Adding a supplementary row")
de_io <- iotable_get()
CO2 <- c( 0.2379, 0.5172, 0.0456, 0.1320, 0.0127, 0.0530)  
names ( CO2) <- c("agriculture_group", "industry_group","construction",
                  "trade_group","business_services_group","other_services_group") 
CO2 <- cbind ( 
  data.frame ( iotables_row = "CO2"),as.data.frame ( t(CO2)))
de_coeff <- input_coefficient_matrix_create ( iotable_get() )

extended <- supplementary_add  ( de_io, CO2)

#The Eurostat Manual page 494

test_that("correct data is returned", {
  expect_equal(extended$construction [ which ( extended[,1] == "CO2") ], 
               0.0456, tolerance=1e-6)
  expect_equal(extended$other_services[ which ( extended[,1] == "CO2") ], 
               0.0530, tolerance=1e-6)
})




