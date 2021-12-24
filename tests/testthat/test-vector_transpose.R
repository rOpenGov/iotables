context ("Creating longer data tables.")


test_that("Correct form returned", {
  expect_warning(vector_transpose ( data.frame (indicator = "my_inidcator", 
                                                agriculture = 0.0123,
                                                manufacturing = 0.1436,
                                                trade = 0.0921), 
                                    .keep = TRUE))
  expect_equal(names(vector_transpose_longer( data.frame (indicator = "my_inidcator", 
                                                    agriculture = 0.0123,
                                                    manufacturing = 0.1436,
                                                    trade = 0.0921), 
                                        .keep = TRUE)), 
               c("indicator", "nace_r2", "value"))
  expect_equal(names(vector_transpose_longer( data.frame (indicator = "my_inidcator", 
                                                    agriculture = 0.0123,
                                                    manufacturing = 0.1436,
                                                    trade = 0.0921), 
                                        .keep = FALSE)),
               c("nace_r2", "value"))
})

test_that("Key column is correctly created", {
  expect_equal(key_column_create ("iotables_row", c("CO2_multiplier", "CH4_multiplier")), 
               tibble ( iotables_row = c("CO2_multiplier", "CH4_multiplier"))) })

data("germany_airpol")

context ("Creating wider data tables.")

airpol_wide <- vector_transpose_wider (data_table =  germany_airpol[, -2],
                                       names_from = 'induse',
                                       values_from = 'value')

airpol_wide_2 <- vector_transpose_wider (data_table =  germany_airpol[1:8, 3:4],
                                       names_from = 'induse',
                                       values_from = 'value', 
                                       key_column_values = "CO2_emission", 
                                       key_column_name = "emissions")

test_that("Key column is correctly created", {
  expect_equal(ncol(airpol_wide), 9) 
  expect_equal(ncol(airpol_wide_2), 9)
  expect_equal(names(airpol_wide_2)[1], "emissions") 
  expect_equal(names(airpol_wide)[1], names(germany_airpol)[1]) 
  expect_equal(as.character(airpol_wide_2[1,1]), "CO2_emission") 
  })
