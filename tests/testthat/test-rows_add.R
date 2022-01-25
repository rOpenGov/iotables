rows_to_add <- data.frame ( iotables_row = "CO2_emission", 
                            agriculture_group =  10448, 
                            industry_group    =  558327, #construction is omitted
                            trade_group       =  11194)

rows_to_add_2 <- data.frame ( agriculture_group =  10448, 
                              industry_group    =  558327, #construction is omitted
                              trade_group       =  11194)

added   <- rows_add (iotable_get(), rows_to_add = rows_to_add)
added_2 <- rows_add(data_table = iotable_get(), rows_to_add = rows_to_add_2)
added_3 <- rows_add(iotable_get(), rows_to_add = c(industry_group    =  1534, 
                                                    trade_group       =  4), 
                     row_names = "CH4_emission")

test_that("rows_add adds conforming rows to the data_table input", {
  expect_equal(as.character(added[20,1]), rows_to_add[1,1])
  expect_equal(as.numeric(added[20,4]), 0) # construction is omitted
  expect_equal(as.character(added_2[20,1]), "new_row_1")
  expect_equal(as.numeric(added[20,2]), rows_to_add[1,2])
  expect_equal(as.numeric(added_2[20,5]), rows_to_add_2[1,3])
  expect_equal(as.character(added_3[20,1]), "CH4_emission")
  expect_equal(as.numeric(added_3[20,5]), 4)
})

test_that("rows_add sends error message when incorrect dimensions are used", {
  expect_error(rows_add(iotable_get(), rows_to_add = c(industry_group    =  1534, 
                                                       trade_group       =  4), 
                        row_names = c("CO2_emission", "CH4_emission")))
})
 
