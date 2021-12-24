context ("Adding conforming rows to a matrix")

rows_to_add <- data.frame ( iotables_row = "CO2_emission", 
                            agriculture_group =  10448, 
                            industry_group    =  558327, #construction is omitted
                            trade_group       =  11194)

rows_to_add_2 <- data.frame ( agriculture_group =  10448, 
                              industry_group    =  558327, #construction is omitted
                              trade_group       =  11194)

added <- rows_add (iotable_get(), rows_to_add = rows_to_add)
added_2 <- rows_add(data_table = iotable_get(), rows_to_add = rows_to_add_2)
added_3 <-  rows_add(iotable_get(), rows_to_add = c(industry_group    =  1534, 
                                                    trade_group       =  4), 
                     row_names = "CH4_emission")

test_that("Correct row names and values are added to data_table", {
  expect_equal(added[20,1], rows_to_add[1,1])
  expect_equal(added[20,4], 0) # construction is omitted
  expect_equal(added_2[20,1], "new_row_1")
  expect_equal(added[20,2], rows_to_add[1,2])
  expect_equal(added_2[20,5], rows_to_add_2[1,3])
  expect_equal(added_3[20,1], "CH4_emission")
  expect_equal(added_3[20,5], 4)
})

test_that("Incorrect dimensions", {
  expect_error(rows_add(iotable_get(), rows_to_add = c(industry_group    =  1534, 
                                                       trade_group       =  4), 
                        row_names = c("CO2_emission", "CH4_emission")))
})
 
