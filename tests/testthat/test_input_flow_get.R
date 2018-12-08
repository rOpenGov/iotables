library (testthat)
library (iotables)
context ("Creating an input flow table")

input_flow_table <- input_flow_get(iotable_get(labelling = "iotables"),
                           households = TRUE)

test_that("correct data is returned", {
  expect_equal(input_flow_table [1,2], 1131)
  expect_equal(input_flow_table[3,'final_consumption_households'],
               3457)
})

#check Eurostat Manual page 461

test_that("households are included or excluded", {
  expect_equal("final_consumption_households" %in% 
                 names ( input_flow_get(iotable_get(labelling = "iotables"),
                                        households = FALSE) ), FALSE)
  expect_equal("final_consumption_households" %in% 
                 names ( input_flow_table ), TRUE)
})
