test_table <- input_coefficient_matrix_create(iotable_get(source = "germany_1995"))
  
test_table[, 2] <- 0
subsetted <- empty_remove (test_table)

test_that("empty_remove() removes empty rows as expected", {
  expect_equal(names(subsetted), 
               c("iotables_row", "industry_group", "construction", "trade_group", 
                 "business_services_group", "other_services_group"))
  expect_equal(nrow(subsetted)+1, ncol(subsetted))
})
