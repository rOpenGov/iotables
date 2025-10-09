test_that("industry to product conversion", {
  data_table <- data.frame(
    ind_use = "A01", 
    A01 = 1,  A02 = 3, TOTAL = 4, 
    HH = 2
  )
  
  converted <- convert_industry_to_product(data_table)
  expect_equal(converted$ind_use, "A01")
  expect_equal(converted$CPA_A01, 1)
  expect_equal(converted$HH, 2)
  
  data_table_2 <- data.frame(
    prod_na = "CPA_A01", 
    A01 = 1,  A02 = 3, TOTAL = 4
  )
  converted_2 <- convert_industry_to_product(data_table_2)
  expect_equal(converted_2$prod_na, "CPA_A01")
  expect_equal(converted_2$CPA_A01, 1)
  expect_equal(converted_2$TOTAL, 4)
})
