test_that("household_column_find works on Netherlands 2006 dataset", {
  data("netherlands_2006")
  
  pos <- household_column_find(netherlands_2006)
  
  # It should find "final_consumption_households"
  expect_true("final_consumption_households" %in% names(netherlands_2006)[pos])
  
  # The position should be the column number of "final_consumption_households"
  expect_equal(
    pos,
    which(names(netherlands_2006) == "final_consumption_households")
  )
})

test_that("household_column_find works with renamed Netherlands columns", {
  df <- netherlands_2006
  names(df)[names(df) == "final_consumption_households"] <- "Final_Consumption_HouseHolds"
  
  pos <- household_column_find(df)
  
  # Should still detect case-insensitive match
  expect_equal(pos, which(names(df) == "Final_Consumption_HouseHolds"))
})

test_that("household_column_find falls back to partial matches", {
  df <- netherlands_2006
  names(df)[names(df) == "final_consumption_households"] <- "all_households_expenditure"
  
  pos <- household_column_find(df)
  
  # Should detect "households" substring
  expect_equal(pos, which(names(df) == "all_households_expenditure"))
})
