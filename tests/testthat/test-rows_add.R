test_that("rows_add adds conforming rows to the data_table input", {
  rows_to_add <- data.frame(
    iotables_row = "CO2_emission",
    agriculture_group = 10448,
    industry_group = 558327,
    trade_group = 11194
  )
  rows_to_add_2 <- data.frame(
    agriculture_group = 10448,
    industry_group = 558327,
    trade_group = 11194
  )
  
  added   <- rows_add(iotable_get(), rows_to_add = rows_to_add)
  added_2 <- rows_add(iotable_get(), rows_to_add = rows_to_add_2)
  added_3 <- rows_add(
    iotable_get(),
    rows_to_add = c(industry_group = 1534, trade_group = 4),
    row_names   = "CH4_emission"
  )
  
  expect_identical(added[20, 1, drop = TRUE], "CO2_emission")
  expect_identical(added[20, 4, drop = TRUE], 0)     # construction omitted
  expect_identical(added_2[20, 1, drop = TRUE], "new_row_1")
  expect_identical(added[20, 2, drop = TRUE], 10448)
  expect_identical(added_2[20, 5, drop = TRUE], 11194)
  expect_identical(added_3[20, 1, drop = TRUE], "CH4_emission")
  expect_identical(added_3[20, 5, drop = TRUE], 4)
  
  # sanity check on names
  expect_named(added, names(iotable_get()))
})

test_that("rows_add errors when row_names length mismatches input", {
  expect_error(
    rows_add(
      iotable_get(),
      rows_to_add = c(industry_group = 1534, trade_group = 4),
      row_names   = c("CO2_emission", "CH4_emission")
    ),
    "do not match"
  )
})

