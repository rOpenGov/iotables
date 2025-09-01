test_that("netherlands_2000 dataset has correct structure", {
  data(netherlands_2000, package = "iotables")

  # Dimensions: 14 rows × 13 columns
  expect_equal(dim(netherlands_2000), c(13, 14))

  # Column names
  expected_cols <- c(
    "prod_na", "agriculture_group", "mining_group",
    "manufacturing_group", "utilities_group",
    "construction_group", "services_group", "TOTAL",
    "final_consumption_private", "final_consumption_households",
    "final_consumption_government", "gross_fixed_capital_formation",
    "exports", "total_use"
  )
  expect_equal(names(netherlands_2000), expected_cols)

  # prod_na should be character
  expect_true(is.character(netherlands_2000$prod_na))

  # Numeric columns should be numeric/integer
  expect_true(all(sapply(netherlands_2000[, -1], is.numeric)))
})

test_that("netherlands_2000 matches reference values from Spicosa PDF", {
  data(netherlands_2000, package = "iotables")

  # Agriculture row, agriculture_group col = 2731 (Table 1, p. 9)
  expect_equal(
    netherlands_2000$agriculture_group[netherlands_2000$prod_na == "agriculture_group"],
    2731
  )

  # Manufacturing row, exports = 113777
  expect_equal(
    netherlands_2000$exports[netherlands_2000$prod_na == "manufacturing_group"],
    113777
  )

  # Services row, final_consumption_households = 123398
  expect_equal(
    netherlands_2000$final_consumption_households[
      netherlands_2000$prod_na == "services_group"
    ],
    123398
  )

  # Total use for agriculture = 21863
  expect_equal(
    netherlands_2000$total_use[netherlands_2000$prod_na == "agriculture_group"],
    21863
  )
})
