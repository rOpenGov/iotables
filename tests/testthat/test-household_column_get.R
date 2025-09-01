test_that("household_column_get works with germany_1995", {
  households <- household_column_get(iotable_get(source = "germany_1995"))

  expect_s3_class(households, "data.frame")
  expect_equal(
    as.numeric(unlist(households[1:2, 2])),
    c(8500, 197792),
    tolerance = 1e-3
  )
})

test_that("household_column_get works with netherlands_2000", {
  data("netherlands_2000")

  households <- household_column_get(netherlands_2000)

  expect_s3_class(households, "data.frame")
  expect_equal(names(households)[2], "final_consumption_households")
})

test_that("household_column_get returns NULL if no column found", {
  df <- data.frame(prod_na = c("a", "b"), industry = c(1, 2))
  expect_null(household_column_get(df))
})
