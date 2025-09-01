test_that("airpol_get works with germany_1995 replication dataset", {
  df <- airpol_get(airpol = "CO2", geo = "germany_1995", year = 1995)

  expect_s3_class(df, "data.frame")
  expect_true("indicator" %in% names(df))
  expect_true("CO2_emission" %in% df$indicator)
})
