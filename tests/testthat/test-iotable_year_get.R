test_that("iotable_year_get() stops on error:", {
  expect_error(iotable_year_get(
    source = "germany_1995",
    geo = "DE", unit = "MIO_NAC"
  ))
})

test_that("iotable_year_get() returns correct data:", {
  expect_equal(
    iotable_year_get(
      source = "germany_1995",
      geo = "DE", unit = "MIO_EUR"
    ),
    as.Date("1995-01-01")
  )
})
