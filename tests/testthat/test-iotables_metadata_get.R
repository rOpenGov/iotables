test_that("iotables_metadata_get errors gracefully on missing file", {
  expect_message(
    out <- iotables_metadata_get(source = "naio_10_cp1700"),
    "not found"
  )
  expect_null(out)
})

test_that("iotables_metadata_get strips data column", {
  dummy <- data.frame(
    geo = "DE", unit = "MIO_EUR", year = 1995,
    data = I(list(data.frame(x = 1)))
  )

  out <- iotables_metadata_get(dummy, source = "naio_10_cp1700")
  expect_false("data" %in% names(out))
  expect_s3_class(out, "data.frame")
})
