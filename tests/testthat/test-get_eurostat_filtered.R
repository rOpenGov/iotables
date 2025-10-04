test_that("filtered Eurostat download returns correct values", {
  skip_on_cran()
  skip_on_ci()
  downloaded <- get_eurostat_filtered(
    id = "naio_10_cp1700",
    filters = list(geo = "HU", time = "2020", stk_flow = "TOTAL")
  )
  expect_s3_class(downloaded, "data.frame")
  expect_true(all(downloaded$stk_flow == "TOTAL"))
  expect_true(all(downloaded$geo == "HU"))
  expect_true(all(downloaded$TIME_PERIOD == as.Date("2020-01-01")))
  expect_equal(attr(downloaded, "dataset"), "naio_10_cp1700")
})
