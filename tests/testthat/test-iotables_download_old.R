Sys.setenv(RUN_MANUAL_IOTABLES_TESTS = "false")


test_that("manual Eurostat download works for a real dataset with
          legacy downloader", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if(
    Sys.getenv("RUN_MANUAL_IOTABLES_TESTS") != "true",
    "Set RUN_MANUAL_IOTABLES_TESTS=true to run this manually."
  )

  io_old <- iotables_download_old(force_download = TRUE)
  expect_true(is.data.frame(io_old))
  expect_gt(nrow(io_old), 1000)
  expect_true(all(
    c(
      "unit", "stk_flow", "geo", "time", "unit_lab", "year",
      "stk_flow_lab", "geo_lab", "data"
    ) %in% names(io_old)
  ))
  expect_true(is.numeric(io_old$year))
  expect_s3_class(io_old$time, "Date")
  expect_true(is.numeric(io_old$data[[1]]$values))
})
