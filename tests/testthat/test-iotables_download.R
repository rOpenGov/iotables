test_that("invalid source triggers validation error", {
  expect_error(
    iotables_download(source = "naio_10_cp1701"),
    regexp = "not in supported tables"
  )
})

test_that("force_download default is logical", {
  fn <- get("iotables_download", envir = asNamespace("iotables"))
  formals(fn)$force_download |>
    expect_identical(FALSE)
})

test_that("built-in datasets return correctly", {
  expect_error(iotables_download("germany_1995"),
    regexp = "is a built-in example handled by"
  )
  expect_error(iotables_download("croatia_2010_1800"),
    regexp = "is a built-in example handled by"
  )
  expect_error(iotables_download("netherlands_2000"),
    regexp = "is a built-in example handled by"
  )
})


library(mockery)

test_that("iotables_download() handles Eurostat download + nesting correctly", {
  fake_data <- data.frame(
    geo = c("BE", "BE"),
    unit = "MIO_EUR",
    stk_flow = "DOM",
    TIME_PERIOD = as.Date(c("2020-01-01", "2020-01-01")),
    values = c(100, 200),
    prd_ava = c("CPA_A01", "CPA_B02"),
    prd_use = c("CPA_A01", "CPA_B02"),
    stringsAsFactors = FALSE
  )

  fake_labelled <- fake_data %>%
    dplyr::mutate(
      rows = seq_len(nrow(.)),
      TIME_PERIOD_lab = TIME_PERIOD,
      values_lab = values,
      freq_lab = "Annual",
      unit_lab = "Million euro",
      stk_flow_lab = "Domestic output",
      geo_lab = "Belgium"
    )

  stub(
    iotables_download, "eurostat::get_eurostat",
    function(...) fake_data
  )
  stub(
    iotables_download, "eurostat::label_eurostat",
    function(...) fake_labelled
  )

  result <- iotables_download("naio_10_cp1700")

  expect_s3_class(result, "data.frame")
  expect_true(
    all(c("geo", "year", "unit", "data") %in%
      names(result))
  )
  expect_true(is.list(result$data))
  expect_equal(unique(result$geo), "BE")

  inner <- result$data[[1]]
  expect_s3_class(inner, "data.frame")
  expect_true("values" %in% names(inner))
  expect_true(is.numeric(inner$values))
  expect_true(
    all(c("prd_use", "prd_ava") %in% names(inner))
  )
})

Sys.setenv(RUN_MANUAL_IOTABLES_TESTS = "false")

test_that("manual Eurostat download works for a real dataset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if(
    Sys.getenv("RUN_MANUAL_IOTABLES_TESTS") != "true",
    "Set RUN_MANUAL_IOTABLES_TESTS=true to run this manually."
  )

  naio_10_cp1700 <- iotables_download("naio_10_cp1700",
    force_download = TRUE
  )
  expect_true(is.data.frame(naio_10_cp1700))
  expect_gt(nrow(naio_10_cp1700), 1000)
  expect_true(all(
    c(
      "unit", "stk_flow", "geo", "time",
      "unit_lab", "year", "stk_flow_lab", "geo_lab",
      "data"
    ) %in% names(naio_10_cp1700)
  ))
  expect_true(is.numeric(naio_10_cp1700$year))
  expect_s3_class(naio_10_cp1700$time, "Date")
  expect_true(is.numeric(naio_10_cp1700$data[[1]]$values))
})