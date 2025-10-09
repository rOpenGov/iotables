test_that("filtered Eurostat download returns correct values", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  # CPA product×product
  d1 <- get_eurostat_filtered(
    id = "naio_10_cp1700",
    filters = list(
      geo = "HU",
      time = "2020",
      stk_flow = "TOTAL"
    )
  )
  expect_s3_class(d1, "data.frame")
  expect_true(all(d1$stk_flow == "TOTAL"))
  expect_true(all(d1$geo == "HU"))
  expect_true(all(d1$TIME_PERIOD == as.Date("2020-01-01")))
  expect_equal(attr(d1, "dataset"), "naio_10_cp1700")
  expect_true(all(c("prd_ava", "prd_use") %in% names(d1)))
  expect_equal(sum(is.na(d1$values)), 0)


  # NACE industry×industry
  d2 <- get_eurostat_filtered(
    id = "naio_10_cp1750",
    filters = list(
      geo = "CZ",
      time = "2020",
      stk_flow = "TOTAL"
    )
  )
  expect_s3_class(d2, "data.frame")
  expect_true(all(d2$stk_flow == "TOTAL"))
  expect_true(all(d2$geo == "CZ"))
  expect_true(all(d2$TIME_PERIOD == as.Date("2020-01-01")))
  expect_equal(attr(d2, "dataset"), "naio_10_cp1750")
  expect_true(all(c("ind_ava", "ind_use") %in% names(d2)))
  expect_equal(sum(is.na(d2$values)), 0)
})

test_that("filtered Eurostat download returns correct values", {
  skip_on_cran()
  skip_if_offline()
  filters_list <- list(geo="IT", unit="THS_T", time=2020)
  airpol_it <- get_eurostat_data(id="env_ac_ainah_r2", 
                                  filters = filters_list)
  expect_equal(unique(airpol_it$geo), "IT")
  expect_equal(unique(airpol_it$unit), "THS_T")
  expect_equal(unique(airpol_it$year), 2020L)
})
