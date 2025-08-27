test_that("employment_get errors on invalid labelling", {
  expect_error(
    employment_get("CZ", year = "2010", labelling = "wrong"),
    "Labelling must be any of"
  )
})

Sys.setenv(RUN_EUROSTAT_TESTS = "false")
# Only test it occassionally, takes very long to test.
skip_if_no_eurostat <- function() {
  if (Sys.getenv("RUN_EUROSTAT_TESTS") != "true") {
    testthat::skip("Eurostat tests are disabled (set RUN_EUROSTAT_TESTS=true to enable).")
  }
}

test_that("employment_get errors on invalid geo", {
  skip_on_cran() # avoid spurious failures due to Eurostat availability
  skip_if_not_installed("eurostat")
  skip_if_no_eurostat()
  expect_error(
    employment_get("ZZ", year = "2010"),
    "No employment data found with geo parameter"
  )
})

test_that("employment_get errors on invalid year", {
  skip_on_cran() # avoid spurious failures due to Eurostat availability
  skip_if_not_installed("eurostat")
  skip_if_no_eurostat()
  expect_error(
    employment_get("CZ", year = "1800"),
    "No employment data found with the year parameter"
  )
})

test_that("employment_get returns a data.frame for valid inputs", {
  skip_on_cran() # avoid spurious failures due to Eurostat availability
  skip_if_not_installed("eurostat")
  skip_if_no_eurostat()
  result <- employment_get("CZ", year = "2010", labelling = "iotables")
  expect_s3_class(result, "data.frame")
  expect_true(any(grepl("employment_", names(result)[1])))
  expect_true("real_estate_imputed_a" %in% names(result))
})

test_that("employment_get works with different labelling formats", {
  skip_on_cran() # avoid spurious failures due to Eurostat availability
  skip_if_not_installed("eurostat")
  skip_if_no_eurostat()
  res_prod <- employment_get("CZ", year = "2010", labelling = "prod_na")
  res_ind <- employment_get("CZ", year = "2010", labelling = "induse")

  expect_s3_class(res_prod, "data.frame")
  expect_s3_class(res_ind, "data.frame")

  expect_true(any(grepl("employment_", names(res_prod)[1])))
  expect_true(any(grepl("employment_", names(res_ind)[1])))
})

test_that("employment_get handles UK/EL special geo codes", {
  skip_on_cran() # avoid spurious failures due to Eurostat availability
  skip_if_not_installed("eurostat")
  skip_if_no_eurostat()
  expect_warning(
    res_uk <- employment_get("GB", year = "2010"),
    "Switching GB to Eurostat abbreviation UK"
  )
  expect_s3_class(res_uk, "data.frame")

  expect_warning(
    res_el <- employment_get("GR", year = "2010"),
    "Switching GR to Eurostat abbreviation EL"
  )
  expect_s3_class(res_el, "data.frame")
})
