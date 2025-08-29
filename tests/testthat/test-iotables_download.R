test_that("Necessary input parameters are checked", {
  expect_error(iotables_download(
    source = "naio_10_cp1701",
    year = 2015, geo = "DE",
    unit = "MIO_EUR", labelling = "iotables"
  ))
})

test_that("returns early when tempdir_data is already processed", {
  fake_processed <- data.frame(
    geo = "DE", unit = "MIO_EUR",
    year = 1995,
    data = I(list(data.frame(x = 1)))
  )

  with_mocked_bindings(
    validate_source = function(src) invisible(TRUE),
    tempdir_data = function(src, force_download) fake_processed,
    {
      expect_message(
        out <- iotables_download(source = "naio_10_cp1700"),
        regexp = "Returning the processed SIOTs"
      )
      expect_identical(out, fake_processed)
    }
  )
})

test_that("errors when download shape is not plausible", {
  fake_bad <- data.frame(a = 1, b = 2, c = 3) # too few cols/rows

  with_mocked_bindings(
    validate_source = function(src) invisible(TRUE),
    tempdir_data = function(src, force_download) fake_bad,
    {
      expect_error(
        iotables_download(source = "naio_10_cp1700"),
        regexp = "was not successful"
      )
    }
  )
})


test_that("uk_2010 path returns uk_2010_get()", {
  sentinel <- data.frame(ok = TRUE)
  with_mocked_bindings(
    uk_2010_get = function() sentinel,
    {
      res <- iotables_download(source = "uk_2010")
      expect_identical(res, sentinel)
    }
  )
})
