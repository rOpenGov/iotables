library (testthat)


test_that("Necessary input parameters are checked", {
  expect_error(iotables_download(source = 'naio_10_cp1701', 
                           year = 2015, geo = "DE",
                           unit = "MIO_EUR", labelling = "iotables"))
})
