test_that("iotables_metadata_get strips data column", {
  dummy <- data.frame(
    geo = "DE", unit = "MIO_EUR", year = 1995,
    data = I(list(data.frame(x = 1)))
  )

  out <- iotables_metadata_get(dummy, source = "naio_10_cp1700")
  expect_false("data" %in% names(out))
  expect_s3_class(out, "data.frame")
})

test_that("metadata is extracted correctly from nested tibble", {
  fake <- tibble::tibble(
    geo = c("BE", "FR"),
    year = c(2020, 2020),
    unit = "MIO_EUR",
    stk_flow = "DOM",
    data = list(data.frame(x = 1), data.frame(x = 2))
  )
  
  meta <- iotables_metadata_get(fake, "naio_10_cp1700")
  expect_false("data" %in% names(meta))
  expect_true(all(c("geo", "year", "stk_flow") %in% names(meta)))
})

test_that("reads from cache when dat is NULL", {
  tmp <- file.path(tempdir(), "naio_10_cp1700_processed.rds")
  fake <- tibble::tibble(
    geo = "BE", year = 2020, data = list(data.frame(a = 1))
  )
  saveRDS(fake, tmp)
  
  meta <- iotables_metadata_get(NULL, "naio_10_cp1700")
  expect_s3_class(meta, "tbl_df")
  unlink(tmp)
})

test_that("gracefully handles missing cache file", {
  unlink(file.path(tempdir(), "naio_10_cp1750_processed.rds"))
  expect_message(iotables_metadata_get(NULL, "naio_10_cp1750"), 
                 "Try running iotables_download")
})

