test_that("getdata() returns the right dataset to the current environment", {
  my_data <- "metadata"
  expect_s3_class(getdata("metadata"), "data.frame")
  expect_s3_class(getdata(my_data), "data.frame")
})

test_that("getdata() fails gracefully", {
  my_non_data <- "non_data"
  expect_error(getdata(my_non_data),
    regexp = "not found in iotables package data"
  )
  expect_error(getdata("no_data"),
    regexp = "not found in iotables package data"
  )
})
