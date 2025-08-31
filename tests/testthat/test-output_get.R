test_that("output_get returns a one-row P1 vector with key column", {
  dt <- iotable_get()
  out <- output_get(dt)
  
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_identical(names(out)[1], names(dt)[1])
  
  # label should be one of the recognised P1 names
  p1_label <- as.character(out[[1]][1])
  expect_true(p1_label %in% c("output", "output_bp", "P1", "p1"))
  
  # all non-key columns are numeric
  if (ncol(out) > 1L) {
    is_num <- vapply(out[-1], is.numeric, logical(1))
    expect_true(all(is_num))
  }
})

test_that("output_get finds P1 when any standard label is present", {
  dt <- iotable_get()
  key_vals <- as.character(unlist(dt[, 1]))
  expect_true(any(c("output", "output_bp", "P1", "p1") %in% key_vals))
  
  out <- output_get(dt)
  expect_equal(nrow(out), 1L)
  expect_true(!anyNA(unlist(out[-1])))
})
