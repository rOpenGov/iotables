test_that("prd_use is correctly assembled", {
  prd_use <- getdata("prd_use")
  expect_gt(prd_use[prd_use$id == "TOTAL", ]$numeric_order, 110000)
  expect_gt(prd_use[prd_use$id == "CPA_TOTAL", ]$numeric_order, 110000)
  expect_true("CPA_C16" %in% prd_use$id)
  expect_true("CPA_C15" %in% prd_use$id)
  expect_gt(nrow(prd_use), 180)
  expect_equal(ncol(prd_use), 10)
})
