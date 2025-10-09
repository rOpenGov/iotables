test_that("prd_ava is correctly assembled", {
  prd_ava <- getdata("prd_ava")
  expect_gt(prd_ava[prd_ava$id == "TOTAL", ]$numeric_order, 110000)
  expect_gt(prd_ava[prd_ava$id == "CPA_TOTAL", ]$numeric_order, 110000)
  expect_true("CPA_C16" %in% prd_ava$id)
  expect_true("CPA_C15" %in% prd_ava$id)
  expect_equal(ncol(prd_ava), 10)
})
