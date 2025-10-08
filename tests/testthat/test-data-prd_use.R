test_that("prd_ava TOTAL after intermediate rows", {
  prd_use <- getdata("prd_use")
  expect_gt(prd_use[prd_use$id=="TOTAL", ]$numeric_order, 110000)
  expect_gt(prd_use[prd_use$id=="CPA_TOTAL", ]$numeric_order, 110000)
})

