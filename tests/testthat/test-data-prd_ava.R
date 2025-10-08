test_that("prd_ava TOTAL after intermediate rows", {
  prd_ava <- getdata("prd_ava")
  expect_gt(prd_ava[prd_ava$id=="TOTAL", ]$numeric_order, 110000)
  expect_gt(prd_ava[prd_ava$id=="CPA_TOTAL", ]$numeric_order, 110000)
})



