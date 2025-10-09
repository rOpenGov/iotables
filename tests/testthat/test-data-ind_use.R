test_that("ind_use is correctly assembled", {
  ind_use <- getdata("ind_use")
  expect_gt(ind_use[ind_use$id == "TOTAL", ]$numeric_order, 110000)
  expect_gt(nrow(ind_use), 180)
  expect_true("C16" %in% ind_use$id)
  expect_true("C15" %in% ind_use$id)
  expect_equal(ncol(ind_use), 10)
})
