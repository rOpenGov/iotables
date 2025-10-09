test_that("ind_ava is correctly constructed", {
  ind_ava <- getdata("ind_ava")
  expect_true("C16" %in% ind_ava$id)
  expect_true("C15" %in% ind_ava$id)
  expect_gt(nrow(ind_ava), 135)
  expect_gt(ind_ava[ind_ava$id == "TOTAL", ]$numeric_order, 110000)
})
