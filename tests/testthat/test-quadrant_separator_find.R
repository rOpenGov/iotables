test_that("quadrant separator correctly locates end of Quadrant I", {
  small_io <- data.frame(
    prod_na = c("CPA_A", "CPA_B", "CPA_C", "output"),
    CPA_A = c(10, 4, 1, 35),
    CPA_B = c(2, 8, 5, 30),
    CPA_C = c(3, 2, 6, 30),
    total = c(20, 16, 18, NA_real_)
  )

  expect_equal(quadrant_separator_find(small_io), 4) # before total
  expect_equal(quadrant_separator_find(small_io, include_total = TRUE), 5)
})
