test_that("create_knitr_table works with a simple data frame", {
  skip_if_not_installed("knitr")
  foo <- data.frame(
    observation = c("indicator1", "indicator2"),
    value = c(100, 200)
  )
  
  tbl <- create_knitr_table(foo, caption = "Demo")
  
  expect_s3_class(tbl, "knitr_kable")
})

test_that("create_knitr_table errors on empty input", {
  expect_error(create_knitr_table(NULL), "Empty data table")
})

test_that("create_knitr_table respects col.names override", {
  skip_if_not_installed("knitr")
  foo <- data.frame(a = 1:2, b = 3:4)
  tbl <- create_knitr_table(foo, col.names = c("X", "Y"))
  expect_s3_class(tbl, "knitr_kable")
})

test_that("create_knitr_table allows digits override", {
  skip_if_not_installed("knitr")
  foo <- data.frame(a = 1.2345, b = 6.7890)
  tbl <- create_knitr_table(foo, digits = 2)
  expect_s3_class(tbl, "knitr_kable")
})
