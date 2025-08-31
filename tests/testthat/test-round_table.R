test_that("round_table: no-op when digits is NULL", {
  df <- tibble::tibble(a = c(1.2345, 2.3456))
  out <- round_table(df, digits = NULL)
  expect_identical(out, df)
})

test_that("round_table: rounds numeric columns to given digits", {
  df <- tibble::tibble(a = c(1.2345, 2.3456), b = "x")
  out <- round_table(df, digits = 2)
  expect_equal(out$a, c(1.23, 2.35))
  expect_identical(out$b, df$b) # non-numeric untouched
})

test_that("round_table: keeps exact 1e-06 values unchanged", {
  df <- tibble::tibble(a = c(1e-06, 1e-05, 0))
  out <- round_table(df, digits = 4)
  expect_identical(out$a[1], 1e-06) # preserved
  expect_equal(out$a[2:3], c(0.0000, 0.0000))
})

test_that("round_table: handles NA without warnings", {
  df <- tibble::tibble(a = c(NA_real_, 1.234))
  expect_silent({
    out <- round_table(df, digits = 1)
  })
  expect_true(is.na(out$a[1]))
  expect_equal(out$a[2], 1.2)
})

test_that("round_table: invalid digits -> warning and no-op", {
  df <- tibble::tibble(a = c(1.234, 2.345))
  expect_warning(
    out <- round_table(df, digits = "two"),
    "rounding digits must be numeric"
  )
  expect_identical(out, df)
})

test_that("round_table: works with data.frame too", {
  df <- data.frame(
    a = c(1.234, 2.345), b = letters[1:2],
    stringsAsFactors = FALSE
  )
  out <- round_table(df, digits = 1)
  expect_equal(out$a, c(1.2, 2.3))
  expect_identical(out$b, df$b)
})
