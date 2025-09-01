test_that("check_digits accepts valid inputs", {
  expect_invisible(check_digits(NULL)) # NULL is allowed
  expect_invisible(check_digits(2)) # integer is numeric
  expect_invisible(check_digits(2.5)) # double is numeric
})

test_that("check_digits rejects invalid inputs", {
  expect_error(
    check_digits("two"),
    "rounding 'digits' must be numeric"
  )
  expect_error(
    check_digits(list(2)),
    "rounding 'digits' must be numeric"
  )
})
