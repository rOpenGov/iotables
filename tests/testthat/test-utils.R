test_that("is_key_column_present() works in different scenarios", {
  # Case 1: first column is numeric → should error
  expect_error(
    is_key_column_present(data.frame(
      agriculture = 0.0123,
      manufacturing = 0.1436,
      trade = 0.0921
    )),
    "expected non-numeric first col"
  )

  # Case 2: key column exists but no keyword match → should error
  expect_error(
    is_key_column_present(
      data.frame(
        indicator = "your_indicator",
        agriculture = 0.0123,
        manufacturing = 0.1436,
        trade = 0.0921
      ),
      potential_keywords = c("her_indicator", "his_indicator")
    ),
    "has no key column containing any of"
  )

  # Case 3: valid keyword present → should return TRUE
  expect_true(
    is_key_column_present(
      data.frame(
        indicator = "your_indicator",
        agriculture = 0.0123,
        manufacturing = 0.1436,
        trade = 0.0921
      ),
      potential_keywords = c("her_indicator", "your_indicator")
    )
  )
})

test_that("validate_source() correctly validates sources", {
  # Invalid source
  expect_error(
    validate_source("not_a_table"),
    "is not in supported tables"
  )

  # Valid source
  expect_silent(validate_source("naio_10_cp1700"))
})

test_that("ensure_l68_columns() adds missing columns only once", {
  df <- data.frame(existing = 1)

  result <- ensure_l68_columns(df, c("CPA_L68A", "CPA_L68B"))

  expect_named(result, c("existing", "CPA_L68A", "CPA_L68B"))
  expect_identical(result$CPA_L68A, 0)
  expect_identical(result$CPA_L68B, 0)

  second <- ensure_l68_columns(result, c("CPA_L68A", "CPA_L68B"))

  expect_named(second, c("existing", "CPA_L68A", "CPA_L68B"))
  expect_identical(second$CPA_L68A, 0)
  expect_identical(second$CPA_L68B, 0)
})

test_that("ensure_l68_columns() keeps existing non-zero values", {
  df <- data.frame(CPA_L68A = 5, another = 2)

  result <- ensure_l68_columns(df, c("CPA_L68A", "CPA_L68B"))

  expect_identical(result$CPA_L68A, 5)
  expect_identical(result$CPA_L68B, 0)
  expect_identical(result$another, 2)
})
