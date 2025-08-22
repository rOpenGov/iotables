test_that("empty_remove drops all-zero columns and matching rows", {
  tbl <- data.frame(
    sector = c("A", "B", "C"),
    A = c(1, 0, 0),
    B = c(0, 0, 0),
    C = c(0, 2, 0),
    check.names = FALSE
  )
  
  out <- suppressMessages(empty_remove(tbl))
  
  # Column "B" and row "B" should be gone
  expect_false("B" %in% names(out))
  expect_false("B" %in% out$sector)
  
  # Other sectors remain
  expect_true(all(c("A", "C") %in% names(out)))
})

test_that("empty_remove keeps table unchanged if no empty cols/rows", {
  tbl <- data.frame(
    sector = c("A", "B"),
    A = c(1, 0),
    B = c(0, 1)
  )
  out <- suppressMessages(empty_remove(tbl))
  expect_equal(out, tbl)
})

test_that("empty_remove handles NA columns as empty", {
  tbl <- data.frame(
    sector = c("A", "B"),
    A = c(1, 2),
    B = c(NA, NA)
  )
  out <- suppressMessages(empty_remove(tbl))
  expect_false("B" %in% names(out))
})


test_that("empty_remove() removes empty rows as expected", {
  test_table <- input_coefficient_matrix_create(iotable_get(source = "germany_1995"))
  
  test_table[, 2] <- 0
  subsetted <- empty_remove(test_table)
  
  expect_equal(
    names(subsetted),
    c(
      "iotables_row", "industry_group", "construction", "trade_group",
      "business_services_group", "other_services_group"
    )
  )
  expect_equal(nrow(subsetted) + 1, ncol(subsetted))
})


test_that("non_zero_columns_find works for numeric columns", {
  expect_true(non_zero_columns_find(c(1, 0, 0)))
  expect_false(non_zero_columns_find(c(0, 0, 0)))
})

test_that("non_zero_columns_find always returns TRUE for factors/characters", {
  expect_true(non_zero_columns_find(c("a", "b")))
  expect_true(non_zero_columns_find(factor(c("x", "y"))))
})

test_that("non_zero_columns_find works on data.frame columns", {
  df <- data.frame(a = c(0, 0), b = c(1, 0))
  out <- vapply(df, non_zero_columns_find, logical(1))
  expect_equal(out, c(a = FALSE, b = TRUE))
})
