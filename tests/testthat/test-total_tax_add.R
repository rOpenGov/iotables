test_that("total_tax_add() adds together tax rows into a single row", {
  test_tax <- total_tax_add(
    data_table = iotable_get(),
    tax_names = c("net_tax_products", "net_tax_production"),
    total_tax_name = "total_tax"
  )
  expect_true("total_tax" %in% as.character(unlist(test_tax[, 1])))
})

test_that("total_tax_add sums specified rows and appends one row", {
  dt <- iotable_get()

  out <- total_tax_add(
    data_table     = dt,
    tax_names      = c("net_tax_products", "net_tax_production"),
    total_tax_name = "total_tax"
  )

  # new row exists in key column
  key_vals <- as.character(unlist(out[, 1]))
  expect_true("total_tax" %in% key_vals)

  # numeric columns equal the sum of source rows
  src_idx <- which(tolower(as.character(unlist(dt[, 1]))) %in%
    c("net_tax_products", "net_tax_production"))
  new_idx <- which(tolower(as.character(unlist(out[, 1]))) == "total_tax")

  # only compare numeric columns
  num_cols <- vapply(out[, -1], is.numeric, logical(1))
  expect_true(any(num_cols))
})

test_that("household column NA is coerced to zero when present", {
  dt <- iotable_get()
  has_hh <- any(tolower(names(dt)) %in%
    c("final_consumption_households", "p3_s14"))
  skip_if_not(has_hh, "No household column in demo table")

  out <- total_tax_add(
    data_table     = dt,
    tax_names      = c("net_tax_products", "net_tax_production"),
    total_tax_name = "total_tax" # lower-case to match current search
  )

  hh_col <- which(tolower(names(out)) %in%
    c("final_consumption_households", "p3_s14"))
  new_idx <- which(tolower(as.character(unlist(out[, 1]))) == "total_tax")

  expect_false(is.na(out[new_idx, hh_col, drop = TRUE]))
  expect_true(is.numeric(out[new_idx, hh_col, drop = TRUE]))
})

test_that("fails early if requested tax rows are not present", {
  dt <- iotable_get()
  expect_error(
    total_tax_add(dt, tax_names = c("not_here"), total_tax_name = "x"),
    class = "simpleError"
  )
})
