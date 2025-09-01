test_that("vector_transpose_longer works with .keep option", {
  df <- data.frame(
    indicator = "my_indicator",
    agriculture = 0.0123,
    manufacturing = 0.1436,
    trade = 0.0921
  )

  expect_warning(
    vector_transpose(df, .keep = TRUE),
    regexp = "deprecated", # expect deprecation warning
    fixed = TRUE
  )

  out1 <- vector_transpose_longer(df, .keep = TRUE)
  expect_s3_class(out1, "data.frame")
  expect_equal(names(out1), c("indicator", "nace_r2", "value"))

  out2 <- vector_transpose_longer(df, .keep = FALSE)
  expect_equal(names(out2), c("nace_r2", "value"))
})


test_that("key_column_create works with empty values", {
  out <- key_column_create("id")
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), "id")
  expect_equal(nrow(out), 0)
})


test_that("key_column_create returns correct tibble", {
  out <- key_column_create(
    "iotables_row",
    c("CO2_multiplier", "CH4_multiplier")
  )
  expect_equal(
    out,
    tibble::tibble(iotables_row = c("CO2_multiplier", "CH4_multiplier"))
  )
})

test_that("vector_transpose_wider returns correct forms", {
  data("germany_airpol")

  airpol_wide_1 <- vector_transpose_wider(
    data_table = germany_airpol[, -2],
    names_from = "induse",
    values_from = "value"
  )

  airpol_wide_2 <- vector_transpose_wider(
    data_table = germany_airpol[1:8, 3:4],
    names_from = "induse",
    values_from = "value",
    key_column_values = "CO2_emission",
    key_column_name = "emissions"
  )

  airpol_wide_3 <- vector_transpose_wider(
    data_table = germany_airpol[1:8, -2],
    names_from = "induse",
    values_from = "value",
    key_column_values = "CO2",
    key_column_name = "iotables_rows"
  )

  expect_equal(ncol(airpol_wide_1), 9)
  expect_equal(ncol(airpol_wide_2), 9)
  expect_equal(ncol(airpol_wide_3), 9)

  expect_equal(names(airpol_wide_2)[1], "emissions")
  expect_equal(names(airpol_wide_3)[1], "iotables_rows")
  expect_equal(names(airpol_wide_1)[1], names(germany_airpol)[1])

  expect_equal(as.character(airpol_wide_2[1, 1]), "CO2_emission")
  expect_equal(as.character(airpol_wide_3[1, 1]), "CO2")
})

test_that("vector_transpose_longer drops key column when .keep = FALSE", {
  df <- data.frame(ind = "foo", x = 10, y = 20)
  out <- vector_transpose_longer(df, .keep = FALSE)
  expect_equal(names(out), c("nace_r2", "value"))
})
