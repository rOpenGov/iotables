test_nested_data <- data.frame(
  unit = "MIO_NAC", stk_flow = "TOTAL", geo = "HU", time = as.Date("2015-01-01"), year = 2015,
  unit_lab = "Million units of national currency", stk_flow_lab = "Total",
  geo_lab = "Hungary", values = c(1, 2), nace_r2 = c("A", "B")
)

test_nested_data <- tidyr::nest(test_nested_data, data = c("values", "nace_r2"))

test_that("get_saved_table finds exceptions", {
  expect_error(find_saved_table(test_nested_data, geo = "HU", year = 2015, unit = "MIO_NAC", stk_flow = "DOM"))
  expect_error(find_saved_table(test_nested_data, geo = "HU", year = 2015, unit = "M_EUR", stk_flow = "TOTAL"))
  expect_error(find_saved_table(test_nested_data, geo = "HU", year = 2016, unit = "MIO_NAC", stk_flow = "TOTAL"))
  expect_error(find_saved_table(test_nested_data, geo = "AD", year = 2015, unit = "MIO_NAC", stk_flow = "TOTAL"))
  expect_equal(ncol(test_nested_data), ncol(
    find_saved_table(test_nested_data, geo = "HU", year = 2015, unit = "MIO_NAC", stk_flow = "TOTAL")
  ))
})

test_that("get_package_iots works", {
  expect_equal(unique(get_package_iots("germany_1995")$geo), "DE")
})
