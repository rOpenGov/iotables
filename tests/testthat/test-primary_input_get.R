

test_that("Retreieving a primary input vector with primary_input_get()", {
  siot <- iotable_get(
    source = "germany_1995",
    geo = "DE", year = 1990,
    unit = "MIO_EUR",
    labelling = "short"
  )
  
  expect_equal(as.character(unlist(primary_input_get(
    data_table = siot,
    primary_input = "D1"
  )))[3], "296464")
})


test_that("primary_input_get errors nicely on bad inputs", {
  siot <- iotable_get(source = "germany_1995", geo = "DE", year = 1990,
                      unit = "MIO_EUR", labelling = "short")
  
  expect_error(primary_input_get(NULL, "D1"),
               "No input-output table")
  expect_error(primary_input_get(siot, "not_a_label"),
               "not found")
  expect_error(primary_input_get(siot, c("D1", "D2")),
               "single character")
})

