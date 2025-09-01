test_that("primary_input_get errors nicely on bad inputs", {
  siot <- iotable_get(
    source = "germany_1995", geo = "DE", year = 1990,
    unit = "MIO_EUR", labelling = "short"
  )

  expect_error(
    primary_input_get(NULL, "D1"),
    "No input-output table"
  )
  expect_error(
    primary_input_get(siot, "not_a_label"),
    "not found"
  )
  expect_error(
    primary_input_get(siot, c("D1", "D2")),
    "single character"
  )
})


test_that("Retrieving a primary input vector with primary_input_get()", {
  siot <- iotable_get(
    source = "germany_1995",
    geo = "DE", year = 1990,
    unit = "MIO_EUR",
    labelling = "short"
  )

  d1_row <- primary_input_get(siot, "D1")

  # Check that the first column is "D1"
  expect_equal(as.character(d1_row[[1]]), "D1")
  expect_equal(d1_row[["CPA_B-E"]], 296464) # industry group

  # K1: consumption of fixed capital
  k1 <- primary_input_get(siot, "K1")
  expect_equal(k1$prod_na, "K1")
  expect_equal(k1[["CPA_B-E"]], 63769)

  # B2A3N: operating surplus and mixed income
  b2a3n <- primary_input_get(siot, "B2A3N")
  expect_equal(b2a3n$prod_na, "B2A3N")
  expect_equal(b2a3n[["CPA_B-E"]], 33332)
})
