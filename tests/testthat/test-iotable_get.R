test_that("Necessary input parameters are checked in iotable_get()", {
  expect_error(iotable_get(
    source = "naio_10_cp1701",
    year = 2015, geo = "DE",
    unit = "MIO_EUR", labelling = "iotables"
  ))
})

test_that("Correct data is returned by iotable_get()", {
  expect_equal(as.numeric(iotable_get(
    source = "germany_1995",
    geo = "DE", year = 1995,
    unit = "MIO_EUR", labelling = "iotables"
  )[1, 2]), 1131)
  expect_equal(as.character(unlist(iotable_get(
    source = "germany_1995",
    geo = "DE", year = 1995,
    unit = "MIO_EUR", labelling = "short"
  )[4, 1])), "CPA_G-I")
  expect_equal(
    as.numeric(iotable_get(
      source = "croatia_2010_1800", geo = "HR",
      year = 2010, unit = "T_NAC"
    )[1, 3]),
    expected = 164159, tolerance = 0.6
  )
  expect_equal(
    as.numeric(iotable_get(
      source = "croatia_2010_1900", geo = "HR",
      year = 2010, unit = "T_NAC"
    )[2, 5]),
    expected = 1, tolerance = 0.5
  )
  expect_equal(
    as.character(unlist(iotable_get(
      source = "croatia_2010_1900", geo = "HR",
      year = 2010, unit = "T_NAC",
      labelling = "short"
    )[2, 1])),
    expected = "CPA_A02"
  )
  expect_equal(
    as.character(unlist(iotable_get(
      source = "croatia_2010_1900", geo = "HR",
      year = 2010, unit = "T_NAC",
      labelling = "iotables"
    )[2, 1])),
    expected = "forestry"
  )
})


run_only_manually <- function() {
  # This test is too time and resource consuming for automatically 
  # running on CRAN.
  test <- iotable_get(
    source = "naio_10_cp1750", stk_flow = "TOTAL",
    geo = "CZ", unit = "MIO_NAC", year = 2010,
    data_directory = "data-raw", force_download = FALSE
  )

  test_that("Correct data is returned by iotable_get()", {
    expect_equal(as.numeric(test[1, 2]), 10161)
  })
}

test_that("Correct data is returned for private consumption 
          by iotable_get()", {
  germany_table <- iotable_get(
    source = "germany_1995",
    geo = "DE", year = 1995,
    unit = "MIO_EUR", labelling = "iotables"
  )
  
  hh <- germany_table$final_consumption_households[which(germany_table$iotables_row == "output")]
  expect_equal(hh, 1001060)
})


test_that("labelling switches shape of first column as documented", {
  de_iot <- iotable_get(
    source = "germany_1995",
    geo = "DE", year = 1995, unit = "MIO_EUR",
    labelling = "iotables"
  )
  de_short <- iotable_get(
    source = "germany_1995",
    geo = "DE", year = 1995, unit = "MIO_EUR",
    labelling = "short"
  )
  expect_identical(names(de_iot)[1], "iotables_row")
  expect_true(names(de_short)[1] %in% c("prod_na", "t_rows2"))
})

