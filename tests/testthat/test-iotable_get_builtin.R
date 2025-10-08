test_that("Necessary input parameters are checked in iotable_get_builtin()", {
  expect_error(iotable_get_builtin(
    source = "naio_10_cp1701",
    year = 2015, geo = "DE",
    unit = "MIO_EUR", labelling = "iotables"
  ))
})

test_that("Correct data is returned by iotable_get_builtin()", {
  expect_equal(as.numeric(iotable_get_builtin(
    source = "germany_1995",
    geo = "DE", year = 1995,
    unit = "MIO_EUR", labelling = "iotables"
  )[1, 2]), 1131)

  expect_equal(as.character(unlist(iotable_get_builtin(
    source = "germany_1995",
    geo = "DE", year = 1995,
    unit = "MIO_EUR", labelling = "short"
  )[4, 1])), "CPA_G-I")
  expect_equal(
    as.numeric(iotable_get_builtin(
      source = "croatia_2010_1800", geo = "HR",
      year = 2010, unit = "T_NAC"
    )[1, 3]),
    expected = 164159, tolerance = 0.6
  )
  expect_equal(
    as.numeric(iotable_get_builtin(
      source = "croatia_2010_1900", geo = "HR",
      year = 2010, unit = "T_NAC"
    )[2, 5]),
    expected = 1, tolerance = 0.5
  )
  expect_equal(
    as.character(unlist(iotable_get_builtin(
      source = "croatia_2010_1900", geo = "HR",
      year = 2010, unit = "T_NAC",
      labelling = "short"
    )[2, 1])),
    expected = "CPA_A02"
  )
  expect_equal(
    as.character(unlist(iotable_get_builtin(
      source = "croatia_2010_1900", geo = "HR",
      year = 2010, unit = "T_NAC",
      labelling = "iotables"
    )[2, 1])),
    expected = "forestry"
  )
})


test_that("Correct data is returned for private consumption
          by iotable_get_builtin()", {
  germany_table <- iotable_get_builtin(
    source = "germany_1995",
    geo = "DE", year = 1995,
    unit = "MIO_EUR", labelling = "iotables"
  )

  hh <- germany_table$final_consumption_households[which(germany_table$iotables_row == "output")]
  expect_equal(hh, 1001060)
})


test_that("labelling switches shape of first column as documented", {
  de_iot <- iotable_get_builtin(
    source = "germany_1995",
    geo = "DE", year = 1995, unit = "MIO_EUR",
    labelling = "iotables"
  )
  de_short <- iotable_get_builtin(
    source = "germany_1995",
    geo = "DE", year = 1995, unit = "MIO_EUR",
    labelling = "short"
  )
  expect_identical(names(de_iot)[1], "iotables_row")
  expect_true(names(de_short)[1] %in% c("prod_na", "t_rows2"))
})


# Run this test only if manually approved
Sys.setenv(RUN_MANUAL_IOTABLES_TESTS = "false")

test_that("manual Eurostat download works for a real dataset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if(
    Sys.getenv("RUN_MANUAL_IOTABLES_TESTS") != "true",
    "Manual test; set RUN_MANUAL_IOTABLES_TESTS=true to enable."
  )

  test <- iotable_get_builtin(
    source = "naio_10_cp1750", stk_flow = "TOTAL",
    geo = "CZ", unit = "MIO_NAC", year = 2010,
    data_directory = "data-raw", force_download = FALSE
  )

  test_that("Correct data is returned by iotable_get_builtin()", {
    expect_equal(as.numeric(test[1, 2]), 10161)
  })
})
