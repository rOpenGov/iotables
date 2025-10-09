test_that("airpol_get works with germany_1995 replication dataset", {
  df <- airpol_get(
    airpol = "CO2",
    geo = "germany_1995",
    year = 1995
  )

  expect_s3_class(df, "data.frame")
  expect_true("indicator" %in% names(df))
  expect_true("CO2_emission" %in% df$indicator)
})

test_that("manual Eurostat download works for a real dataset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip_if(
    Sys.getenv("RUN_MANUAL_IOTABLES_TESTS") != "true",
    "Set RUN_MANUAL_IOTABLES_TESTS=true to run this manually."
  )
  
  airpol_df <- airpol_get(
    airpol = "GHG",
    geo = "HU",
    year = 2020,
    unit = "THS_T",
    data_directory = NULL,
    force_download = FALSE
  )
  
  test_that("Correct attributes are present", {
    expect_equal(attr(airpol_df, "geo"), "HU")
    expect_equal(attr(airpol_df, "year"), 2020L)
    expect_equal(attr(airpol_df, "unit"), "THS_T")
  })
  
  # Check manually on
  # https://ec.europa.eu/eurostat/databrowser/view/
  # env_ac_ainah_r2__custom_18274018/default/table
  
  test_that("Correct amount returned", {
    expect_equal(round(airpol_df$A01, 2), 9509.31)
  })
  
  test_that("Grouped industry data is present", {
    expect_true(all(c("A", "C", "C", "D", "E", 
                      "F", "G", "H", "J", "K", "L", 
                      "M", "N", "O", "P", "Q", "R", 
                      "S", "T", "U") %in% names(airpol_df)))
  })
})


test_that("manual Eurostat download works for a real dataset", {
  skip_on_cran()
  skip_if_offline()

  airpol_it <- airpol_get(
    airpol = "GHG",
    geo = "IT",
    year = 2020,
    unit = "THS_T",
    data_directory = NULL,
    force_download = TRUE
  )
  
  test_that("Correct attributes are present", {
    expect_equal(attr(airpol_it, "geo"), "IT")
    expect_equal(attr(airpol_it, "year"), 2020L)
    expect_equal(attr(airpol_it, "unit"), "THS_T")
  })
  
  # Check manually on
  # https://ec.europa.eu/eurostat/databrowser/view/
  # env_ac_ainah_r2__custom_18274018/default/table
  
  test_that("Correct amount returned", {
    expect_equal(round(airpol_it$A01, 2), 41975.13)
    expect_equal(round(airpol_it$A02, 2), 65.69)
  })
  
  test_that("Grouped industry data is present", {
    expect_true(all(c("A", "C", "C", "D", "E", 
                      "F", "G", "H", "J", "K", "L", 
                      "M", "N", "O", "P", "Q", "R", 
                      "S", "T", "U") %in% names(airpol_it)))
  })
})

