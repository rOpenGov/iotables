test_that("Necessary input parameters are checked in
          iotable_get_eurostat()", {
  expect_error(iotable_get_eurostat(
    source = "naio_10_cp1701",
    year = 2015, geo = "DE",
    unit = "MIO_EUR", labelling = "iotables"
  ))
})


test_that("iotable_get_eurostat() with
          a product x product dataset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  cz_eurostat_io <- iotable_get_eurostat(
    source = "naio_10_cp1700",
    stk_flow = "TOTAL",
    geo = "CZ",
    unit = "MIO_NAC",
    year = 2015,
    labelling = "iotables",
    data_directory = NULL,
    force_download = TRUE
  )

  # Manually check:
  # CPA_01xCPA_01: 25949
  # CPA_01xCPA_02: 582
  # CPA_01xCPA_02: 297
  # CPA_02xCPA_02: 6356
  
  test_that("Correct values are returned by iotable_get_eurostat()", {
    expect_true(all(cz_eurostat_io$agriculture[c(1,2)]== c(25949, 582)))
    expect_equal(cz_eurostat_io$forestry[1], 297)
    expect_equal(cz_eurostat_io$forestry[2], 6356)
  })
  
  test_that("Correct ordering of the columns", {
    expect_equal(names(cz_eurostat_io)[63], "total")
  })

  test_that("Correct attributes are returned by iotable_get_eurostat()", {
    expect_equal(attr(cz_eurostat_io, "dataset_source"), "naio_10_cp1700")
    expect_equal(attr(cz_eurostat_io, "stk_flow"), "TOTAL")
    expect_equal(attr(cz_eurostat_io, "geo"), "CZ")
    expect_equal(attr(cz_eurostat_io, "unit"), "MIO_NAC")
  })
})

test_that("iotable_get_eurostat() with
          a product x product dataset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  cz_eurostat_io <- iotable_get_eurostat(
    source = "naio_10_cp1700",
    stk_flow = "TOTAL",
    geo = "CZ",
    unit = "MIO_NAC",
    year = 2015,
    labelling = "iotables",
    data_directory = NULL,
    force_download = TRUE
  )

  # Manually check:
  # CPA_01xCPA_01: 25949
  # CPA_01xCPA_02: 582
  # CPA_01xCPA_02: 297
  # CPA_02xCPA_02: 6356
  
  test_that("Correct values are returned by iotable_get_eurostat()", {
    expect_true(all(cz_eurostat_io$agriculture[c(1,2)]== c(25949, 582)))
    expect_equal(cz_eurostat_io$forestry[1], 297)
    expect_equal(cz_eurostat_io$forestry[2], 6356)
  })
  
  test_that("Correct ordering of the columns", {
    expect_equal(names(cz_eurostat_io)[63], "total")
  })

  test_that("Correct attributes are returned by iotable_get_eurostat()", {
    expect_equal(attr(cz_eurostat_io, "dataset_source"), "naio_10_cp1700")
    expect_equal(attr(cz_eurostat_io, "stk_flow"), "TOTAL")
    expect_equal(attr(cz_eurostat_io, "geo"), "CZ")
    expect_equal(attr(cz_eurostat_io, "unit"), "MIO_NAC")
  })
})

test_that("iotable_get_eurostat() with
          a product x product dataset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  cz_eurostat_io <- iotable_get_eurostat(
    source = "naio_10_cp1700",
    stk_flow = "TOTAL",
    geo = "CZ",
    unit = "MIO_NAC",
    year = 2015,
    labelling = "iotables",
    data_directory = NULL,
    force_download = TRUE
  )

  # Manually check:
  # CPA_01xCPA_01: 25949
  # CPA_01xCPA_02: 582
  # CPA_01xCPA_02: 297
  # CPA_02xCPA_02: 6356
  
  test_that("Correct values are returned by iotable_get_eurostat()", {
    expect_true(all(cz_eurostat_io$agriculture[c(1,2)]== c(25949, 582)))
    expect_equal(cz_eurostat_io$forestry[1], 297)
    expect_equal(cz_eurostat_io$forestry[2], 6356)
  })
  
  test_that("Correct ordering of the columns", {
    expect_equal(names(cz_eurostat_io)[63], "total")
  })

  test_that("Correct attributes are returned by iotable_get_eurostat()", {
    expect_equal(attr(cz_eurostat_io, "dataset_source"), "naio_10_cp1700")
    expect_equal(attr(cz_eurostat_io, "stk_flow"), "TOTAL")
    expect_equal(attr(cz_eurostat_io, "geo"), "CZ")
    expect_equal(attr(cz_eurostat_io, "unit"), "MIO_NAC")
  })
})

test_that("iotable_get_eurostat() with a product x product dataset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  cz_eurostat_io <- iotable_get_eurostat(
    source = "naio_10_cp1700",
    stk_flow = "TOTAL",
    geo = "CZ",
    unit = "MIO_NAC",
    year = 2015,
    labelling = "iotables",
    data_directory = NULL,
    force_download = TRUE
  )

  # Manually check:
  # CPA_01xCPA_01: 25949
  # CPA_01xCPA_02: 582
  # CPA_01xCPA_02: 297
  # CPA_02xCPA_02: 6356
  
  test_that("Correct values are returned by iotable_get_eurostat()", {
    expect_true(all(cz_eurostat_io$agriculture[c(1,2)]== c(25949, 582)))
    expect_equal(cz_eurostat_io$forestry[1], 297)
    expect_equal(cz_eurostat_io$forestry[2], 6356)
  })
  
  test_that("Correct ordering of the columns", {
    expect_equal(names(cz_eurostat_io)[63], "total")
  })

  test_that("Correct attributes are returned by iotable_get_eurostat()", {
    expect_equal(attr(cz_eurostat_io, "dataset_source"), "naio_10_cp1700")
    expect_equal(attr(cz_eurostat_io, "stk_flow"), "TOTAL")
    expect_equal(attr(cz_eurostat_io, "geo"), "CZ")
    expect_equal(attr(cz_eurostat_io, "unit"), "MIO_NAC")
  })
})

test_that("iotable_get_eurostat() with a product x product dataset
          with short coded labelling", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  
  cz_eurostat_io_short <- iotable_get_eurostat(
    source = "naio_10_cp1700",
    stk_flow = "TOTAL",
    geo = "CZ",
    unit = "MIO_NAC",
    year = 2015,
    labelling = "short",
    data_directory = NULL,
    force_download = TRUE
  )
  
  # Manually check:
  # CPA_01xCPA_01: 25949
  # CPA_01xCPA_02: 582
  # CPA_01xCPA_02: 297
  # CPA_02xCPA_02: 6356
  
  test_that("Correct values are returned by iotable_get_eurostat()", {
    expect_true(all(cz_eurostat_io_short$CPA_A01[c(1,2)]== c(25949, 582)))
  })
  
  test_that("Correct ordering of the columns", {
    expect_equal(names(cz_eurostat_io_short)[63], "TOTAL")
  })
  
  test_that("Correct attributes are returned by iotable_get_eurostat()", {
    expect_equal(attr(cz_eurostat_io_short, "dataset_source"), "naio_10_cp1700")
  })
})

test_that("manual Eurostat download works for a real dataset", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  cz_iop <- iotable_get_eurostat(
    source = "naio_10_cp1750",
    stk_flow = "TOTAL",
    geo = "CZ",
    unit = "MIO_NAC",
    year = 2015,
    data_directory = NULL,
    force_download = TRUE
  )

  # Manually check:
  # CPA_01: c(24600,542,36)
  # CPA_02: c(1039, 6740,1)

  test_that("Correct values are returned by iotable_get_eurostat()", {
    expect_true(all(cz_iop$agriculture[c(1,2, 3)]==  c(24600,542,36)))
    expect_equal(cz_iop$forestry[c(1:3)], c(1039, 6740,1))
  })
  
  test_that("Correct attributes are returned by iotable_get_eurostat()", {
    expect_equal(attr(cz_iop, "dataset_source"), "naio_10_cp1750")
    expect_equal(attr(cz_iop, "stk_flow"), "TOTAL")
    expect_equal(attr(cz_iop, "geo"), "CZ")
    expect_equal(attr(cz_iop, "unit"), "MIO_NAC")
  })
})



