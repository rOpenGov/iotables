test_that("returns expected compensation indicator (package example)", {
  res <- input_indicator_create(
    data_table = iotable_get(),
    input_row = "compensation_employees",
    digits = 4
  )
  # First data cell sanity (legacy tolerance as in original test)
  expect_equal(
    as.numeric(unlist(res[1, 2])),
    0.21,
    tolerance = 4e-3
  )
})

test_that("Beutel/Eurostat GVA indicator matches published vector (tol.)", {
  # Values used previously in the package tests (Eurostat Manual ~p. 498)
  de_gva_indicator <- input_indicator_create(
    data_table = iotable_get(),
    input_row = "gva",
    digits = 4
  )
  # Compare the first 6 industries (vector layout kept)
  expect_equal(
    as.numeric(de_gva_indicator[2:7]),
    c(0.4934, 0.3659, 0.4708, 0.5766, 0.5999, 0.7172),
    tolerance = 4e-3
  )
})

test_that("multiple rows: order preserved and custom names respected", {
  res <- input_indicator_create(
    data_table = iotable_get(),
    input_row = c("gva", "compensation_employees"),
    digits = 3,
    indicator_names = c("GVA indicator", "Income indicator")
  )
  expect_identical(res[[1]], c("GVA indicator", "Income indicator"))
  expect_true(all(vapply(res[-1], is.numeric, logical(1))))
})

test_that("missing input rows warn and present rows returned", {
  expect_warning(
    res <- input_indicator_create(
      data_table = iotable_get(),
      input_row = c("gva", "definitely_not_here"),
      digits = 2
    ),
    regexp = "were not found"
  )
  # Should return only the present one (gva)
  expect_equal(nrow(res), 1L)
})

test_that("households = TRUE does not break structure", {
  res <- input_indicator_create(
    data_table = iotable_get(),
    input_row = "gva",
    households = TRUE
  )
  expect_s3_class(res, "data.frame")
  expect_gte(ncol(res), 2)
})


test_that("input_indicator_create() returns expected compensation data", {
  data_table <- iotable_get()
  expect_equal(
    as.numeric(unlist(input_indicator_create(
      data_table = iotable_get(),
      input_row = "compensation_employees",
      digits = 4
    )[1, 2])),
    expected = 0.21, tolerance = .004
  )
})

test_that("input_indicator_create() returns expected data
          from the Eurostat Manual p498", {
  data_table <- iotable_get()
  de_gva <- primary_input_get(data_table,
    primary_input = "gva"
  )

  de_gva_indicator <- input_indicator_create(
    data_table = data_table,
    input_row = "gva",
    digits = 4
  )
  expect_equal(as.numeric(de_gva_indicator[2:7]),
    expected = c(0.4934, 0.3659, 0.4708, 0.5766, 0.5999, 0.7172),
    tolerance = .004
  )
})
