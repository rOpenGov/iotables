test_that("correct data is returned by coefficient_matrix_create()", {
  small_io <- data.frame(
    prod_na = c("CPA_A", "CPA_B", "CPA_C", "output"),
    CPA_A   = c(10, 4, 1, 35),
    CPA_B   = c(2, 8, 5, 30),
    CPA_C   = c(3, 2, 6, 30),
    total   = c(20, 16, 18, NA_real_)
  )

  small_cm <- coefficient_matrix_create(small_io)

  # Structure checks
  expect_s3_class(small_cm, "data.frame")
  expect_true(all(vapply(small_cm[-1], is.numeric, logical(1))))
  expect_false(any(is.na(small_cm[-1])))

  # Check correct number of rows and columns
  expect_equal(nrow(small_cm), 4)
  expect_equal(ncol(small_cm), 4)

  # Verify a few known ratios
  expect_equal(round(small_cm[1, "CPA_A"], 3), round(10 / 35, 3))
  expect_equal(round(small_cm[2, "CPA_B"], 3), round(8 / 30, 3))
  expect_equal(round(small_cm[3, "CPA_C"], 3), round(6 / 30, 3))
})


test_that("coefficient_matrix_create matches Eurostat manual values", {
  # The Eurostat Manual uses a different rounding. There is a slight mismatch.
  cm_de <- coefficient_matrix_create(
    data_table = iotable_get(source = "germany_1995"),
    total = "output",
    digits = 4
  )

  # Manual of Supply, Use and Input–Output Tables, Table 15.4 (Beutel 2008)
  expect_equal(as.numeric(unlist(cm_de[1, 2])),
    0.0258,
    tolerance = 1e-3
  )

  # Primary inputs rows should exist
  cm_inputs <- coefficient_matrix_create(
    iotable_get(),
    return_part = "primary_inputs"
  )
  expect_equal(nrow(cm_inputs), 13)
})

test_that("Households are treated correctly in coefficient_matrix_create", {
  expect_equal(
    ncol(coefficient_matrix_create(
      data_table = iotable_get(source = "germany_1995"),
      total = "output",
      households = TRUE,
      digits = 4
    )),
    8
  )
  expect_equal(
    ncol(coefficient_matrix_create(
      data_table = iotable_get(source = "germany_1995"),
      total = "output",
      digits = 4
    )),
    7
  )
})


test_that("Households are handled correctly in coefficient_matrix_create", {
  cm_with_hh <- coefficient_matrix_create(
    data_table = iotable_get(source = "germany_1995"),
    total = "output",
    households = TRUE,
    digits = 4
  )

  cm_no_hh <- coefficient_matrix_create(
    data_table = iotable_get(source = "germany_1995"),
    total = "output",
    digits = 4
  )

  # Difference should be exactly one column
  expect_equal(ncol(cm_with_hh) - ncol(cm_no_hh), 1)

  # Both should have numeric-only columns beyond the key
  expect_true(all(vapply(cm_with_hh[-1], is.numeric, logical(1))))
  expect_true(all(vapply(cm_no_hh[-1], is.numeric, logical(1))))
})
