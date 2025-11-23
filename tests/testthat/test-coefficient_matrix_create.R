test_that("coefficient_matrix_create creates correct coefficients
          from a realistic SIOT subset", {

  small_io <- data.frame(
  prod_na = c(
    "CPA_A", "CPA_B", "CPA_C",
    "CPA_TOTAL",    # end of quadrant I
    "D1",           # primary input 1 (compensation of employees)
    "D29X31",       # primary input 2 (taxes less subsidies)
    "P1"            # output row (denominator)
  ),
  
  # Quadrant I (products × products)
  CPA_A = c(10, 4, 1, 35, 5, 1, 41),
  CPA_B = c( 2, 8, 5, 30, 4, 2, 36),
  CPA_C = c( 3, 2, 6, 30, 3, 1, 34),
  TOTAL   = c(20, 16, 18, NA, NA, NA, NA),
  
  # Quadrant II (for households only)
  P3_S14  = c(100,150,200, NA, NA, NA, NA),
  
  check.names = FALSE)

  small_cm <- coefficient_matrix_create(data_table = small_io)

  ## --- Structure --------------------------------------------------------
  expect_s3_class(small_cm, "data.frame")
  expect_true(all(vapply(small_cm[-1], is.numeric, logical(1))))
  expect_false(any(is.na(small_cm[-1])))

  ## --- Coefficient checks ----------------------------------------
  # denominator row = P1 = c(41, 36, 34)

  # CPA_A row: 10/41, 2/36, 3/34
  expect_equal(round(small_cm[1, "CPA_A"], 3), round(10 / 41, 3))
  expect_equal(round(small_cm[1, "CPA_B"], 3), round(2 / 36, 3))
  expect_equal(round(small_cm[1, "CPA_C"], 3), round(3 / 34, 3))

  # CPA_B row: 4/41, 8/36, 2/34
  expect_equal(round(small_cm[2, "CPA_A"], 3), round(4 / 41, 3))
  expect_equal(round(small_cm[2, "CPA_B"], 3), round(8 / 36, 3))
  expect_equal(round(small_cm[2, "CPA_C"], 3), round(2 / 34, 3))

  # CPA_C row: 1/41, 5/36, 6/34
  expect_equal(round(small_cm[3, "CPA_A"], 3), round(1 / 41, 3))
  expect_equal(round(small_cm[3, "CPA_B"], 3), round(5 / 36, 3))
  expect_equal(round(small_cm[3, "CPA_C"], 3), round(6 / 34, 3))

  ## --- Correct size for returning technology matrix ----------
  small_cm_p <- coefficient_matrix_create(
    data_table = small_io,
    return_part = "products"
  )

  expect_identical(
    names(small_cm_p),
    c("prod_na", "CPA_A", "CPA_B", "CPA_C")
  )

  expect_identical(
    as.character(small_cm_p$prod_na),
    c("CPA_A", "CPA_B", "CPA_C", "CPA_TOTAL")
  )

  ## --- Correct size for returning primary inputs ----------
  small_cm_pi <- coefficient_matrix_create(
    data_table = small_io,
    return_part = "primary_inputs"
  )

  expect_identical(
    names(small_cm_pi),
    c("prod_na", "CPA_A", "CPA_B", "CPA_C")
  )

  expect_identical(
    as.character(small_cm_pi$prod_na),
    c("D1", "D29X31", "P1")
  )

  ## --- Correct size with households  ----------
  small_cm_p_hh <- coefficient_matrix_create(
    data_table = small_io,
    households = TRUE, 
    return_part = NULL,
    digits = 4
  )

  expect_identical(
    names(small_cm_p_hh),
    c("prod_na", "CPA_A", "CPA_B", "CPA_C", "P3_S14")
  )

  expect_identical(
    as.character(small_cm_p_hh$prod_na),
    as.character(small_io$prod_na)
  )
  
  D1_row_expected <- c(5/41, 4/36, 3/34)
  expect_equal(round(D1_row_expected, 4), 
               as.numeric(small_cm_p_hh[5, 2:4]))
  
  
  
  expect_equal(
    as.numeric(small_cm_p_hh$P3_S14),
    round(c(100/41, 150/36, 200/34, 
            sum(c(100/41, 150/36, 200/34)), 0, 0, 0), 4)
  )
})


test_that("coefficient_matrix_create matches Eurostat manual values", {
  # The Eurostat Manual uses a different rounding. There is a slight mismatch.
  cm_de <- coefficient_matrix_create(
    data_table = iotable_get(source = "germany_1995"),
    total = "output",
    digits = 4
  )

  # Manual of Supply, Use and Input–Output Tables,
  # Table 15.4 (Beutel 2008)
  expect_equal(as.numeric(unlist(cm_de[1, 2])),
    0.0258,
    tolerance = 1e-3
  )

  # Primary inputs rows should exist
  cm_inputs <- coefficient_matrix_create(
    iotable_get(),
    return_part = "primary_inputs"
  )
  expect_equal(nrow(cm_inputs), 12)
})

test_that("Households are treated correctly in
          coefficient_matrix_create", {
            
  iot_de <- iotable_get(source = "germany_1995")
  cm_de <- coefficient_matrix_create(
    data_table = iot_de,
    total = "output",
    households = TRUE,
    return_part = NULL,
    digits = 4
  )          
  
  expect_equal(
    ncol(cm_de),
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


test_that("Households are handled correctly in
          coefficient_matrix_create", {
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

test_that("coefficient_matrix_create preserves
          row/column name casing", {
  # A minimal symmetric toy table with precise uppercase CPA names
  small_io <- data.frame(
    prod_na = c("CPA_A01", "forestry", "CPA_A03", "TOTAL", "P1"),
    CPA_A01 = c(1, 2, 3, 6, 1),
    forestry = c(4, 5, 6, 17, 1),
    CPA_A03 = c(7, 8, 9, 24, 1),
    CPA_TOTAL = c(12, 15, 18, 47, 1),
    P1 = c(1, 1, 1, 1, 1),
    check.names = FALSE
  )

  cm <- coefficient_matrix_create(small_io)

  # Check that coefficient matrix still has uppercase names
  expect_identical(names(cm), names(small_io)[1:4])

  # Check that row names in column 1 are preserved in uppercase
  expect_identical(
    as.character(cm$prod_na),
    as.character(small_io$prod_na)
  )

  # Check that the set of row industry codes matches the set of
  # column codes (this confirms that row/column name matching was
  # not broken)
  row_codes <- as.character(cm$prod_na[cm$prod_na != "output"])
  col_codes <- names(cm)[-1] # drop key column

  expect_identical(sort(row_codes[1:3]), sort(col_codes))
})
