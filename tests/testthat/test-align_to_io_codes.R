test_that("align_to_io_codes works with a minimal fictitious example", {
  # --- Minimal IO table -----------------------------------------------------
  io_example <- data.frame(
    prod_na = c("A", "B", "C31_32"),
    A = c(10, 2, 1),
    B = c(1, 15, 3),
    C31_32 = c(0, 1, 20),
    check.names = FALSE
  )

  # --- External satellite account ------------------------------------------
  # Includes:
  # * indicator column
  # * matching codes (A, B)
  # * mismatched naming (C31_32 → C31_C32)
  # * aggregate TOTAL to be dropped
  ext_example <- data.frame(
    indicator = "GHG_emission",
    A = 0.5,
    B = 0.2,
    `C31-32` = 0.3,
    TOTAL = 0.7,
    check.names = FALSE
  )

  # --- Run alignment --------------------------------------------------------
  aligned <- align_to_io_codes(ext_example, io_example)

  # TEST 1 — Output is a data.frame/tibble of correct dimensions
  expect_s3_class(aligned, "data.frame")
  expect_equal(nrow(aligned), 1)
  expect_equal(ncol(aligned), ncol(io_example))
  expect_equal( # strict name order is expected
    names(aligned),
    c("prod_na", "A", "B", "C31_32")
  )

  # TEST 2 — Correct identifier column: only 'prod_na'
  expect_true("prod_na" %in% names(aligned))
  expect_false("indicator" %in% names(aligned))
  expect_equal(aligned$prod_na, "GHG_emission")

  # TEST 3 — Aggregates (e.g., TOTAL) were dropped
  expect_false("TOTAL" %in% names(aligned))

  # TEST 4 — Naming conventions fixed: C31-C32 → C31_32
  expect_true("C31_32" %in% names(aligned))
  expect_false("C31-32" %in% names(aligned))

  # TEST 5 — IO order preserved (prod_na first, then A, B, C31_32)
  expect_equal(
    names(aligned),
    c("prod_na", "A", "B", "C31_32")
  )

  # TEST 6 — Values mapped correctly
  expect_equal(aligned$A, 0.5)
  expect_equal(aligned$B, 0.2)
  expect_equal(aligned$C31_32, 0.3)
})
