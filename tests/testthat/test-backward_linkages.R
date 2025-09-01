test_that("backward_linkages() works on identity matrix", {
  I <- diag(3)
  colnames(I) <- rownames(I) <- c("A", "B", "C")
  I_df <- data.frame(sector = rownames(I), I, check.names = FALSE)

  bw <- backward_linkages(I_df)

  expect_s3_class(bw, "data.frame")
  expect_equal(nrow(bw), 1)
  expect_equal(names(bw), c("sector", "A", "B", "C"))
  expect_equal(as.numeric(bw[1, -1]), c(1, 1, 1))
})

test_that("backward_linkages() returns the correct calculated results", {
  de_coeff <- input_coefficient_matrix_create(
    data_table = iotable_get(),
    digits = 4
  )

  I <- leontief_inverse_create(de_coeff)

  I4 <- sapply(I[, 2:ncol(I)], function(x) round(x, 4)) # sapply b/c unknown dims
  I4 <- cbind(I[, 1], I4)
  I4 <- as.data.frame(I4)
  for (i in 2:ncol(I4)) {
    I4[, i] <- as.numeric(as.character(I4[, i]))
  }
  names(I4) <- names(I)

  bw4 <- backward_linkages(Im = I4)
  bw <- backward_linkages(Im = I)

  # The Eurostat Manual uses slightly different rounding, so a tolerance is applied
  expect_equal(
    as.numeric(unlist(bw[, 2:7])),
    c(1.7048, 1.8413, 1.8136, 1.6035, 1.5951, 1.3782),
    tolerance = 1e-3
  )
})

test_that("backward_linkages() preserves structure", {
  de_coeff <- input_coefficient_matrix_create(iotable_get(), digits = 2)
  I <- leontief_inverse_create(de_coeff)
  bw <- backward_linkages(I)

  expect_s3_class(bw, "data.frame")
  expect_equal(nrow(bw), 1)
  expect_equal(names(bw), names(I))
})
