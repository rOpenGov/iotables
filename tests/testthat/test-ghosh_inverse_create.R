test_that("Ghosh inverse agriculture column matches Eurostat example", {
  om <- output_coefficient_matrix_create(
    data_table = iotable_get(),
    total = "tfu"
  )
  G <- ghosh_inverse_create(output_coefficients_matrix = om)

  expect_true(
    all(round(G$agriculture_group, 4) ==
      c(1.0339, 0.0118, 0.0037, 0.0103, 0.0117, 0.0043))
  )
})

test_that("Ghosh inverse preserves key and column names", {
  om <- output_coefficient_matrix_create(iotable_get())
  G <- ghosh_inverse_create(om)

  expect_identical(names(G), names(om))
  expect_equal(ncol(G), ncol(om))
})

test_that("Ghosh inverse is actually the inverse (numeric block)", {
  om <- output_coefficient_matrix_create(iotable_get())
  # Build (I - B) the same way as in the function
  GM <- leontief_matrix_create(technology_coefficients_matrix = om)
  Gm <- as.matrix(GM[, -1, drop = FALSE])

  G <- ghosh_inverse_create(om)
  Gin <- as.matrix(G[, -1, drop = FALSE])

  ident <- Gin %*% Gm
  expect_true(max(abs(ident - diag(nrow(ident)))) < 1e-8)
})

test_that("digits argument rounds numeric part and keeps key", {
  om <- output_coefficient_matrix_create(iotable_get())
  G0 <- ghosh_inverse_create(om)
  G2 <- ghosh_inverse_create(om, digits = 2)

  expect_identical(G0[[1]], G2[[1]]) # key unchanged
  expect_true(all(G2[, -1] == round(G0[, -1], 2))) # rounded values
})
