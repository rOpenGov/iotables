test_that("forward_linkages returns expected structure and values", {
  de_out <- output_coefficient_matrix_create(
    data_table = iotable_get(),
    total = "final_demand",
    digits = 4
  )

  fw <- forward_linkages(
    output_coefficient_matrix = de_out,
    digits = 4
  )

  # Structure check
  expect_s3_class(fw, "data.frame")
  expect_equal(ncol(fw), 2)
  expect_equal(names(fw), c(names(de_out)[1], "forward_linkages"))

  # Trivial check: same number of rows as input matrix
  expect_equal(nrow(fw), nrow(de_out))

  # Example values (Eurostat manual, rounding differences tolerated)
  expect_equal(
    round(fw$forward_linkages, 4),
    c(2.1126, 1.6691, 1.3558, 1.5848, 2.1037, 1.2106),
    tolerance = 1e-2
  )
})

test_that("forward_linkages works without rounding", {
  de_out <- output_coefficient_matrix_create(
    data_table = iotable_get(),
    total = "final_demand"
  )

  fw <- forward_linkages(de_out)

  expect_type(fw$forward_linkages, "double")
  expect_false(any(is.na(fw$forward_linkages)))
})
