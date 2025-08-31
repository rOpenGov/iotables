test_that("output_multiplier_create returns correct structure", {
  siot <- iotable_get(source = "germany_1995", 
                      geo = "DE", year = 1990,
                      unit = "MIO_EUR", labelling = "short")
  coeff <- input_coefficient_matrix_create(siot)
  
  result <- output_multiplier_create(coeff)
  
  # Structure
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("output_multipliers" %in% result[[1]])
  
  # Names: first is key, others match coeff column names
  expect_equal(names(result)[-1], names(coeff)[-1])
  
  # Values are column sums of Leontief inverse
  inv <- leontief_inverse_create(coeff)
  expected <- colSums(inv[, -1, drop = FALSE])
  expect_equal(
    unname(as.numeric(result[1, -1])),
    unname(expected)
  )
  expect_equal(names(result)[-1], names(expected))
})

test_that("output_multiplier_create works with tiny toy matrix", {
  coeff <- data.frame(
    key = c("a", "b"),
    a = c(0.1, 0.2),
    b = c(0.3, 0.1)
  )
  result <- output_multiplier_create(coeff)
  expect_equal(nrow(result), 1)
  expect_equal(result[[1]], "output_multipliers")
})
