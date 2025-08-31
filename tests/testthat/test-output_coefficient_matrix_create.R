test_that("output_coefficient_matrix_create returns a well-formed table", {
  dt <- iotable_get()
  out <- output_coefficient_matrix_create(dt, total = "tfu")

  expect_s3_class(out, "data.frame")
  expect_gte(ncol(out), 2)
  # key column preserved
  expect_identical(names(out)[1], names(dt)[1])
  # all numeric columns except the key
  expect_true(all(vapply(out[-1], is.numeric, logical(1))))
})


test_that("rounding applies only when digits is provided", {
  dt <- iotable_get()
  out0 <- output_coefficient_matrix_create(dt, total = "tfu")
  out2 <- output_coefficient_matrix_create(dt, total = "tfu", digits = 2)

  # key column identical
  expect_identical(out0[[1]], out2[[1]])
  # rounded equals manual rounding (except the epsilon 1e-6 guard)
  round_eps <- function(x, digits) ifelse(x == 1e-6, x, round(x, digits))
  manual <- as.data.frame(lapply(out0[-1], round_eps, digits = 2))
  expect_equal(out2[-1], manual)
})

# Optional: Beutel / Germany 1995 check (only if you have published targets)
test_that("Germany 1995 output coefficients basic sanity (Beutel context)", {
  # This is a weak regression test without hard-coded published numbers.
  # Replace with known target values if you have them.
  dt <- iotable_get(source = "germany_1995")
  out <- output_coefficient_matrix_create(dt, total = "tfu")

  # basic checks: dimensions > 0, coefficients in [0, +Inf) (TFU can be 0→eps)
  expect_gt(nrow(out), 0)
  expect_gt(ncol(out), 2)
  num <- as.matrix(out[, -1, drop = FALSE])
  expect_true(all(is.finite(num)))
  expect_true(all(num >= 0))
})

test_that("correct output coefficients are returned", {
  io_table <- iotable_get()
  output_coefficients <- output_coefficient_matrix_create(
    data_table = io_table, "final_demand", digits = 4
  )

  business_agriculture_input <- output_coefficients %>%
    dplyr::filter(iotables_row == "business_services_group") %>%
    dplyr::select(agriculture_group) %>%
    as.numeric(.)

  BSBS <- output_coefficients %>%
    dplyr::filter(iotables_row == "business_services_group") %>%
    dplyr::select(business_services_group) %>%
    as.numeric(.)
  expect_equal(business_agriculture_input, 0.0053, tolerance = .00005)
  expect_equal(BSBS, 0.2790, tolerance = .00005)
})
