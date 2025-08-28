test_that("conforming_vector_create preserves structure", {
  de_input_flow <- input_flow_get(data_table = iotable_get())

  out <- conforming_vector_create(de_input_flow)

  expect_equal(ncol(out), ncol(de_input_flow))
  expect_equal(names(out), names(de_input_flow))
  expect_equal(nrow(out), 1)
})

test_that("conforming_vector_create fills values with zeros", {
  df <- data.frame(a = 1:2, b = 3:4)
  out <- conforming_vector_create(df)

  expect_equal(out[1, ], data.frame(a = 0, b = 0))
})

test_that("conforming_vector_create works on single-column data", {
  df <- data.frame(x = 5:6)
  out <- conforming_vector_create(df)

  expect_equal(names(out), "x")
  expect_equal(as.numeric(out[1, ]), 0)
})
