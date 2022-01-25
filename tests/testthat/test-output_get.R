output_vector <- output_get(iotable_get())

test_that("correct output vector is returned by output_get()", {
  expect_equal(as.character(output_get(iotable_get())[[1]][1]), "output")
  })

#more tests needed