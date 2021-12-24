context ("Creating an output vector")

output_vector <- output_get(iotable_get())

test_that("correct data is returned", {
  expect_equal(as.character(output_get(iotable_get())[[1]][1]), "output")
  })

#more tests needed