siot <- iotable_get(source = "germany_1995", 
                    geo = 'DE', year = 1990, 
                    unit = "MIO_EUR",
                    labelling = "short")

test_that("Retreieving a primary input vector with primary_input_get()", {
  expect_equal(as.character(unlist(primary_input_get(data_table = siot,
                                                     primary_input = "D1")))[3], "296464" )
})




