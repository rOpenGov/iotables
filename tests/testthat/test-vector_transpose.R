test_that("Correct form returned", {
  expect_equal(names(vector_transpose ( data.frame (indicator = "my_inidcator", 
                                                    agriculture = 0.0123,
                                                    manufacturing = 0.1436,
                                                    trade = 0.0921), 
                                        .keep = TRUE)), c("indicator", "nace_r2", "value"))
  expect_equal(names(vector_transpose ( data.frame (indicator = "my_inidcator", 
                                                    agriculture = 0.0123,
                                                    manufacturing = 0.1436,
                                                    trade = 0.0921), 
                                        .keep = FALSE)), c("nace_r2", "value"))
})


