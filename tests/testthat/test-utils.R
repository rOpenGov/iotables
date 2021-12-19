
test_that("is_key_column_present", {
  expect_error(is_key_column_present ( data.frame (agriculture = 0.0123,
                                              manufacturing = 0.1436,
                                              trade = 0.0921))
               )
  expect_error(is_key_column_present ( data.frame (indicator = "your_indicator", 
                                                   agriculture = 0.0123,
                                                   manufacturing = 0.1436,
                                                   trade = 0.0921), 
                                       potential_keywords = c("her_indicator", "his_indicator"))
  )
  expect_true(is_key_column_present ( data.frame (indicator = "your_indicator", 
                                                   agriculture = 0.0123,
                                                   manufacturing = 0.1436,
                                                   trade = 0.0921), 
                                       potential_keywords = c("her_indicator", "your_indicator"))
  )
})

