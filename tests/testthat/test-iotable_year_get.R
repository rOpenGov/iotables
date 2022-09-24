test_that("iotable_year_get() stops on error:", {
  expect_error(iotable_year_get(source = "germany_1995", 
                                geo = 'DE', unit = "MIO_NAC"))
  
 })

test_that("iotable_year_get() returns correct data:", {
  expect_equal(iotable_year_get(source = "germany_1995", 
                           geo = 'DE', unit = "MIO_EUR"),
               as.Date('1995-01-01'))
  
  })


#germany_1995 <- germany_1995 %>% mutate ( time = as.Date("1995-01-01"))
#usethis::use_data(germany_1995, overwrite = TRUE)


