library (testthat)
library (iotables)
require (dplyr)
context ("Eurostat Manual example")

hh_consumption <- germany_1990 %>%
  dplyr::filter ( prod_na == "P1") %>%
  dplyr::filter ( iotables_col  == "final_consumption_households" ) %>%
  dplyr::select ( values ) %>%
  unlist () %>% as.numeric ()

test_that("correct data is recorded", {
  expect_equal(hh_consumption, 1001060)
})


