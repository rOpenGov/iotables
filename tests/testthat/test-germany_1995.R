hh_consumption <- germany_1995 %>%
  dplyr::filter ( prod_na == "P1") %>%
  dplyr::filter ( iotables_col  == "final_consumption_households" ) %>%
  dplyr::select ( values ) %>%
  unlist () %>% as.numeric ()

test_that("The household consumption value of the Eurostat Manual example dataset is correctly returned.", {
  expect_equal(hh_consumption, 1001060)
})


