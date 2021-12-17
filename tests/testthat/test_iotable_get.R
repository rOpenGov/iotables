context ("iotable_get()")

test_that("Necessary input parameters are checked", {
  expect_error(iotable_get(source = 'naio_10_cp1700', 
                           year = 2015, geo = NULL,
                           unit = "MIO_EUR", labelling = "iotables"))
  expect_error(iotable_get(source = 'naio_10_cp1701', 
                           year = 2015, geo = "DE",
                           unit = "MIO_EUR", labelling = "iotables"))
})


test_that("Correct data is returned by iotable_get()", {
  expect_equal(iotable_get(source = 'germany_1990', 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = "iotables")[1,2], 1131)
  expect_equal(as.character(iotable_get(source = 'germany_1990', 
                           geo = 'DE', year = 1990, 
                           unit = "MIO_EUR", labelling = 'short')[4,1]), "CPA_G-I")
  expect_equal(as.numeric(iotable_get ( source = "croatia_2010_1800", geo = "HR",
                                        year = 2010, unit = "T_NAC")[1,3]), 
               expected = 164159,  tolerance = 0.6)
  expect_equal(as.numeric(iotable_get ( source = "croatia_2010_1900", geo = "HR",
                                        year = 2010, unit = "T_NAC")[2,5]), 
               expected = 1,  tolerance = 0.5)
  expect_equal(as.character(iotable_get ( source = "croatia_2010_1900", geo = "HR",
                                          year = 2010, unit = "T_NAC", 
                                          labelling = 'short')[[1]][2]), 
               expected = "CPA_A02")
  expect_equal(as.character(iotable_get ( source = "croatia_2010_1900", geo = "HR",
                                          year = 2010, unit = "T_NAC", 
                                          labelling = "iotables")[[1]][2]), 
               expected = "forestry")
  })

#Slovakia A01, A01 shoud be 497.37

#test <- iotable_get ( source = "naio_10_cp1750", stk_flow = "TOTAL",
#                      geo = "CZ", unit = "MIO_NAC", year = 2010, 
#                      data_directory = "data-raw", force_download = FALSE)
# A01, A01 should yield 10,161

germany_table <- iotable_get(source = 'germany_1990', 
            geo = 'DE', year = 1990, 
            unit = "MIO_EUR", labelling = "iotables")

hh <- germany_table$final_consumption_households[which( germany_table$iotables_row == "output")]

test_that("Correct data is returned for private consumption by iotable_get()", {
  expect_equal(hh, 1001060)
})

