data_table <- iotable_get()

test_that("input_indicator_create() returns expected compensation data", {
  expect_equal(as.numeric(unlist(input_indicator_create( data_table = iotable_get(), 
                                                         input_row = "compensation_employees",
                                                         digits = 4)[1,2])), 
               expected = 0.21, tolerance = .004)
})

de_gva <- primary_input_get ( data_table,
                              primary_input = "gva") 

de_gva_indicator <- input_indicator_create(data_table  = data_table,
                                           input_row = "gva", 
                                           digits = 4)

test_that("input_indicator_create() returns expected data from the Eurostat Manual p498", {
  expect_equal(as.numeric(de_gva_indicator[2:7]), 
               expected = c(0.4934, 0.3659, 0.4708, 0.5766, 0.5999, 0.7172),
               tolerance = .004)
})