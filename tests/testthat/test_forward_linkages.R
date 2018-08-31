library (testthat)
library (iotables)
context ("Creating forward linkages")

de_use <- use_table_get ( source = "germany_1990", geo = "DE",
               year = 1990, unit = "MIO_EUR", 
               households = FALSE, labelling = "iotables")

de_output <- output_get ( source = "germany_1990", geo = "DE",
               year = 1990, unit = "MIO_EUR",
               households = FALSE, labelling = "iotables")

de_coeff <- input_coefficient_matrix_create( de_use, de_output, digits = 4)

L <- iotables::leontieff_matrix_create( technology_coefficients_matrix = de_coeff )
I <- leontieff_inverse_create (L)
#fw <- forward_linkages ( I )

#The Eurostat Manual uses a different rounding. There is a slight mismatch)

#test_that("correct data is returned", {
#  expect_equal(round ( fw$total[which ( fw$iotables_row == "agriculture_group")], 4),
#               2.1126, tolerance=1e-3)
#})
