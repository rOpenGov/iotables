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
I4 <- sapply ( I[, 2:ncol(I)], function (x) round(x, 4))
I4 <- cbind(I[,1], I4)
I4 <- as.data.frame ( I4)
for (i in 2:ncol(I4)) I4[,i] <- as.numeric(as.character(I4[,i]))
names (I4) <- names ( I )
bw4 <- backward_linkages ( Im = I4 )
bw <-  backward_linkages ( Im = I )


#The Eurostat Manual uses a different rounding. There is a slight mismatch)

test_that("correct data is returned", {
  expect_equal(as.numeric(unlist ( bw[, 2:7] )), 
               c(1.7048,1.8413,1.8136,1.6035,1.5951,1.3782), tolerance=1e-3)
})





