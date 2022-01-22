library (iotables) ; library (dplyr) ; library (devtools)
load_all()
source = "germany_1990"
geo <- geo_input <- "DE"
year = 1990
unit <- unit_input <- "MIO_EUR"
households = FALSE
labelling = "iotables"
de_use <- use_table_get ( source = "germany_1990", geo = "DE",
           year = 1990, unit = "MIO_EUR", 
           households = FALSE, labelling = "iotables")

de_output <- output_get ( source = "germany_1990", geo = "DE",
            year = 1990, unit = "MIO_EUR",
            households = FALSE, labelling = "iotables")

L_de <- leontief_matrix_create(de_coeff)
I_de <- leontief_inverse_create(L_de)

de_emp <- primary_input_get ( input = "employment_total",
                              source = "germany_1990", geo = "DE",
                              year = 1990,  
                              households = FALSE, labelling = "iotables")

de_emp_input <- input_indicator_create(de_emp, de_output)
multiplier_create ( de_emp_input, I_de, "hello", digits = 4)
round(as.matrix(de_emp_input[,2:7]) %*% as.matrix(I_de[,2:7]),4)



de_emissions <- readxl::read_excel(path = 
      "not_included/germany_emissions.xlsx")
de_demand <-  t(de_output[2:ncol(de_output)]) - rowSums(de_use[,2:ncol(de_use)]) 
diag_demand <- diag ( as.vector(t(de_demand)), nrow(de_demand), nrow(de_demand) )
de_coeff <- input_coefficient_matrix_create( de_use, de_output, digits = 4)
de_emp_coeff <- input_coefficient_matrix_create( de_use, de_emp, digits = 4)
bi <- as.matrix(de_emp_coeff[,2:ncol(de_emp_coeff)]) %*% as.matrix(I_de [,2:ncol(I_de)])

employment_coefficients <- de_emp
for (i in 2:ncol(employment_coefficients )) {
  employment_coefficients [,i] <- (employment_coefficients [,i]  / as.numeric(de_output[i]) )}

as.matrix(emission_coefficients[,2:7])%*% as.matrix(I_de[,2:7])%*%diag(ger$output_bp[1:6] - rowSums(ger[1:6,2:7]))

as.matrix(employment_coefficients[,2:7]) %*% as.matrix(I_de[,2:7])



