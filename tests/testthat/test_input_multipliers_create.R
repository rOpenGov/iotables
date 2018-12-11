library (testthat)
library (iotables)
library (dplyr)

data_table <- iotable_get ()
context ("Creating input multipliers")
direct_effects_de <- coefficient_matrix_create(data_table = data_table, 
                                              total = 'output', 
                                              return = 'primary_inputs') 
coeff_de <- input_coefficient_matrix_create( data_table )

I_de <- leontieff_inverse_create( coeff_de )

multipliers <- input_multipliers_create(
 direct_effects = direct_effects_de,
 inverse = I_de)


value_added_row <- multipliers [ direct_effects_de[,1] == "gva", ]

emp_row <- which ( multipliers[,1] == "compensation_employees")
agr_col <- which ( names(multipliers) == "agriculture_group")

agr_employees <- multipliers [emp_row, agr_col ]


test_that("correct data is returned", {
  expect_equal(as.numeric (agr_employees), 0.4172, tolerance=1e-3)
  expect_equal(as.numeric(value_added_row[2:7]), 
               c(0.845,0.7647,0.8615, 0.9019,0.9393, 0.9199), 
               tolerance=1e-3)
})



###Netherlands

nl_input_flow_2 <- input_flow_get  ( netherlands_2006,
                             households = TRUE)

nl_coeff_2   <- input_coefficient_matrix_create(
     data_table = netherlands_2006,
     households = TRUE, 
     digits = NULL)



#Check against p12
test_that("correct data is returned from netherlands_2006", {
  expect_equal(round(nl_coeff_2 [,2],2), 
               c(0.12, 0, 0.15, 0.04, 0.01, 0.13, 0.11), 
               tolerance=1e-3)
})


later <- function(x) {
  nl_coeff_2$final_consumption_households <- c(0, 0, 0.08, 0.03, 0, 0.59, 0) #for the time being
  
  View  (nl_coeff_2)
  
  I_nl_2 <- leontieff_inverse_create(technology_coefficients_matrix =
                                       nl_coeff_2)
  
  direct_effects_nl_2 <- coefficient_matrix_create(data_table = netherlands_2006, 
                                                   total = 'output', 
                                                   households = TRUE,
                                                   return = 'primary_inputs')
  
  income_nl_effects_2 <- direct_effects_nl_2 %>%
    dplyr::filter_at ( dplyr::vars(1), any_vars (. == "compensation_employees") )
  
  income_effects_2_published <- c(0.3887,
                                  0.1456,
                                  0.4515,
                                  0.3124,
                                  0.6858,
                                  0.7279)
  
  income_multipliers_2_published  <- c(
    3.6377,
    3.4423,
    2.7144,
    3.4996,
    2.8813,
    2.0907)
  
  
  nl_income_mulitplier_2 <- income_multiplier_create (
    
  )
  nl_income_multiplier_2 <- income_nl_multipliers_2[ , 2:7] /income_indicator_nl_2[ , 2:7]
  
  test_that("correct data is returned from netherlands_2006", {
    expect_equal(round(as.numeric (I_nl_2[2,2:8]),3), 
                 c(0.024, 1.028, 0.024, 0.300, 0.018, 0.014, 0.019), tolerance=1e-3)
    expect_equal(as.numeric(income_nl_effects_2[,2:7]), 
                 income_effects_2_published, 
                 tolerance=1e-3)
    expect_equal(as.numeric(nl_income_multiplier_2), 
                 income_multipliers_2_published, 
                 tolerance=1e-3)
  })
}





