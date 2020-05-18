## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(iotables)
require(dplyr)

## ----download, eval=FALSE-----------------------------------------------------
#  #Not run
#  not_included_directory <- file.path('..', 'not_included')
#  if ( ! dir.exists(not_included_directory) ) dir.create (not_included_directory)
#  #The contents of the 'not_included' directory can be found on GitHub,
#  #but they are not released and distributed with the package.
#  
#  naio_10_cp1700 <- iotables_download(
#    "naio_10_cp1700", #SIOT
#    data_directory = not_included_directory)
#  
#  # For inclusion in the package, the files must be smaller.
#  # Reducing the size of the bulk files will not affect
#  # the demonstration.
#  
#  naio_10_cp1700 <- naio_10_cp1700 %>%
#    dplyr::filter ( geo %in% c("CZ", "SK")) %>%
#    dplyr::filter ( year %in% c(2010, 2015))
#  
#  #Conforming employment data both sexes from 15 years old, year 2015.
#  #prod_na vocabulary for product x product conformity
#  emp_cz <- employment_get(geo = "CZ", year = "2015", sex = "Total",
#    age = "Y_GE15", labelling = "prod_na",
#    data_directory = not_included_directory,
#    force_download = TRUE)
#  
#  #Conforming employment data #both sexes from 15 years old, year 2017.
#  emp_sk <- employment_get(geo = "SK", year = "2017", sex = "Total",
#    age = "Y_GE15", labelling = "prod_na",
#    data_directory = not_included_directory,
#    force_download = TRUE)
#  
#  save (naio_10_cp1700, emp_sk, emp_cz,
#        file = file.path('..', 'inst', 'extdata',
#                         'naio_10_product_x_product.rda'))

## ----load---------------------------------------------------------------------
#load from pre-saved file to increase running speed

load (system.file('extdata', 
                  'naio_10_product_x_product.rda', 
                  package = 'iotables') )


## ----preprocess---------------------------------------------------------------
cz_io <-  iotable_get ( labelled_io_data = naio_10_cp1700, 
                         source = "naio_10_cp1700", geo = "CZ", 
                         year = 2015, unit = "MIO_NAC", 
                         stk_flow = "TOTAL",
                         labelling = "short" )

sk_io <-  iotable_get ( labelled_io_data = naio_10_cp1700, 
                        source = "naio_10_cp1700", geo = "SK", 
                        year = 2010, unit = "MIO_EUR", 
                        stk_flow = "TOTAL",
                        labelling = "short" )

cz_input_flow <- input_flow_get( data_table = cz_io )

sk_input_flow <- input_flow_get( data_table = sk_io)

cz_output <- output_get( data_table = cz_io)
sk_output <- output_get( data_table = sk_io)

## ----inputcoeff, results='asis'-----------------------------------------------
input_coeff_matrix_cz <- input_coefficient_matrix_create(
  data_table = cz_io
)

input_coeff_matrix_sk <- input_coefficient_matrix_create(
  data_table = sk_io
)

knitr::kable(head(input_coeff_matrix_cz[,1:8]))

## ----leontieff, results='asis'------------------------------------------------
L_cz <- leontieff_matrix_create( input_coeff_matrix_cz  )
I_cz <- leontieff_inverse_create( input_coeff_matrix_cz )

L_sk <- leontieff_matrix_create( input_coeff_matrix_sk  )
I_sk <- leontieff_inverse_create( input_coeff_matrix_sk )

knitr::kable(head(I_cz[,1:8]))

## ----direct, results='asis'---------------------------------------------------
primary_inputs_cz <- coefficient_matrix_create(data_table = cz_io, 
                                              total = 'output', 
                                              return = 'primary_inputs') 

primary_inputs_sk <- coefficient_matrix_create(data_table = sk_io, 
                                              total = 'output', 
                                              return = 'primary_inputs')

direct_cz <- direct_effects_create( primary_inputs_cz, I_cz )  
direcz_sk <- direct_effects_create( primary_inputs_sk, I_sk )  

knitr::kable (head(direct_cz[,1:8]), digits = 4)

