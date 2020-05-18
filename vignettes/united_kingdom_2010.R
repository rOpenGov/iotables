## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(iotables)
require(dplyr); require(tidyr); require(testthat)

## ----read_control_data, eval=FALSE--------------------------------------------
#  uk_2010_data <- iotables_download ( source = "uk_2010" )
#  save ( uk_2010_data, file = file.path('data-raw', 'uk_2010_data.rda'))
#  uk_test_results <- iotables:::uk_2010_results_get ()
#  #saved as package data

## ----downloading_excel_data---------------------------------------------------
uk_siot              <- iotable_get ( 
                          labelled_io_data = uk_2010_data, 
                          source = "uk_2010_siot" )
uk_published_coeff   <- iotable_get ( 
                          labelled_io_data = uk_2010_data, 
                          source = "uk_2010_siot" )
uk_published_inverse <- iotable_get ( 
                          labelled_io_data = uk_2010_data, 
                          source = "uk_2010_siot" )

## ----compare_coeff------------------------------------------------------------
uk_input_coeff <- input_coefficient_matrix_create( 
                      data_table = uk_siot)

uk_coeff_published <- select (uk_input_coeff, 1 ) %>%
  left_join ( ., 
              mutate_if( iotable_get ( labelled_io_data = uk_2010_data, 
                               source = "uk_2010_coeff" ), 
                            is.factor, as.character), 
              by = "prod_na")

test_that("correct data is returned", {
  expect_equal(round(uk_input_coeff[,2:8], 8),
               round(uk_coeff_published  [,2:8], 8)) })

## ----compare_inverse----------------------------------------------------------
uk_calculated_inverse <- leontieff_inverse_create(uk_input_coeff)
uk_published_inverse <- select (uk_calculated_inverse, 1 ) %>%
  left_join ( ., mutate_if( 
    iotable_get ( labelled_io_data = uk_2010_data, 
                               source = "uk_2010_inverse" ),
                  is.factor, as.character), 
              by = "prod_na")

test_that("correct data is returned", {
  expect_equal(round(uk_calculated_inverse[,2:8], 8),
               round(uk_published_inverse  [,2:8], 8)) })

## ----compare_effects----------------------------------------------------------
employment_effect_results <- uk_test_results %>%
  select ( uk_row_label, `Employment cost effects`)

primary_inputs_uk <- coefficient_matrix_create(
    data_table  = uk_siot, 
    total       = 'output', 
    return_part = 'primary_inputs')

employment_input <- filter (primary_inputs_uk , prod_na == "D1")

employment_effects <- direct_effects_create( employment_input, uk_calculated_inverse ) %>%
  gather ( prod, values, !!2:ncol(.)) %>%
  mutate ( prod_na = prod ) %>%
  select ( -prod ) %>%
  left_join ( select ( metadata_uk_2010, prod_na, uk_row_label  ), 
              by = 'prod_na') %>%
  left_join ( employment_effect_results, by = 'uk_row_label' ) %>%
  filter ( !is.na(uk_row_label )) %>%
  select ( prod_na, uk_row_label, values, `Employment cost effects`)


iotables:::create_knitr_table (
      data_table =  employment_effects[1:10,], 
      digits = 4,
      caption = "Comparison of Calculated And Published Employment Cost Effects", 
      col.names = c("industry code", "row label", "calculated", "published"), 
      col_width = c(2,11,3,3))

## ----gva_effects--------------------------------------------------------------
uk_siot2 <- uk_siot %>%
  filter (  prod_na %in% c("B2A3G", "D1", "D29X39") )  %>%
  summarize_if ( is.numeric, sum, na.rm = TRUE ) %>%
  cbind ( data.frame ( prod_na = "GVA"), . ) %>%
  rbind ( uk_siot, .)

gva_effect_results <- uk_test_results %>%
  select ( uk_row_label, `GVA effects`)

gva_input <- coefficient_matrix_create(
   data_table  = uk_siot2, 
   total       = 'output', 
   return_part = 'primary_inputs') %>%
  filter ( prod_na == "GVA" )

gva_effects <- direct_effects_create( gva_input,
                                      uk_calculated_inverse ) %>%
  gather ( prod, values, !!2:ncol(.)) %>%
  mutate ( prod_na = prod ) %>%
  select ( -prod ) %>%
  left_join ( select ( metadata_uk_2010, prod_na, uk_row_label  ), 
              by = 'prod_na') %>%
  left_join ( gva_effect_results, by = 'uk_row_label' ) %>%
  filter ( !is.na(uk_row_label )) %>%
  select ( prod_na, uk_row_label, values, `GVA effects`)

iotables:::create_knitr_table (
      data_table =  gva_effects[1:10,], 
      digits = 4,
      caption = "Comparison of Calculated And Published GVA Effects", 
      col.names = c("industry code", "row label", "calculated", "published"), 
      col_width = c(2,11,3,3))

