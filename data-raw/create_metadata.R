
library (dplyr)
metadata <- readxl::read_excel("data-raw/metadata.xlsx", 
                               sheet = "all") %>%
  dplyr::arrange( numeric_label ) %>%
  dplyr::select ( -digit_1, -digit_2 )

metadata_uk_2010 <- readxl::read_excel(path = file.path('data-raw', 'metadata_uk_2010.xlsx')) %>%
  dplyr::mutate ( uk_col = gsub("\\.", "-", as.character(uk_col))) %>%
  dplyr::mutate ( uk_col = gsub(" & ", "-", as.character(uk_col))) %>%
  dplyr::mutate ( uk_row = gsub("\\.", "-", as.character(uk_row))) %>%
  dplyr::mutate ( uk_row = gsub(" & ", "-", as.character(uk_row))) %>%
  dplyr::mutate ( uk_col = trimws(uk_col, 'both')) %>%
  dplyr::select ( -digit_1, -digit_2, -digit_3_5 )

load ( file.path('data-raw', 'uk_2010_data.rda'))

uk_test_results <- iotables:::uk_2010_results_get ()

usethis::use_data(metadata, metadata_uk_2010, uk_test_results, 
                  uk_2010_data,
                   internal = FALSE, overwrite = TRUE)
