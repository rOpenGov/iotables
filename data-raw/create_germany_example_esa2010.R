library (dplyr) ; library(tidyr) ; library (devtools)

germany <- read.csv ( file.path('data-raw', "Beutel_15_4_esa2010.csv"), 
                      stringsAsFactors = F)

germany_total <- germany %>% 
  mutate ( total = agriculture_group + industry_group + construction +
             trade_group + business_services_group + 
             other_services_group)


germany_long <- germany_total %>%
  gather ( iotables_col, values, agriculture_group:total) %>%
  mutate ( iotables_label_c  = plyr::mapvalues (iotables_col, 
                                           from = c('agriculture_group', 'industry_group', 'construction',
                                                    'trade_group', 'business_services_group', 'other_services_group', 
                                                    'total', 
                                                    'final_consumption_households', 
                                                    'final_consumption_government', 'gross_capital_formation',
                                                    'inventory_change', 'export_goods_services', 'total_final_use'), 
                                           to = c("Agriculture group", "Industry (except construction)", "Construction", "Trade group", 
                                                  "Business services group", "Other services group", "Total",
                                                  "Household consumption", 
                                                  "Government consumption", "Gross capital formation", "Inventory change", 
                                                  "Exports", "Total final use"))) %>%
  mutate ( induse           = plyr::mapvalues (iotables_label_c, 
                                                from = c("Agriculture group", "Industry (except construction)", "Construction", "Trade group", 
                                                       "Business services group", "Other services group", "Total", "Household consumption", 
                                                       "Government consumption", "Gross capital formation", "Inventory change", 
                                                       "Exports", "Total final use"), 
                                                to = c('CPA_A', 'CPA_B-E', 'CPA_F',
                                                       'CPA_G-I', 'CPA_J-N', 
                                                       'CPA_O-T', 'CPA_TOTAL',
                                                       'P3_S14', 'P3_S13', 'P5',
                                                       'P52', 'P6', 'TFU')) 
           ) %>%
  mutate ( ordering_r  = plyr::mapvalues (prod_na, 
                                          from = c('CPA_A', 'CPA_B-E', 'CPA_F',
                                                   'CPA_G-I', 'CPA_J-N', 
                                                   'CPA_O-T', 'CPA_TOTAL', 'P7',
                                                   'D21_M_D31', 'P2PP', 'D1',
                                                   'D29_M_D39', 'K1', 'B2N_B3N',
                                                   'B1G', 'P1', 
                                                   'EMP-WS', 'EMP-FTE', 'EMP'  
                                                   ), 
                                          to = seq(1:19))) %>%
  mutate ( ordering_c  = plyr::mapvalues (iotables_col, 
                                          from = c('agriculture_group', 'industry_group', 'construction',
                                                   'trade_group', 'business_services_group', 'other_services_group', 
                                                   'total',
                                                   'final_consumption_households', 
                                                   'final_consumption_government', 'gross_capital_formation',
                                                   'inventory_change', 'export_goods_services', 'total_final_use'), 
                                          to = seq(1:13))) %>%
  mutate ( ordering_c = as.numeric(ordering_c), 
           ordering_r = as.numeric(ordering_r)) %>%
  mutate ( r_quadrant = ifelse ( ordering_r > 7, yes = "2_4", no = "1_3")) %>%
  mutate ( c_quadrant = ifelse ( ordering_c > 6, yes = "3_4", no = "1_2")) %>%
  mutate ( geo  = "DE") %>%
  mutate ( geo_lab = "Germany") %>%
  mutate ( time = as.Date ('1990-01-01')) %>%
  mutate ( unit = 'MIO_EUR' ) %>%
  mutate ( unit_lab = "Million euro") %>%
  mutate ( prod_na = tolower(prod_na)) %>%
  mutate ( prod_na = forcats::fct_reorder(prod_na, 
                                          as.numeric(ordering_r))) %>%
  mutate ( iotables_col = forcats::fct_reorder(iotables_col, 
                                          as.numeric(ordering_c))) %>%
  mutate ( iotables_label_r = forcats::fct_reorder(prod_na, 
                                                   as.numeric(ordering_r))) %>%
  mutate ( iotables_label_c = forcats::fct_reorder(iotables_col, 
                                                   as.numeric(ordering_c))) %>%
  arrange ( prod_na, iotables_col ) 

names ( germany_long)

germany_1990 <- germany_long %>%
  dplyr::select ( -iotables_label_c, -ordering_c, -c_quadrant, 
           -iotables_label_r, -ordering_r, -r_quadrant, 
           -quadrant, -numeric_label ) %>%
  dplyr::mutate ( prod_na = toupper(prod_na)) 

usethis::use_data(germany_1990, overwrite = TRUE)


#usethis::use_data ( germany_metadata_rows, germany_metadata_cols, 
#                     internal = TRUE, overwrite = TRUE)
