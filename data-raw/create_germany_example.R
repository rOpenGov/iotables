library (dplyr) ; library(tidyr) ; library (devtools)

germany <- read.csv ( "data-raw/Beutel_15_4.csv", stringsAsFactors = F)

#write.csv (iotables:::germany_metadata_rows, "germany_metadata_rows.csv")
#write.csv (iotables:::germany_metadata_cols, "germany_metadata_cols.csv")


#germany_metadata_cols <- readxl::read_excel("data-raw/Germany_metadata.xlsx", 
#                                            sheet = "germany_metadata_cols") %>%
 # mutate_if ( is.factor, as.character )

#germany_metadata_rows <- readxl::read_excel("data-raw/Germany_metadata.xlsx", 
  #                                           sheet = "germany_metadata_rows") %>%
 # mutate_if ( is.factor, as.character )

germany_long <- germany %>%
  gather ( t_cols2, values, agriculture_group:output_bp) %>%
  mutate ( t_cols2_lab  = plyr::mapvalues (t_cols2, 
                                           from = c('agriculture_group', 'manufacturing_group', 'construction_group',
                                                    'trade_group', 'business_services_group', 'other_services_group', 
                                                    'consumption_expenditure_household', 
                                                    'consumption_expenditure_government', 'gross_capital_formation',
                                                    'inventory_change', 'export_goods_services', 'output_bp'), 
                                           to = c("Agriculture group", "Manufacturing group", "Construction group", "Trade group", 
                                                  "Business services group", "Other services group", "Household consumption", 
                                                  "Government consumption", "Gross capital formation", "Inventory change", 
                                                  "Exports", "Output at basic prices"))) %>%
  mutate ( iotables_label_c = t_cols2 ) %>%
  mutate ( ordering_r  = plyr::mapvalues (t_rows2, 
                                          from = c('cpa_a', 'cpa_c', 'cpa_f',
                                                   'cpa_g_i', 'cpa_business', 
                                                   'cpa_other', 'cpa_total', 'P7',
                                                   'D21_M_D31', 'P2PP', 'D1',
                                                   'D29_M_D39', 'K1', 'B2N_B3N',
                                                   'B1G', 'P1', 
                                                   'EMP-WS', 'EMP-FTE', 'EMP'  
                                                   ), 
                                          to = seq(1:19))) %>%
  mutate ( ordering_c  = plyr::mapvalues (t_cols2, 
                                          from = c('agriculture_group', 'manufacturing_group', 'construction_group',
                                                   'trade_group', 'business_services_group', 'other_services_group', 
                                                   'consumption_expenditure_household', 
                                                   'consumption_expenditure_government', 'gross_capital_formation',
                                                   'inventory_change', 'export_goods_services', 'output_bp'), 
                                          to = seq(1:12))) %>%
  mutate ( ordering_c = as.numeric(ordering_c), 
           ordering_r = as.numeric(ordering_r)) %>%
  mutate ( r_quadrant = ifelse ( ordering_r > 6, yes = "2_4", no = "1_3")) %>%
  mutate ( c_quadrant = ifelse ( ordering_c > 6, yes = "3_4", no = "1_2")) %>%
  mutate ( geo  = "DE") %>%
  mutate ( geo_lab = "Germany") %>%
  mutate ( time = as.Date ('1990-01-01')) %>%
  mutate ( unit = 'MIO_EUR' ) %>%
  mutate ( unit_lab = "Million euro") %>%
  mutate ( t_rows2 = tolower(t_rows2)) %>%
  mutate ( t_cols2 = tolower(t_cols2))  %>%
  mutate ( t_rows2 = forcats::fct_reorder(t_rows2, 
                                          as.numeric(ordering_r))) %>%
  mutate ( t_cols2 = forcats::fct_reorder(t_cols2, 
                                          as.numeric(ordering_c))) %>%
  mutate ( iotables_label_r = forcats::fct_reorder(t_rows2  , 
                                                   as.numeric(ordering_r))) %>%
  mutate ( iotables_label_c = forcats::fct_reorder(t_cols2, 
                                                   as.numeric(ordering_c))) %>%
  arrange ( t_rows2, t_cols2 )


germany_1990 <- germany_long %>%
  dplyr::select ( -iotables_label_c, -ordering_c, -c_quadrant, 
           -iotables_label_r, -ordering_r, -r_quadrant, 
           -quadrant, -iotables_row, -numeric_label )

devtools::use_data(germany_1990, overwrite = TRUE)

employment_metadata <- 

devtools::use_data ( germany_metadata_rows, germany_metadata_cols, 
                     internal = TRUE, overwrite = TRUE)
