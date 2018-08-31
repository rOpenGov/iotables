library ( iotables) ; library (dplyr) ; library(tidyr) ; library (devtools)

germany <- read.csv ( "data-raw/Beutel_15_4.csv", stringsAsFactors = F)

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
  mutate ( iotables_col = t_cols2 ) %>%
  mutate ( col_order  = plyr::mapvalues (t_cols2, 
                                          from = c('agriculture_group', 'manufacturing_group', 'construction_group',
                                                   'trade_group', 'business_services_group', 'other_services_group', 
                                                   'consumption_expenditure_household', 
                                                   'consumption_expenditure_government', 'gross_capital_formation',
                                                   'inventory_change', 'export_goods_services', 'output_bp'), 
                                          to = seq(1:12))) %>%
  mutate ( col_order = as.numeric(col_order)) %>%
  mutate ( row_order = as.numeric(numeric_label)) %>%
  mutate ( r_quadrant = ifelse ( numeric_label > 6, yes = "2_4", no = "1_3")) %>%
  mutate ( c_quadrant = ifelse ( col_order > 6, yes = "3_4", no = "1_2")) %>%
  mutate ( geo  = "DE") %>%
  mutate ( geo_lab = "Germany") %>%
  mutate ( time = as.Date ('1990-01-01')) %>%
  mutate ( unit = 'MIO_EUR' ) %>%
  mutate ( unit_lab = "Million euro") %>%
  mutate ( t_rows2 = tolower(t_rows2)) %>%
  mutate ( t_cols2 = tolower(t_cols2))  %>%
  mutate ( t_rows2 = forcats::fct_reorder(t_rows2, 
                                          as.numeric(numeric_label))) %>%
  mutate ( t_cols2 = forcats::fct_reorder(t_cols2, 
                                          as.numeric(col_order))) %>%
  mutate ( iotables_label_r = forcats::fct_reorder(iotables_row , 
                                                   as.numeric(numeric_label))) %>%
  mutate ( iotables_col = forcats::fct_reorder(iotables_col, 
                                                   as.numeric(col_order))) %>%
  arrange ( t_rows2, t_cols2 )

check <- germany_long %>%
  select ( t_rows2, t_cols2, values ) %>%
  spread ( t_cols2, values )

check <- germany_long %>%
  select ( iotables_row, iotables_col, values ) %>%
  spread ( iotables_col, values )

germany_metadata_rows <- distinct ( germany_long, 
                                    t_rows2, t_rows2_lab, 
                                    iotables_row, numeric_label, r_quadrant) %>%
  mutate (  row_order = as.numeric(numeric_label))

germany_metadata_cols <- distinct ( germany_long, 
                                    t_cols2, t_cols2_lab, 
                                    iotables_col, col_order, c_quadrant) %>%
  mutate ( col_order =  as.numeric(col_order))

check <- germany_long %>%
  select ( -iotables_col, -col_order, -c_quadrant, 
           -iotables_row, -numeric_label, -r_quadrant )

check <- check %>%  left_join(., germany_metadata_rows) %>%
  left_join(., germany_metadata_cols)

t_rows_data <- read.csv ('data-raw/t_rows_data.csv', stringsAsFactors = F)
t_cols_data <- read.csv  ('data-raw/t_cols_data.csv', stringsAsFactors = F)
p_cols_data <- read.csv ( 'data-raw/p_cols_data.csv', stringsAsFactors = F)
p_rows_data <- read.csv ("data-raw/p_rows_data.csv", stringsAsFactors = F)

t_rows_vocabulary <- readxl::read_excel(path = "not_included/t_rows_vocabulary.xlsx")
names (t_rows_vocabulary  ) <- gsub( ":", "_", names (t_rows_vocabulary  ))

t_cols_vocabulary <- readxl::read_excel(path = "not_included/t_cols2.xlsx")
names (t_cols_vocabulary) <- gsub( ":", "_", names (t_cols_vocabulary  ))

induse_vocabulary <- readxl::read_excel(path = "not_included/induse.xlsx")
names (induse_vocabulary) <- gsub( ":", "_", names (induse_vocabulary  ))

prod_na_vocabulary <- readxl::read_excel(path = "not_included/prod_na.xlsx")
names (prod_na_vocabulary) <- gsub( ":", "_", names (prod_na_vocabulary  ))

employment_metadata <- readRDS("not_included/iotables_labor_aggregation_labels.rds")

devtools::use_data(t_rows_data, t_cols_data, 
                   p_cols_data, p_rows_data, 
                   germany_metadata_rows,
                   germany_metadata_cols,
                   t_rows_vocabulary, t_cols_vocabulary,
                   induse_vocabulary, prod_na_vocabulary,
                   employment_metadata,
                   internal = TRUE, overwrite = TRUE)
