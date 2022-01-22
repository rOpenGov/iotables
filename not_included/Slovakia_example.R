##Slovakia_example.R
library(iotables) ; library(dplyr) ; library (devtools)
load_all()
geo <- geo_input <- "SK"
unit_input <- unit <- "MIO_EUR"
year = 2010; labelling = "iotables"
stk_flow <- stk_flow_input <- "DOM"
keep_total <- FALSE
households <- TRUE

#iocp <- get_io_current_prices( ) 
#saveRDS(iocp, "not_included/eurostat_io_current_prices.rds")
labelled_io_table <- iotable_get ( source = "naio_10_cp1750", 
                                   geo = "CZ")

labelled_io_table <- readRDS("C:/Users/Daniel Antal/OneDrive - Visegrad Investments/2017 Projektek/SK_2017/data-raw/naio_cp17_r2.rds")

sk_use_1700_2 <- use_table_get ( source = "naio_10_cp1750", geo = "SK",
                                  year = 2010, unit = "MIO_EUR",
                                  stk_flow = "DOM", households = TRUE,
                                  keep_total = FALSE)

output_vector_sk <- output_get(source = "naio_10_cp1700", geo = "SK",
                               year = 2010, unit = "MIO_EUR", 
                               labelling = "iotables",  households = TRUE,
                               keep_total = FALSE)
sk_coeff <- input_coefficient_matrix_create( 
  input_flow = sk_use_1700_2,
  output = output_vector_sk, 
  digits = 4)
L <- leontief_matrix_create( technology_coefficients_matrix =
                                sk_coeff )
I <- leontief_inverse_create(L)

sk_emp <- employment_get ()

input_flow = sk_use_1700_2
output = output_vector_sk
sk_io2 <- iotable_get2 ( source = "naio_cp17_r2", geo = "SK",
                       year = 2010, unit = "MIO_EUR")

source= "naio_cp17_r2" ; geo= "SK" ; year = 2010 ; unit = "MIO_EUR"
sk_io_1700 <- iotable_get ( source = "naio_10_cp1700", geo = "SK",
                       year = 2010, unit = "MIO_EUR")
sk_io_1700_2 <- iotable_get2 ( source = "naio_10_cp1700", geo = "SK",
                            year = 2010, stk_flow = "IMP", 
                            unit = "MIO_EUR")
sk_use_1700_2 <- use_table_get2 ( source = "naio_10_cp1700", geo = "SK",
                               year = 2010, unit = "MIO_EUR",
                               stk_flow = "DOM", households = TRUE,
                               keep_total = TRUE)

DE_use_1700_2 <- use_table_get2 ( source = "germany_1990", geo = "DE",
                                  year = 1990, unit = "MIO_EUR",
                                  stk_flow = "DOM", households = TRUE,
                                  keep_total = TRUE)

labelled_io_data <- sk_io_1700

retrieve_from_temp_bulk <- readRDS(paste0(tempdir(), "\\naio_cp17_r2.rds" ))

sk_use <- use_table_get2 ( sk_io_1700_2 )


tech_sk <- get_technology_data(sk_io)
tech_sk_2 <- get_technology_data(sk_io, households = TRUE)

input_flow_sk <- input_flow_get ( labelled_io_data = sk_io, 
                                         technology = tech_sk_2, 
                                         year = 2010, geo = "SK", 
                                         unit = "MIO_EUR",
                                         named = TRUE ) 

output_vector_sk <- output_get(labelled_io_data = iocp, technology = tech_sk, 
                            year = 2010, geo = 'SK', unit = "MIO_EUR", 
                            named = TRUE, households = TRUE)

#sk_output_vector <- outlay_total (input_flow_sk )

input_coefficients <- input_coefficient_matrix_create(
  input_flow_sk, output_vector_sk, digits = 4)

L <- iotables::leontief_matrix_create( technology_coefficients_matrix =
                                          input_coefficients)

I <- iotables::leontief_inverse_create(L)

value_added_sk <- input_value_added_get( labelled_io_data = iocp, 
                                         technology = tech_sk, geo = "SK", 
                                         year = 2010, unit = "MIO_EUR", 
                                         named = TRUE) 

va_indicator_sk <- input_indicator_create (
  value_added_sk, output_vector_sk, digits = 4) 

va_multiplier_sk <- multiplier_create ( 
  input_vector  =    va_indicator_sk, Im = I, digits = 4 ) %>%
  tidyr::gather ( t_cols2, values, !! 2:ncol(.))  %>%
  mutate ( group = 'services') %>%
  mutate ( group = ifelse(grepl("CPA_A", t_cols2), "agriculture", group )) %>%
  mutate ( group = ifelse(grepl("CPA_B", t_cols2), "mining", group )) %>%
  mutate ( group = ifelse(grepl("CPA_C", t_cols2), "manufacturing", group )) %>%
  mutate ( group = ifelse(grepl("CPA_F", t_cols2), "construction", group )) %>%
  mutate ( group = ifelse(grepl("CPA_R90-R92", t_cols2), "live music & arts", group )) %>%
  mutate ( group = ifelse(grepl("CPA_J59_J60", t_cols2), "music & audiovisual", group )) %>%
  filter ( t_cols2 != "CPA_L68A")
  
slovakia_employment_aggregation <- read.csv("not_included/slovakia_employment_aggregation.csv", 
                                            stringsAsFactors = FALSE)
names ( slovakia_employment_aggregation )[1] <- 'employment_label_sk'


employment_sk <- readxl::read_xls(
  path = "C:/Users/Daniel Antal/OneDrive - Visegrad Investments/2017 Projektek/CEEMID_Import/_data/Slovakia/Employment by industry A88 - domestic concept [nu1057rs].xls", 
  sheet = "FTE"
) %>%
  filter (! grepl ( "By industry", Industry)) %>%
  dplyr::rename ( employment_label_sk = Industry) %>%
  tidyr::gather ( year, employment, !! 2:ncol(.)) %>%
  dplyr::mutate ( year = as.numeric(year)) 

employment_2015 <- employment_sk %>%
  filter ( year == 2015) %>%
  select ( -year )

fte_sk <- employment_aggregate(employment_2015, slovakia_employment_aggregation, 
                               label = "employment_label_sk") %>%
  mutate (  CPA_L68A = 0 )

employment_indicator <- iotables::input_indicator_create (
  fte_sk, output_vector_sk, digits = 4)

employment_multipliers_sk <- multiplier_create ( 
  input_vector  =   employment_indicator, Im = I, digits = 4 ) %>%
  tidyr::gather ( t_cols2, values, !! 2:ncol(.))  %>%
  mutate ( group = 'services') %>%
  mutate ( group = ifelse(grepl("CPA_A", t_cols2), "agriculture", group )) %>%
  mutate ( group = ifelse(grepl("CPA_B", t_cols2), "mining", group )) %>%
  mutate ( group = ifelse(grepl("CPA_C", t_cols2), "manufacturing", group )) %>%
  mutate ( group = ifelse(grepl("CPA_F", t_cols2), "construction", group )) %>%
  mutate ( group = ifelse(grepl("CPA_R90-R92", t_cols2), "live music & arts", group )) %>%
  mutate ( group = ifelse(grepl("CPA_J59_J60", t_cols2), "music & audiovisual", group )) %>%
  filter ( t_cols2 != "CPA_L68A")

