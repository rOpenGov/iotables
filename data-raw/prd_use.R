library(dplyr)

prd_use_voc <- readxl::read_excel(
  here::here("data-raw", "eurostat_vocabularies_2025.xlsx"),
  sheet = "prod_use"
) %>%
  janitor::clean_names()

prd_use <- readxl::read_excel(
  here::here("data-raw", "prd_use.xlsx")
) %>%
  mutate(
    uri = sprintf("https://dd.eionet.europa.eu/vocabularyconcept/eurostat/prd_use/%s", notation)
  ) %>%
  arrange(numeric_order) %>%
  select(-ind_use_notation) %>%
  mutate(group = ifelse(grepl("^CPA_", notation), "Industry", group))

eurostat_cpa <- readxl::read_excel("eurostat_cpa.xlsx") %>%
  mutate ( id = gsub("CPA_", "", notation)) %>%
  select ( notation, id, numeric_order )

prd_use_ext <- prd_use %>%
  left_join ( eurostat_cpa[eurostat_cpa$notation %in% prd_use$id,] %>%
                select ( id = notation, new_number = numeric_order)) %>%
  mutate ( new_number = ifelse(is.na(new_number), numeric_order*10, new_number)) %>%
  mutate ( new_number = ifelse(new_number<100000, new_number*10, new_number))

prd_use_extended <- eurostat_cpa[!eurostat_cpa$notation %in% prd_use$id,] %>%
  mutate( id= notation) %>%
  select( id ) %>% left_join(cpa2_1) %>%
  mutate ( status  = case_when(
    id %in% cpa2_1$id ~ "Valid in CPA2_1", 
    TRUE ~ "Valid in ind_use"
  )) %>%
  mutate ( new_number  = numeric_order ) %>%
  full_join( prd_use_ext ) %>%
  mutate ( numeric_order = new_number ) %>%
  select ( id, label, status, status_modified, 
           notation, quadrant, numeric_order, 
           iotables_label, block, uri) %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate ( numeric_order = ifelse(id == "CPA_TOTAL", 
                                   190000, numeric_order)) %>%
  arrange(numeric_order) 

double <- prd_use_extended %>% 
  select ( id, notation, iotables_label ) %>%
  group_by_all() %>% summarise( n = n()) %>%
  filter ( n > 1) %>%
  ungroup() %>% left_join ( prd_use_extended)

assertthat::assert_that(all(prd_use_voc$id %in% prd_use_extended$id ))
prd_use <- prd_use_extended
usethis::use_data(prd_use, overwrite = F)
