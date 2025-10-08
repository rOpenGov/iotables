library(dplyr)

ind_ava_voc <- readxl::read_excel(
  here::here("data-raw", "eurostat_vocabularies_2025.xlsx"),
  sheet = "ind_ava"
)

ind_ava <- readxl::read_excel(here::here("data-raw", "ind_ava.xlsx")) %>%
  mutate(
    block = case_when(
      quadrant == 10 ~ "intermediate", # product technology block
      quadrant == 20 ~ "primary_inputs", # value-added
      quadrant == 30 ~ "control_total", # totals like "Total supply at basic prices"
      quadrant == 50 ~ "extension", # diagnostics, balancing items
      TRUE ~ "unspecified"
    )
  ) %>%
  mutate(uri = sprintf("https://dd.eionet.europa.eu/vocabularyconcept/eurostat/ind_ava/%s", notation)) %>%
  select(-nr, -account_group) %>%
  rename(numeric_order = numeric_label) %>%
  arrange(numeric_order)


eurostat_cpa_ind <- readxl::read_excel("eurostat_cpa.xlsx") %>%
  select ( id, new_number = numeric_order ) %>%
  filter ( grepl("^CPA", id)) %>%
  mutate ( id = gsub("CPA_", "", id ))

ind_ava_ext <- ind_ava %>%
  left_join ( eurostat_cpa_ind[eurostat_cpa_ind$id %in% ind_ava$id,] %>%
                select ( id, new_number)) %>%
  mutate ( new_number = ifelse(is.na(new_number), numeric_order*10, new_number)) %>%
  mutate ( new_number = ifelse(new_number<100000, new_number*10, new_number)) 

cpa2_1_mod <- cpa2_1  %>%
  filter ( grepl("^CPA", id)) %>%
  mutate ( id = gsub("CPA_", "", id )) %>%
  mutate ( notation = gsub("CPA_", "", notation ))

ind_ava_extended <- cpa2_1_mod[!cpa2_1_mod $id %in% ind_ava$id,] %>%
  mutate ( status  =  "Adopted from CPA2_1") %>%
  mutate ( new_number  = numeric_order ) %>%
  full_join(ind_ava_ext) %>%
  mutate ( numeric_order = new_number ) %>%
  select ( id, label, status, status_modified, 
           notation, quadrant, numeric_order, 
           iotables_label, block, uri) %>%
  filter (!is.na(id)) %>%
  filter ( numeric_order != 308910 ) %>%
  distinct(id, .keep_all = TRUE)  %>% # some character coding problem
  arrange(numeric_order) 

double <- ind_ava_extended %>% 
  select ( id, notation, iotables_label ) %>%
  group_by_all() %>% summarise( n = n()) %>%
  filter ( n > 1) %>%
  ungroup() %>% left_join (ind_ava_extended)

assertthat::assert_that(all(ind_ava_voc$Id %in% ind_ava$id))
ind_ava <- ind_ava_extended 
usethis::use_data(ind_ava, overwrite=F)
