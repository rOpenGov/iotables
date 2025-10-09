library(dplyr)

prd_ava_voc <- readxl::read_excel(
  here::here(
    "data-raw",
    "eurostat_vocabularies_2025.xlsx"
  ),
  sheet = "prd_ava"
) %>%
  janitor::clean_names()

prd_ava <- readxl::read_excel(here::here("data-raw", "prd_ava.xlsx"))

eurostat_cpa <- readxl::read_excel(
  here::here("data-raw", "eurostat_cpa.xlsx")) %>%
  mutate(id = gsub("CPA_", "", notation)) %>%
  select(notation, id, numeric_order)

prd_ava_ext <- readxl::read_excel(here::here("data-raw", "prd_ava.xlsx")) %>%
  left_join(eurostat_cpa[eurostat_cpa$notation %in% prd_ava$id, ] %>%
    select(id = notation, new_number = numeric_order)) %>%
  mutate(new_number = ifelse(is.na(new_number), numeric_order * 10, new_number)) %>%
  mutate(new_number = ifelse(new_number < 100000, new_number * 10, new_number))

prd_ava_extended <- eurostat_cpa[!eurostat_cpa$notation %in% prd_ava$id, ] %>%
  mutate(id = notation) %>%
  select(id) %>%
  left_join(cpa2_1) %>%
  mutate(status = case_when(
    id %in% cpa2_1$id ~ "valid in CPA2_1",
    TRUE ~ "valid in ind_ava"
  )) %>%
  mutate(new_number = numeric_order) %>%
  full_join(prd_ava_ext) %>%
  mutate(numeric_order = new_number) %>%
  select(
    id, label, status, status_modified,
    notation, quadrant, numeric_order,
    iotables_label, block, uri
  ) %>%
  arrange(numeric_order) %>%
  distinct(id, .keep_all = TRUE) %>% # some character coding problem
  mutate(numeric_order = ifelse(id == "CPA_TOTAL",
    190000, numeric_order
  )) %>%
  arrange(numeric_order)

assertthat::assert_that(all(prd_ava_voc$id %in% prd_ava_extended$id))

double <- prd_ava_extended %>%
  select(id, notation, iotables_label) %>%
  group_by_all() %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup() %>%
  left_join(prd_ava_extended)

prd_ava <- prd_ava_extended
usethis::use_data(prd_ava, overwrite = F)
