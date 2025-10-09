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

eurostat_cpa <- readxl::read_excel(
  here::here("data-raw", "eurostat_cpa.xlsx")) %>%
  select(id, new_number = numeric_order) %>%
  filter(grepl("^CPA", id)) %>%
  mutate(id = gsub("CPA_", "", id))

# already in ind_ava but renumbered
ind_ava_ext <- ind_ava %>%
  left_join(eurostat_cpa_ind[eurostat_cpa_ind$id %in% ind_ava$id, ] %>%
    select(id, new_number)) %>%
  mutate(new_number = ifelse(is.na(new_number), numeric_order * 10, new_number)) %>%
  mutate(new_number = ifelse(new_number < 100000, new_number * 10, new_number))

"C16" %in% ind_ava_ext$id
"C15" %in% ind_ava_ext$id

# CPA codes with 0, 1, 2 digits
cpa2_1_mod <- cpa2_1 %>%
  filter(grepl("^CPA", id)) %>%
  mutate(id = gsub("CPA_", "", id)) %>%
  mutate(notation = gsub("CPA_", "", notation))

"C16" %in% cpa2_1_mod$id
"C15" %in% cpa2_1_mod$id

ind_ava_extended_1 <- cpa2_1_mod[!cpa2_1_mod$id %in% ind_ava$id, ] %>%
  mutate(
    # assemble first what is not in in ind_ava
    status = "Adopted from CPA2_1"
  ) %>%
  mutate(new_number = numeric_order)

"C16" %in% ind_ava_extended_1$id
"C15" %in% ind_ava_extended_1$id

ind_ava_extended_2 <- ind_ava_extended_1 %>%
  full_join(ind_ava_ext) %>%
  mutate(numeric_order = new_number) %>%
  select(
    id, label, status, status_modified,
    notation, quadrant, numeric_order,
    iotables_label, block, uri
  ) %>%
  filter(!is.na(id)) %>%
  filter(numeric_order != 308910) %>%
  distinct(id, .keep_all = TRUE) %>% # some character coding problem
  arrange(numeric_order)

all(c(
  "C16" %in% ind_ava_extended_2$id,
  "C15" %in% ind_ava_extended_2$id
))

double <- ind_ava_extended %>%
  select(id, notation, iotables_label) %>%
  group_by_all() %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  ungroup() %>%
  left_join(ind_ava_extended_2)

assertthat::assert_that(nrow(double) == 0)

nrow(ind_ava_extended_2)

assertthat::assert_that(all(ind_ava_voc$Id %in% ind_ava$id))
ind_ava <- ind_ava_extended_2
usethis::use_data(ind_ava, overwrite = F)
