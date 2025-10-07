library(dplyr)

eurostat_voc <- readxl::read_excel(
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

assertthat::assert_that(length(setdiff(eurostat_voc$Id, ind_ava$id)) == 0)

usethis::use_data(ind_ava)
