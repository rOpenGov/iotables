ind_use <- readxl::read_excel(here::here("data-raw", "ind_use.xlsx")) %>%
  mutate(
    block = case_when(
      quadrant == 10 ~ "intermediate", # product technology block
      quadrant == 20 ~ "primary_inputs", # value-added
      quadrant == 30 ~ "final_use", # totals like "Total supply at basic prices"
      quadrant == 50 ~ "extension", # diagnostics, balancing items
      TRUE ~ "unspecified"
    )
  ) %>%
  mutate(uri = sprintf("https://dd.eionet.europa.eu/vocabularyconcept/eurostat/ind_use/%s", notation)) %>%
  arrange(numeric_order) %>%
  select(-ind_use_notation)

usethis::use_data(ind_use)
names(ind_use)
