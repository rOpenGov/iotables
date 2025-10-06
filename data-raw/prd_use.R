prd_use <- readxl::read_excel(here::here("data-raw", "prd_use.xlsx")) %>%
  mutate(uri = sprintf("https://dd.eionet.europa.eu/vocabularyconcept/eurostat/prd_use/%s", notation)) %>%
  arrange(numeric_order) %>%
  select(-ind_use_notation) %>%
  mutate(group = ifelse(grepl("^CPA_", notation), "Industry", group))

usethis::use_data(prd_use)
names(prd_use)
