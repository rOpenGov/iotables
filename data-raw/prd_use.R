library(dplyr)

eurostat_voc <- readxl::read_excel(
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



assertthat::assert_that(nrow(eurostat_voc %>% 
                               filter (! id %in% prd_use$id)) == 0)

usethis::use_data(prd_use)
