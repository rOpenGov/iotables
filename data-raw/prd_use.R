library(dplyr)

eurostat_voc <- readxl::read_excel(here::here("data-raw", "eurostat_vocabularies_2025.xlsx"),
  sheet = "prd_use"
)

prd_use <- readxl::read_excel(
  here::here("data-raw", "prd_use.xlsx")
) %>%
  mutate(
    uri = sprintf("https://dd.eionet.europa.eu/vocabularyconcept/eurostat/prd_use/%s", notation)
  ) %>%
  arrange(numeric_order) %>%
  select(-ind_use_notation) %>%
  mutate(group = ifelse(grepl("^CPA_", notation), "Industry", group))

assertthat::assert_that(length(setdiff(eurostat_voc$Id, prd_use$id)) == 0)

usethis::use_data(prd_use)
