library(dplyr)

eurostat_voc <- readxl::read_excel(
  here::here(
    "data-raw",
    "eurostat_vocabularies_2025.xlsx"
  ),
  sheet = "prd_ava"
)

prd_ava <- readxl::read_excel(here::here("data-raw", "prd_ava.xlsx")) %>%
  relocate(block, .before = uri) %>%
  arrange(numeric_order)

assertthat::assert_that(length(setdiff(eurostat_voc$Id, prd_ava$id)) == 0)


usethis::use_data(prd_ava)

names(prd_ava)
