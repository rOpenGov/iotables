library(dplyr)
prd_ava <- readxl::read_excel(here::here("data-raw", "prd_ava.xlsx")) %>%
  relocate(block, .before = uri) %>%
  arrange(numeric_order)

usethis::use_data(prd_ava)

names(prd_ava)
