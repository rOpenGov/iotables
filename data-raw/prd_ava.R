library(dplyr)

eurostat_voc <- readxl::read_excel(
  here::here(
    "data-raw",
    "eurostat_vocabularies_2025.xlsx"
  ),
  sheet = "prd_ava"
) %>%
  janitor::clean_names()

ind_ava_raw <- read.csv(here::here("data-raw", "ind_ava.csv")) %>%
  janitor::clean_names() %>%
  mutate ( vocabulary  = "ind_ava")
prd_ava_raw <- read.csv(here::here("data-raw", "prd_ava.csv")) %>%
  janitor::clean_names() %>% mutate (vocabulary = "prd_ava")
prd_use_raw <- read.csv(here::here("data-raw", "prd_use.csv")) %>%
  janitor::clean_names() %>% mutate (vocabulary = "prd_use")
ind_use_raw <- read.csv(here::here("data-raw", "ind_use.csv")) %>%
  janitor::clean_names() %>% mutate (vocabulary = "ind_use")

eurostat_raw <- bind_rows(prd_ava_raw, ind_ava_raw) %>%
  bind_rows(prd_use_raw ) %>%
  bind_rows(ind_use_raw )

io_prd_ava <- select(iotable, prd_ava, prd_ava_lab, values) %>%
  mutate ( notation = prd_ava  ) %>% 
  left_join( eurostat_raw )

prd_ava <- readxl::read_excel(
  here::here("data-raw", "prd_ava.xlsx")) %>%
  relocate(block, .before = uri) %>%
  bind_rows(prd_ava %>%
              filter ( id  == "TOTAL") %>%
              mutate ( id = "CPA_TOTAL", 
                       notation = "CPA_TOTAL", 
                       status_modified = NA_character_, 
                       status = "Observed")) %>%
  arrange(numeric_order)

assertthat::assert_that(length(setdiff(eurostat_voc$Id, prd_ava$id)) == 0)

usethis::use_data(prd_ava)

