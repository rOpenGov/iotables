library(dplyr)
library(tidyr)
library(devtools)

read.csv(here::here("data-raw", "netherlands_2006.csv"))
netherlands_2006 <- read.csv2("data-raw/netherlands_2006.csv")

netherlands_2006 <- convert_to_ascii(netherlands_2006)

usethis::use_data(netherlands_2006,
  overwrite = TRUE
)


load(file.path("data", "netherlands_2006.rda"))
