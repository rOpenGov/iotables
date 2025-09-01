co2 <-
  dir(tempdir())

be_io <- iotable_get(source = "naio_10_cp1700", geo = "NL", year = 2020, labelling = "short")

cp1700 <- iotables_read_tempdir(source = "naio_10_cp1700")
library(dplyr)

be_io <- cp1700 %>% filter(
  geo == "BE",
  time == as.Date("2015-01-01"),
  stk_flow == "TOTAL",
  unit == "MIO_EUR"
)
BE <- iotable_get(
  source = "naio_10_cp1700", geo = "BE",
  year = 2015,
  labelling = "short", unit = "MIO_EUR",
  stk_flow = "TOTAL"
)



names(BE)
cv <- conforming_vector_create(BE) %>%
  pivot_longer(
    cols = everything(),
    names_to = "nace_r2",
    values_to = "co2"
  )


co2 <- eurostat::get_eurostat("env_ac_ainah_r2")
names(co2)

?eurostat::get_eurostat_json()

id <- "env_ac_ainah_r2"

ghg <- airpol_get(airpol = "GHG", geo = "BE", year = 2020, unit = "THS_T")

be_io <- BE %>% bind_rows(ghg %>% rename(prod_na = .data$indicator))

ghg_indicator <- input_indicator_create(
  data_table = be_io,
  input_vector = "GHG_output"
)

View(ghg_indicator)
