library ( iotables) ; library (dplyr)

labels <- readRDS("not_included/eurostat_io_current_prices.rds") %>%
  select (t_rows2, t_rows2_lab) %>%
  distinct ( t_rows2, t_rows2_lab)

primary_inputs <- read.csv("not_included/primary_inputs.csv") %>%
  left_join(., labels, by = "t_rows2") %>%
  filter ( complete.cases(.))

devtools::use_data(primary_inputs, overwrite = TRUE)
