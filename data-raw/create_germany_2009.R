germany_2009 <- read.csv(file.path("data-raw", "UN_Manual", "germany_2009.csv"), stringsAsFactors = F)


names(germany_2009)

germany_2009 %>%
  set_names(c(
    "prod_na", "prod_na_lab", "numeric_label", "quadrant", "iotables_row",
    "agriculture_group", "industry_group", "construction", "trade_group", "business_services_group",
    "other_services_group", "total", "final_consumption_households"
  ))



germany_1995
