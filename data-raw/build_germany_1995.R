data(germany_1995, package = "iotables")

data(germany_1995, package = "iotables")

germany_1995_metadata <- list(
  col_classes = sapply(germany_1995, class),
  prod_na_order = as.character(unique(germany_1995$prod_na)),
  iotables_col_levels = levels(germany_1995$iotables_col),
  prod_na_lab_lookup = setNames(
    germany_1995$prod_na_lab,
    paste(germany_1995$prod_na, germany_1995$iotables_col)
  ),
  iotables_row_lookup = setNames(
    germany_1995$iotables_row,
    paste(germany_1995$prod_na, germany_1995$iotables_col)
  )
)

saveRDS(
  germany_1995_metadata,
  file.path("data-raw", "germany_1995_metadata_for_rebuild.rds")
)

germany_1995_metadata <- readRDS(file.path("data-raw", "germany_1995_metadata_for_rebuild.rds"))

build_germany_1995 <- function(
    path = file.path("data-raw", "Beutel_15_4_esa2010.csv")
) {
  library(dplyr)
  library(tidyr)
  
  germany <- read.csv(path, stringsAsFactors = FALSE) %>%
    mutate(total = agriculture_group + industry_group + construction +
             trade_group + business_services_group + other_services_group)
  
  # pivot exactly 13 columns
  germany_long <- germany %>%
    pivot_longer(
      cols = c(
        agriculture_group, industry_group, construction,
        trade_group, business_services_group, other_services_group,
        total,
        final_consumption_households, final_consumption_government,
        gross_capital_formation, inventory_change, exports,
        total_final_use
      ),
      names_to = "iotables_col",
      values_to = "values"
    )
  
  # enforce factor levels and recode induse
  iotables_col_levels <- c(
    "agriculture_group", "industry_group", "construction",
    "trade_group", "business_services_group", "other_services_group",
    "total", "final_consumption_households", "final_consumption_government",
    "gross_capital_formation", "inventory_change", "exports",
    "total_final_use"
  )
  germany_long$iotables_col <- factor(germany_long$iotables_col,
                                      levels = iotables_col_levels)
  
  germany_long <- germany_long %>%
    mutate(
      induse = dplyr::recode(iotables_col,
                             "agriculture_group" = "CPA_A",
                             "industry_group" = "CPA_B-E",
                             "construction" = "CPA_F",
                             "trade_group" = "CPA_G-I",
                             "business_services_group" = "CPA_J-N",
                             "other_services_group" = "CPA_O-T",
                             "total" = "CPA_TOTAL",
                             "final_consumption_households" = "P3_S14",
                             "final_consumption_government" = "P3_S13",
                             "gross_capital_formation" = "P5",
                             "inventory_change" = "P52",
                             "exports" = "P6",
                             "total_final_use" = "TFU"
      ),
      geo = "DE",
      geo_lab = "Germany",
      time = as.Date("1995-01-01"),
      unit = "MIO_EUR",
      unit_lab = "Million euro",
      values = as.integer(round(values))  # match CRAN
    )
  
  # enforce final column order
  germany_1995 <- germany_long %>%
    select(
      prod_na, prod_na_lab, iotables_row, iotables_col,
      values, induse, geo, geo_lab, time, unit, unit_lab
    ) %>%
    mutate(induse = as.character(induse)) %>%  # force match with CRAN
    as.data.frame()
  
  
  germany_1995
}

new <- build_germany_1995()
dim(new)  # should be 247 x 11

subset(new, prod_na == "CPA_A" & iotables_col == "agriculture_group")$values
subset(new, prod_na == "CPA_A" & iotables_col == "final_consumption_households")$values
subset(new, prod_na == "B1G"   & iotables_col == "agriculture_group")$values


identical(dim(old), dim(new))
identical(names(old), names(new))
data.frame(
  column    = names(old),
  class_old = sapply(old, class),
  class_new = sapply(new, class),
  same      = mapply(identical, sapply(old, class), sapply(new, class))
)

if (is.factor(old$iotables_col) || is.factor(new$iotables_col)) {
  list(
    levels_old = levels(old$iotables_col),
    levels_new = levels(new$iotables_col),
    identical  = identical(levels(old$iotables_col), levels(new$iotables_col))
  )
}

list(
  prod_na_old = unique(old$prod_na),
  prod_na_new = unique(new$prod_na),
  identical   = identical(unique(old$prod_na), unique(new$prod_na))
)

library(testthat)
test_that("germany_1995 rebuild matches CRAN object", {
  new <- build_germany_1995()
  expect_identical(unclass(germany_1995), unclass(new))
})
