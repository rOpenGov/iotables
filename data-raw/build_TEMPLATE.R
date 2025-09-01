# data-raw/build_TEMPLATE.R
# Template builder for reproducible legacy datasets in iotables

build_TEMPLATE <- function(
    path = file.path("data-raw", "TEMPLATE.csv"),
    metadata = readRDS(file.path("data-raw", "TEMPLATE_metadata.rds"))
) {
  library(dplyr)
  library(tidyr)
  
  # TODO: load the correct raw CSV
  raw <- read.csv(path, stringsAsFactors = FALSE)
  
  # TODO: add totals if necessary (example from Germany)
  raw <- raw %>%
    mutate(total = agriculture_group + industry_group + construction +
             trade_group + business_services_group + other_services_group)
  
  # TODO: pivot exactly the right set of columns (list them explicitly!)
  long <- raw %>%
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
    ) %>%
    mutate(
      # TODO: adapt induse recode to dataset
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
      # TODO: adjust geo, time, units
      geo = "DE",
      geo_lab = "Germany",
      time = as.Date("1995-01-01"),
      unit = "MIO_EUR",
      unit_lab = "Million euro",
      values = as.integer(round(values))
    )
  
  # Apply metadata
  long$prod_na <- factor(long$prod_na, levels = metadata$prod_na_order)
  long$iotables_col <- factor(long$iotables_col,
                              levels = metadata$iotables_col_levels)
  long <- dplyr::arrange(long, prod_na, iotables_col)
  
  # Apply label lookups
  key <- paste(long$prod_na, long$iotables_col)
  long$prod_na_lab <- metadata$prod_na_lab_lookup[key]
  long$i otables_row <- metadata$i otables_row_lookup[key]
  
  # Coerce to stored classes
  for (col in names(metadata$col_classes)) {
    cls <- metadata$col_classes[[col]]
    if (cls == "integer") {
      long[[col]] <- as.integer(round(long[[col]]))
    } else if (cls == "Date") {
      long[[col]] <- as.Date(long[[col]])
    } else if (cls == "factor") {
      # already handled
    } else {
      long[[col]] <- as.character(long[[col]])
    }
  }
  
  # Final selection in CRAN order
  out <- long %>%
    select(
      prod_na, prod_na_lab, iotables_row, iotables_col,
      values, induse, geo, geo_lab, time, unit, unit_lab
    ) %>%
    as.data.frame()
  
  out
}

# Example usage:
# new <- build_TEMPLATE()
# stopifnot(identical(unclass(TEMPLATE), unclass(new)))
# usethis::use_data(new, overwrite = TRUE)
