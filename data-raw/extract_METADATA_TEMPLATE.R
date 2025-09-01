# data-raw/extract_METADATA_TEMPLATE.R
# Extract reference metadata from a legacy dataset

extract_METADATA_TEMPLATE <- function(dataset) {
  stopifnot(is.data.frame(dataset))

  list(
    col_classes = sapply(dataset, class),
    prod_na_order = as.character(unique(dataset$prod_na)),
    iotables_col_levels = if ("iotables_col" %in% names(dataset) &&
      is.factor(dataset$iotables_col)) {
      levels(dataset$iotables_col)
    } else {
      NULL
    },
    prod_na_lab_lookup = setNames(
      dataset$prod_na_lab,
      paste(dataset$prod_na, dataset$iotables_col)
    ),
    iotables_row_lookup = setNames(
      dataset$iotables_row,
      paste(dataset$prod_na, dataset$iotables_col)
    )
  )
}

# Example: extract metadata for germany_1995 and save
# data(germany_1995, package = "iotables")
# metadata <- extract_METADATA_TEMPLATE(germany_1995)
# saveRDS(metadata, "data-raw/germany_1995_metadata.rds")
