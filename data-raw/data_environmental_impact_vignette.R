# data-raw/data_environmental_impact_vignette.R
# -------------------------------------------------------------------
# Build dataset for the environmental_impact vignette

library(iotables)
library(dplyr)
library(here)

data_dir <- here("data-raw")

# --- 1.  Input–output tables ---------------------------------------

# Bulk download: slow, but works:
io_tables <- iotables_download(
  source = "naio_10_cp1700",
  data_directory = data_dir,
  force_download = TRUE
)

str(io_tables)

# Use the bulk downloaded big tibble in memory to start the
# iotable_get() formatting.

be_io_2020 <- iotable_get(
  labelled_io_data = io_tables,
  source = "naio_10_cp1700",
  geo = "BE",
  year = 2020,
  data_directory = data_dir
)

# --- 2.  Air-pollutant tables -------------------------------------
be_airpol_2020 <- airpol_get(
  airpol = "GHG", # or "CO2" for CO₂ only
  geo = "BE",
  year = 2020,
  unit = "THS_T",
  data_directory = data_dir,
  force_download = TRUE
)

# --- 3.  Save for the vignette ------------------------------------
vignette_data <- list(
  io_be_2020 = be_io_2020,
  airpol_be_2020 = be_airpol_2020
)

usethis::use_data(vignette_data,
  name = "environmental_impact_vignette",
  path = "inst/extdata",
  compress = "xz",
  overwrite = TRUE
)

message("✓  Saved inst/extdata/environmental_impact_vignette.rda")
