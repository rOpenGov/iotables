library(iotables)
BE <- iotable_get(
  source = "naio_10_cp1700",
  geo = "BE",
  year = 2020,
  labelling = "short",
  unit = "MIO_EUR",
  stk_flow = "TOTAL"
)

ghg <- airpol_get(
  airpol = "GHG",
  geo = "BE",
  year = 2020,
  unit = "THS_T"
)

co2 <- airpol_get(
  airpol = "CO2",
  geo = "BE",
  year = 2020,
  unit = "THS_T"
)

methane <- airpol_get(
  airpol = "CH4",
  geo = "BE",
  year = 2020,
  unit = "THS_T"
)

save(methane, co2, ghg, BE, file = here::here("inst", "extdata", "environmental_impact_vignette.rda"))

ghg_indicator <- input_indicator_create(
  data_table = be_io,
  input_row  = "GHG_emission"
)
be_io <- BE %>%
  supplementary_add(ghg %>% convert_industry_to_product())

I_be <- input_coefficient_matrix_create(
  data_table = BE,
  digits = 4
) %>%
  leontief_inverse_create()


ghg_multipliers <- multiplier_create(
  input_vector = ghg_indicator,
  Im = I_be,
  multiplier_name = "GHG_multiplier",
  digits = 4
)
