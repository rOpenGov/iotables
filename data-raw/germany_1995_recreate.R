# data-raw/germany_1995_recreate.R

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
# assumes your stacked iotable_create() already exists & is exported

# --- choose which source to use --------------------------------------------
# You uploaded both. Pick one; the code adapts to either naming.
src_file <- "data-raw/Beutel_15_4_esa2010.csv"
if (!file.exists(src_file)) src_file <- "data-raw/Beutel_15_4.csv"

de <- readr::read_csv(src_file, show_col_types = FALSE)

# --- normalise column names we expect --------------------------------------
# Some files say "industry_group" vs "manufacturing_group"
de <- de %>%
  rename(
    industry_group = dplyr::coalesce(
      .data$industry_group, .data$manufacturing_group
    )
  )

# Beutel/Eurostat block ordering (6 industry cols first)
q1_id <- c("agriculture_group",
           "industry_group",       # formerly "manufacturing_group"
           "construction",
           "trade_group",
           "business_services_group",
           "other_services_group")

# Standard final demand columns present in your CSVs
final_use_id <- c("final_consumption_households",
                  "final_consumption_government",
                  "gross_capital_formation",
                  "inventory_change",
                  "exports")

# Core primary inputs for Quadrant 3 (value added rows)
# (D.1, D.29–D.39 (net), K.1, B.2n+B.3n net)
primary_id <- c("compensation_employees",
                "net_tax_production",
                "consumption_fixed_capital",
                "os_mixed_income_net")

# --- safety checks ----------------------------------------------------------
stopifnot(all(q1_id %in% names(de)))
stopifnot(all(final_use_id %in% names(de)))

# Row id column in your Beutel CSV is typically 'iotables_row'
row_id_col <- "iotables_row"
if (!row_id_col %in% names(de)) {
  # fall back to a plausible name from your earlier scripts
  row_id_col <- "t_rows2"
}
stopifnot(row_id_col %in% names(de))

# --- slice Quadrant blocks --------------------------------------------------
# Q1 & Q2 are the TOP industry rows (same 6 names as q1_id) in Beutel
# Your pasted example shows those exact row labels:
industry_rows <- c("agriculture_group", "industry_group", "construction",
                   "trade_group", "business_services_group",
                   "other_services_group")

# Q3 rows are the four primary inputs listed above
# (Beutel also contains totals, GVA, output, taxes on products, employment,
# which are not part of the Q3 value-added block; we’ll add them later
# if you want a byte-for-byte reproduction of the demo.)
va_rows <- primary_id

# Helper to get a matrix preserving row/col order
get_matrix <- function(dat, r_ids, c_ids, rcol = row_id_col) {
  dat %>%
    filter(.data[[rcol]] %in% r_ids) %>%
    select(all_of(c(rcol, c_ids))) %>%
    arrange(factor(.data[[rcol]], levels = r_ids)) %>%
    { `rownames<-`(as.matrix(select(., all_of(c_ids))), pull(., rcol)) }
}

# Q1: industries × industries
q1 <- get_matrix(de, industry_rows, q1_id)

# Q2: industries × final demand
q2 <- get_matrix(de, industry_rows, final_use_id)

# Q3: primary inputs × industries
q3 <- get_matrix(de, va_rows, q1_id)

# --- create stacked SIOT with Quadrant 4 = NA_real_ ------------------------
siot_de <- iotable_create(
  q1 = q1, q2 = q2, q3 = q3,
  q1_id = q1_id,
  final_use_id = final_use_id,
  primary_id = primary_id,
  id_col = "prod_na"    # keep canonical name used across iotables
)

# Optional: validate closure in the Q1–Q3 core (GVA/Output rows are outside)
if (exists("iotable_validate")) {
  print(iotable_validate(siot_de, out = "report"))
}

# Bring the stacked SIOT to the same long format as germany_1995
de_long_from_new <- siot_de %>%
  # melt everything except the id column
  pivot_longer(
    cols = -prod_na,
    names_to = "t_cols2",
    values_to = "values"
  ) %>%
  rename(t_rows2 = prod_na) %>%
  mutate(
    # match Beutel practice: lower-case codes
    t_rows2 = tolower(t_rows2),
    t_cols2 = tolower(t_cols2),
    # add the same metadata you used when building germany_1995
    geo = "DE",
    geo_lab = "Germany",
    time = as.Date("1995-01-01"),
    unit = "MIO_EUR",
    unit_lab = "Million euro"
  ) %>%
  arrange(t_rows2, t_cols2)

# Example for adding GVA & Output rows if your CSV has them as rows
extra_rows <- c("gva", "output_bp", "net_tax_products",
                "employment_wage_salary", "employment_self_employed",
                "employment_domestic_total")

has_extra <- de %>%
  filter(.data[[row_id_col]] %in% extra_rows) %>%
  select(all_of(c(row_id_col, q1_id, final_use_id))) %>%
  rename(t_rows2 = !!row_id_col) %>%
  pivot_longer(
    cols = -t_rows2,
    names_to = "t_cols2",
    values_to = "values"
  ) %>%
  mutate(
    t_rows2 = tolower(t_rows2),
    t_cols2 = tolower(t_cols2),
    geo = "DE",
    geo_lab = "Germany",
    time = as.Date("1995-01-01"),
    unit = "MIO_EUR",
    unit_lab = "Million euro"
  )

de_long_from_new <- bind_rows(de_long_from_new, has_extra) %>%
  arrange(t_rows2, t_cols2)

# Assuming germany_1995 is already in your namespace (internal dataset)
# We'll compare on (t_rows2, t_cols2) pairs present in both.

common_pairs <- inner_join(
  de_long_from_new %>% select(t_rows2, t_cols2) %>% distinct(),
  germany_1995       %>% select(t_rows2, t_cols2) %>% distinct(),
  by = c("t_rows2","t_cols2")
)

cmp <- de_long_from_new %>%
  semi_join(common_pairs, by = c("t_rows2","t_cols2")) %>%
  select(t_rows2, t_cols2, values_new = values) %>%
  inner_join(
    germany_1995 %>%
      semi_join(common_pairs, by = c("t_rows2","t_cols2")) %>%
      select(t_rows2, t_cols2, values_old = values),
    by = c("t_rows2","t_cols2")
  ) %>%
  mutate(diff = values_new - values_old)

# Inspect maximum absolute difference
summary(abs(cmp$diff))
# See any mismatches
subset(cmp, abs(diff) > 1e-8) %>% arrange(desc(abs(diff)))

library(testthat)
# tests/testthat/test-germany_1995-recreate.R

test_that("new iotable_create reproduces germany_1995 core cells", {
  # source("data-raw/germany_1995_recreate.R")  # or inline the steps here
  # we assume objects de_long_from_new and germany_1995 exist
  
  common <- dplyr::inner_join(
    dplyr::select(de_long_from_new, t_rows2, t_cols2) %>% dplyr::distinct(),
    dplyr::select(germany_1995,     t_rows2, t_cols2) %>% dplyr::distinct(),
    by = c("t_rows2","t_cols2")
  )
  
  cmp <- de_long_from_new %>%
    dplyr::semi_join(common, by = c("t_rows2","t_cols2")) %>%
    dplyr::select(t_rows2, t_cols2, values_new = values) %>%
    dplyr::inner_join(
      germany_1995 %>%
        dplyr::semi_join(common, by = c("t_rows2","t_cols2")) %>%
        dplyr::select(t_rows2, t_cols2, values_old = values),
      by = c("t_rows2","t_cols2")
    ) %>%
    dplyr::mutate(diff = values_new - values_old)
  
  expect_true(all(abs(cmp$diff) < 1e-8 | (is.na(cmp$diff) &
                                            is.na(cmp$values_new) & is.na(cmp$values_old))))
})
