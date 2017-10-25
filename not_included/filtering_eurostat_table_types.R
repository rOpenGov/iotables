library ( dplyr) ; library ( tidyr)
filter ( source  == "naio_10_pyp1700")
a <- t_rows_values %>%
  filter (  source  %in% c("naio_10_pyp1700",  "naio_10_cp1700")) %>%
  mutate_if ( is.factor, as.character ) %>%
  select ( prod_na_lab, prod_na, source ) %>%
  spread (  source, prod_na  ) %>%
  dplyr::rename ( label = prod_na_lab ) 

b <- t_cols_values %>%
  filter (  source  %in% c("naio_10_pyp1700",  "naio_10_cp1700")) %>%
  mutate_if ( is.factor, as.character ) %>%
  select ( induse_lab, induse, source ) %>%
  spread (  source, induse  ) %>%
  dplyr::rename ( label = induse_lab)

c <- t_rows_values %>%
  filter ( ! source  %in% c("naio_10_pyp1700",  "naio_10_cp1700")) %>%
  mutate_if ( is.factor, as.character ) %>%
  select ( prod_na_lab, prod_na, source ) %>%
  spread (  source, prod_na  ) %>%
  dplyr::rename ( label = prod_na_lab)

f <- t_cols_values %>%
  filter ( ! source  %in% c("naio_10_pyp1700",  "naio_10_cp1700")) %>%
  mutate_if ( is.factor, as.character ) %>%
  select ( induse_lab, induse, source) %>%
  spread (  source, induse  ) %>%
  dplyr::rename ( label = induse_lab)

d <- full_join (a, c )

g <- full_join (b, f)

e <- croatia_2010_1700

d <- a %>%
  dplyr::rename ( label = prod_na_lab ) 

#write.csv ( d, "eurostat_tables.csv", row.names = F)
D <- read.csv ( "eurostat_tables.csv", stringsAsFactors = F)

G <- full_join 

write.csv ( G, "eurostat_tables_cols.csv", row.names = F)
