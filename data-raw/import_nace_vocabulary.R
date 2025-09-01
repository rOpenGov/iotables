# data-raw/nace_build.R
# Parse Eurostat NACE 2.1 from RDF (SKOS) and save internal dataset.

# install.packages(c("rdflib","dplyr","tibble","stringr","tidyr","purrr","usethis"))
library(rdflib)
library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
library(purrr)
library(usethis)

# 1) Point to your local RDF (ShowVoc export)
rdf_file <- here::here("data-raw", "ESTAT-NACE2.1.rdf") # adjust path if needed

# 2) Parse RDF/XML
g <- rdflib::rdf_parse(rdf_file, format = "rdfxml")

# 3) SPARQL to list all concepts with notation, label(s), broader, scheme
#    - skos:notation -> code, skos:prefLabel -> labels
#    - skos:broader -> parent concept URI for hierarchy
#    - skos:inScheme -> scheme URI
q <- "
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?concept ?notation ?label ?lang ?broader ?scheme WHERE {
  ?concept a skos:Concept ;
           skos:inScheme ?scheme .
  OPTIONAL { ?concept skos:notation ?notation . }
  OPTIONAL {
    ?concept skos:prefLabel ?label .
    BIND(LANG(?label) AS ?lang)
  }
  OPTIONAL { ?concept skos:broader ?broader . }
}
"

en <- "
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT ?concept ?notation ?label ?broader ?scheme
WHERE {
  ?concept a skos:Concept ;
           skos:inScheme ?scheme .
  OPTIONAL { ?concept skos:notation ?notation . }
  OPTIONAL {
    ?concept skos:prefLabel ?label .
    FILTER(LANGMATCHES(LANG(?label), 'en'))
  }
  OPTIONAL { ?concept skos:broader ?broader . }
}
LIMIT 100
"

raw <- rdflib::rdf_query(g, en) %>% as_tibble()

# 4) Tidy up: coerce to character, keep useful cols
raw <- raw %>%
  mutate(
    concept = as.character(concept),
    notation = as.character(notation),
    label = as.character(label),
    lang = as.character("en"),
    broader = as.character(broader),
    scheme = as.character(scheme)
  )

# 5) Decide on language (keep all, or filter to 'en')
keep_lang <- "en"
nace_labels <- raw %>%
  filter(!is.na(label)) %>%
  group_by(concept, notation, broader, scheme) %>%
  # prefer keep_lang; fallback to first available
  arrange(desc(lang == keep_lang)) %>%
  slice(1L) %>%
  ungroup() %>%
  transmute(
    uri = concept,
    code = coalesce(notation, uri),
    label = label,
    lang = lang,
    parent_uri = broader,
    in_scheme = scheme
  )

# 6) Compute parent_code and a simple "level" (by code structure if present)
#    Many NACE notations are letter+digits; derive a naive level.
nace_labels <- nace_labels %>%
  mutate(
    parent_code = NA_character_
  )

# Map parent_code by joining parent_uri back to a code
uri_to_code <- nace_labels %>%
  select(uri, code) %>%
  distinct()

nace_labels <- nace_labels %>%
  left_join(uri_to_code,
    by = c("parent_uri" = "uri"),
    multiple = "first",
    relationship = "many-to-one"
  ) %>%
  rename(parent_code = code.y) %>%
  rename(code = code.x)

# Optional: derive a shallow "level" from notation shape
# (You can refine to match NACE rules.)
nace_labels <- nace_labels %>%
  mutate(
    level = case_when(
      str_detect(code, "^[A-Z]$") ~ 1L, # Section (A..U)
      str_detect(code, "^[A-Z][0-9]{2}$") ~ 2L, # Division (e.g. C10)
      str_detect(code, "^[0-9]{2}$") ~ 2L, # some exports omit letter
      str_detect(code, "^[0-9]{2}\\.[0-9]$") ~ 3L,
      str_detect(code, "^[0-9]{2}\\.[0-9]{2}$") ~ 4L,
      TRUE ~ NA_integer_
    )
  )

# 7) Minimal sanity checks
stopifnot(nrow(nace_labels) > 0)
stopifnot(any(!is.na(nace_labels$label)))

# 8) Save internal dataset
#    - small: `usethis::use_data(nace_labels, internal = TRUE)`
#    - large: consider compress or split by scheme/version
usethis::use_data(nace_labels, internal = TRUE, overwrite = TRUE)
