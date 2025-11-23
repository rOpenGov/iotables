devtools::load_all()
get_distinct_table <- function(x, nested_data) {
  nested_data$data[[x]] %>%
    dplyr::select(
      starts_with("prd"),
      starts_with("ind")
    ) %>%
    dplyr::distinct_all() %>%
    dplyr::mutate(
      unit = nested_data$unit[x],
      stk_flow = nested_data$stk_flow[x],
      geo = nested_data$geo[x],
      year = nested_data$year[x]
    )
}
source_id <- "naio_10_pyp1630"
create_metadata_catalogue <- function(source_id) {
  file_name <- here::here(
    "data-raw",
    paste0(source_id, "_metadata_catalogue.rds")
  )

  message("Working on ", file_name)

  message("1/5 downloading")

  nested_data <- iotables_download(
    source = source_id,
    data_directory = "data-raw"
  )

  message("2/5 nested data process")

  nested_metadata <- lapply(
    seq_along(nested_data$data),
    function(x) get_distinct_table(x, nested_data)
  )

  message("3/5 binding")
  nested_metadata_df <- do.call(rbind, nested_metadata)

  message("4/5 distinct")
  nested_metadata_df_distinct <- nested_metadata_df %>%
    distinct_all()

  message("5/5 saving:", file_name)
  saveRDS(object = nested_metadata_df_distinct, file = file_name)

  code_file_name <- gsub("_catalogue", "_codes", file_name)

  save_code_list <- function(df, code_file_name) {
    df %>%
      select(
        -any_of(c(
          "stk", "stk_lab", "stk_flow", "geo", "geo_lab",
          "unit", "unit_lab", "year"
        ))
      ) %>%
      distinct_all() %>%
      mutate_all(as.character) %>%
      saveRDS(file = code_file_name)
  }

  safe_code_list <- purrr::safely(save_code_list)
  safe_code_list(df = nested_metadata_df_distinct, code_file_name = code_file_name)
  gc()
}

safely_create_metadata_catalogue <- purrr::safely(
  .f = create_metadata_catalogue
)


fn_create_meta_cat <- function(x) {
  message(x)
  safely_create_metadata_catalogue(x)
}


lapply(c(
  "naio_10_pyp1630",
  "naio_10_cp1700",
  "naio_10_cp1750",
  "naio_10_pyp1700",
  "naio_10_pyp1750",
  "naio_10_cp1610",
  "naio_10_pyp1610",
  "naio_10_cp1620",
  "naio_10_pyp1620",
  "naio_10_cp1630",
  "naio_10_cp15", "naio_10_cp16"
), function(x) fn_create_meta_cat(x))


readRDS(file = here::here("data-raw", "naio_10_pyp1630_metadatacodes.rds"))
