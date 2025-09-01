old_table_selection_function <- function(labelled_io_data, year_input, geo_input, unit_input, stk_flow_input) {
  ## This internal function used to be a part of iotable_get() and it is no longer in use.
  ## It is replaced by get_saved_table().

  selected_table <- which( ## get the number of table to be selected
    labelled_io_data$year == year_input &
      as.character(labelled_io_data$geo) == geo_input &
      labelled_io_data$unit == unit_input
  )

  if (length(selected_table) == 0) {
    stop(paste0(
      "There is no available table for country ", geo_input,
      " in the year ", year_input,
      " with ", unit_input, " units."
    ))
  } else if (length(selected_table) == 3) {
    selected_table <- which( ## get the number of table to be selected
      labelled_io_data$year == year_input &
        as.character(labelled_io_data$geo) == geo_input &
        labelled_io_data$unit == unit_input &
        labelled_io_data$stk_flow == stk_flow_input
    )
  }

  if (length(selected_table) != 1) {
    stop(
      "The parameters geo=", geo, "; unit=", unit_input,
      "; stk_flow=", stk_flow_input,
      "\ndo not select a unique table."
    )
  }

  iotable <- labelled_io_data$data[[selected_table]]
}


## old_table_selection_function in data-raw/old_table_selection_function.R

#' @keywords internal
not_in_use <- function() {
  tmp_iot <- iot %>%
    unnest(iot, cols = "data") %>%
    select(any_of(c("prod_na", "induse", "values")))

  ## How to find out if it has prod_na?

  labelling_information <- tmp_iot %>%
    dplyr::distinct(across(any_of(c("trows", "prod_na", "induse")))) %>%
    rename(code = .data$prod_na) %>%
    left_join(metadata, by = "code") %>%
    arrange(.data$numeric_label)

  if (labelling != "iotables") {
    left_join(
      labelling_information %>%
        select(prod_na = .data$code) %>%
        distinct(.data$prod_na),
      tmp_iot %>%
        pivot_wider(names_from = "induse", values_from = "values"),
      by = "prod_na"
    )
  } else {
    stop("this is not yet working")

    induse_labels <- labelling_information %>%
      select(induse = .data$code, .data$iotables_label) %>%
      distinct(across(all_of(c("induse", "iotables_label")))) %>%
      semi_join(tmp_iot %>% select(any_of("induse")))

    prod_na_labels <- labelling_information %>%
      select(prod_na = .data$code, .data$iotables_label) %>%
      distinct(across(all_of(c("prod_na", "iotables_label")))) %>%
      semi_join(tmp_iot %>% select(any_of("prod_na")), by = "prod_na")

    dpulicates <- prod_na_labels %>%
      left_join(tmp_iot, by = "prod_na") %>%
      add_count(.data$prod_na, .data$induse) %>%
      filter(n > 1)

    rename(iotables_row = .data$iotables_label) %>%
      select(-.data$prod_na) %>%
      left_join(induse_labels) %>%
      select(-.data$induse) %>%
      pivot_wider(names_from = "iotables_label", values_from = "values")
  }
}
