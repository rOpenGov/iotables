#' @importFrom dplyr filter select mutate rename left_join arrange all_of
#' @importFrom dplyr across where
#' @importFrom tidyr pivot_wider
#' @importFrom forcats fct_reorder
#' @importFrom utils data

#' @keywords internal
order_iotable <- function(iotable, stk_flow, source, labelling) {
  # Logically this should be called from iotables_download, too.
  # But now it is called from iotable_get.

  croatia_files <- c("croatia_2010_1700", "croatia_2010_1800", "croatia_2010_1900")
  uk_tables <- c("uk_2010_siot", "uk_2010_use", "uk_2010_imports", "uk_2010_coeff", "uk_2010_inverse")

  ## Exception handling for tax and margin tables ------------------------------------------------
  stk_flow_input <- adjust_stk_flow(stk_flow = stk_flow, source = source)

  ## Define the tables that follow the prod_ind vocabulary ---------------------------------------
  prod_ind <- define_prod_ind()

  ## Getting the vocabulary information ----------------------------------------------------------
  ## Source file is utils-metadata.R

  if (source %in% uk_tables) {
    metadata_uk_2010 <- getdata("metadata_uk_2010")
    metadata_cols <- metadata_uk_2010 %>%
      filter(!is.na(uk_col)) %>%
      select(-all_of(c("uk_row", "uk_row_label", "prod_na", "row_order"))) %>%
      mutate(uk_col = gsub("\\.", "-", as.character(uk_col))) %>%
      mutate(uk_col = gsub(" & ", "-", as.character(uk_col))) %>%
      mutate(uk_col = trimws(uk_col, "both"))

    metadata_rows <- metadata_uk_2010 %>%
      filter(!is.na(uk_row)) %>%
      select(-all_of(c("uk_col", "uk_col_label", "induse", "col_order"))) %>%
      mutate(uk_row = gsub("\\.", "-", as.character(uk_row))) %>%
      mutate(uk_row = gsub(" & ", "-", as.character(uk_row)))
  } else {
    metadata_rows <- get_metadata_rows(source) %>% mutate(across(where(is.factor), as.character))
    metadata_cols <- get_metadata_cols(source) %>% mutate(across(where(is.factor), as.character))
  }

  ## Factor reordering & row ordering -------------------------------------------------------------
  if (source %in% c(prod_ind, uk_tables)) {
    ## Ordering IOTs following the prod_ind vocabulary
    ## First define the joining variables for left_join with metadata
    col_join <- names(iotable)[which(names(iotable) %in% c("induse", "induse_lab", "iotables_col"))]
    row_join <- names(iotable)[which(names(iotable) %in% c("prod_na", "prod_na_lab", "iotables_row"))]

    ## Define the variables that will not be used from the metadata
    remove_vars <- c(
      "quadrant", "account_group", "variable",
      "group", "eu_prod_na"
    )
    remove_vars <- remove_vars[remove_vars %in% names(metadata_cols)]

    if ("stk_flow" %in% names(iotable)) {
      # The germany_1995 files have no stk_input columns.
      iotable_labelled <- iotable %>%
        filter(stk_flow == stk_flow_input)
    } else {
      iotable_labelled <- iotable
    }

    iotable_labelled <- iotable_labelled %>%
      mutate(across(where(is.factor), as.character)) %>%
      left_join(metadata_cols, by = col_join) %>%
      select(-all_of(remove_vars)) %>% # remove repeating columns before joining rows
      mutate(across(where(is.factor), as.character)) %>%
      left_join(metadata_rows, by = row_join)

    if (nrow(iotable_labelled) == 0) {
      stop(
        "No rows found with parameters order_iotable(source='", source,
        "' labelling= '", labelling, "' and stk_flow = '", stk_flow, "')."
      )
    }

    ## Do the reordering if the metadata variable is called prod_na
    iotable_labelled <- iotable_labelled %>%
      arrange(row_order, col_order) %>%
      mutate(prod_na = fct_reorder(prod_na, as.numeric(row_order))) %>%
      mutate(induse = fct_reorder(induse, as.numeric(col_order)))

    if (all(c("uk_row", "uk_col") %in% names(iotable_labelled))) {
      iotable_labelled <- iotable_labelled %>%
        mutate(iotables_row = fct_reorder(uk_row, as.numeric(row_order))) %>%
        mutate(iotables_col = fct_reorder(uk_col, as.numeric(col_order)))
    }

    if (all(c("iotables_row", "iotables_col") %in% names(iotable_labelled))) {
      iotable_labelled <- iotable_labelled %>%
        mutate(iotables_row = fct_reorder(iotables_row, as.numeric(row_order))) %>%
        mutate(iotables_col = fct_reorder(iotables_col, as.numeric(col_order)))
    }
  } else if (!source %in% croatia_files) {
    ## Ordering IOTs that do not follow the prod_na vocabulary
    if (all(c("uk_row", "uk_col") %in% names(iotable_labelled))) {
      iotable_labelled <- iotable_labelled %>%
        mutate(iotables_row = fct_reorder(uk_row, as.numeric(row_order))) %>%
        mutate(iotables_col = fct_reorder(uk_col, as.numeric(col_order)))
    }
    if (all(c("iotables_row", "iotables_col") %in% names(iotable_labelled))) {
      iotable_labelled <- iotable_labelled %>%
        mutate(iotables_row = fct_reorder(iotables_row, as.numeric(row_order))) %>%
        mutate(iotables_col = fct_reorder(iotables_col, as.numeric(col_order)))
    }
  } else {
    ## This is the exception for Croatia
    iotable_labelled <- iotable

    # First join the necessary vocabulary from the metadata...
    by_col <- names(iotable)[which(names(iotable) %in% c("t_cols2", "t_cols2_lab", "iotables_col"))]
    by_row <- names(iotable)[which(names(iotable) %in% c("t_rows2", "t_rows2_lab", "iotables_row"))]

    iotable_labelled <- iotable %>%
      mutate(across(where(is.factor), as.character)) %>%
      left_join(metadata_cols, by = by_col) %>%
      left_join(metadata_rows, by = by_row) %>%
      arrange(row_order, col_order)

    iotable_labelled <- iotable_labelled %>%
      arrange(row_order, col_order) %>% # ?needed
      mutate(t_rows2 = fct_reorder(t_rows2, as.numeric(row_order))) %>%
      mutate(t_cols2 = fct_reorder(t_cols2, as.numeric(col_order))) %>%
      mutate(iotables_row = fct_reorder(iotables_row, as.numeric(row_order))) %>%
      mutate(iotables_col = fct_reorder(iotables_col, as.numeric(col_order)))
  }


  ## selecting which labelling to use -------------------------------------------------
  if (labelling == "iotables") {
    ## Only one labelling can be selected, start with the
    ## internal package  'iotables' labelling
    iotable_labelled_w <- iotable_labelled %>%
      arrange(iotables_row, iotables_col) %>%
      select(all_of(c("iotables_col", "iotables_row", "values"))) %>%
      pivot_wider(names_from = iotables_col, values_from = values)
  } else if (labelling == "short" & source %in% prod_ind) {
    ## Labelling with the Eurostat prod_ind vocabulary
    iotable_labelled_w <- iotable_labelled %>%
      select(all_of(c("prod_na", "induse", "values"))) %>%
      filter(!is.na(prod_na)) %>%
      pivot_wider(names_from = induse, values_from = values)
  } else if (source %in% uk_tables) {
    iotable_labelled_w <- iotable_labelled %>%
      select(all_of(c("uk_row", "uk_col", "values"))) %>%
      filter(!is.na(uk_row)) %>%
      pivot_wider(names_from = uk_col, values_from = values)
  } else {
    ## Labelling with the special Croatia replication files
    iotable_labelled_w <- iotable_labelled %>%
      select(all_of(c("t_rows2", "t_cols2", "values"))) %>%
      pivot_wider(names_from = t_cols2, values_from = values)
  }

  # Return the labelled IOT in wide format:
  iotable_labelled_w
}
