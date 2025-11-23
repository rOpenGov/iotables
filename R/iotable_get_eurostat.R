#' Get a single Eurostat input–output table
#'
#' @description
#' Filters and reshapes one IO/SUT table from a bulk Eurostat dataset.
#' This version excludes all built-in datasets
#' (e.g. Germany 1995, Croatia 2010, UK 2010) which are retrieved with
#' the [iotable_get()] function.
#'
#' @details
#' The Eurostat bulk tables arrive in long form and are not ordered for
#' matrix algebra. This function selects the requested country (`geo`),
#' year, unit and stock/flow (`stk_flow`), joins iotables metadata for
#' consistent row/column labelling, and returns a **wide** table ready
#' for analysis.
#'
#' @inheritParams iotable_get
#' @importFrom forcats fct_reorder
#' @importFrom utils packageVersion
#' @importFrom glue glue
#' @importFrom dplyr rename transmute left_join distinct filter
#' @importFrom tidyr pivot_wider
#' @return
#' A **wide** `data.frame` representing the selected IO table.
#' @export
iotable_get_eurostat <- function(
    source = "naio_10_cp1700",
    geo = "IT",
    year = 2022,
    unit = "MIO_EUR",
    stk_flow = "TOTAL",
    labelling = "iotables",
    data_directory = NULL,
    force_download = TRUE,
    labelled_io_data = NULL) {
  if (labelling == "eurostat") labelling <- "short"

  # --- Validation ------------------------------------------------------------
  if (is.null(source)) stop("Parameter 'source' is mandatory.")
  if (is.null(labelled_io_data) & is.null(geo)) {
    stop("The 'geo' parameter must be a valid Eurostat 'geo' code")
  }

  validate_source(source)

  if (source %in% c(
    "naio_10_cp1620", "naio_10_cp1630",
    "naio_10_pyp1620", "naio_10_pyp1630"
  )) {
    stk_flow <- "TOTAL"
  }

  # --- Download --------------------------------------------------------------
  if (is.null(labelled_io_data)) {
    io_filters <- list(
      geo = geo,
      stk_flow = stk_flow,
      unit = unit
    )

    labelled_io_table <- get_eurostat_filtered(
      id = source,
      filter = io_filters
    )
    if (!is.null(year)) {
      labelled_io_table <- labelled_io_table[labelled_io_table$year == year, ]
      if (length(labelled_io_table) == 0) {
        err_msg <- glue::glue("No data available in source='{source}' for the year='{year}'.")
        stop(err_msg)
      }
    }
  }

  message("Retrieved dataset from Eurostat.")

  # --- Locate table ----------------------------------------------------------

  if (!is.null(labelled_io_data)) {
    selected_table <- which(
      tolower(labelled_io_data$geo) == toupper(geo) &
        labelled_io_data$year == as.numeric(year) &
        toupper(labelled_io_data$unit) == toupper(unit) &
        toupper(labelled_io_data$stk_flow) == toupper(stk_flow)
    )
    if (length(selected_table) == 0) {
      stop("No matching table found for geo/year/unit/stk_flow.")
    }
    iotable <- labelled_io_data$data[[selected_table]]
  } else {
    iotable <- labelled_io_table
  }

  # --- Establish the type of SIOT -----------------------------------

  structure_type <- NA_character_
  row_voc <- col_voc <- NA_character_

  row_vars <- c("prod_na", "prd_ava", "ind_ava", "primary_inputs")
  col_vars <- c("induse", "prd_use", "ind_use", "final_use")

  if (all(c("prd_ava", "prd_use") %in% names(iotable))) {
    structure_type <- "product_product"

    row_voc <- row_vars[which(row_vars %in% names(iotable))]
    col_voc <- col_vars[which(col_vars %in% names(iotable))]

    row_vocab <- getdata(row_voc)
    col_vocab <- getdata(col_voc)

    row_mismatch <- unique(iotable$prd_ava)[
      which(!unique(iotable$prd_ava) %in% unique(row_vocab$id))
    ]

    col_mismatch <- unique(iotable$prd_use)[
      which(!unique(iotable$prd_use) %in% unique(col_vocab$id))
    ]

    assertthat::assert_that(
      length(row_mismatch) == 0,
      msg = glue::glue(
        "Mismatch of row name vocabulary in '{source}' geo={geo} year={year}"
      )
    )

    assertthat::assert_that(
      length(col_mismatch) == 0,
      msg = glue::glue(
        "Mismatch of col name vocabulary in '{source}' geo={geo} year={year}"
      )
    )

    iotable <- iotable %>%
      dplyr::rename(
        prod_na     = prd_ava, # rows
        induse      = prd_use, # columns
      )

    if ("prod_ava_lab" %in% names(iotable)) {
      iotable <- rename(iotable, prod_na_lab = prd_ava_lab)
    } else {
      iotable$prod_na_lab <- NA_character_
    }

    if ("induse_lab" %in% names(iotable)) {
      iotable <- rename(iotable, induse_lab = prd_use_lab)
    } else {
      iotable$induse_lab <- NA_character_
    }
    message("Detected product × product (CPA) structure.")
  }

  if (all(c("ind_ava", "ind_use") %in% names(iotable))) {
    structure_type <- "industry_industry"

    row_voc <- row_vars[which(row_vars %in% names(iotable))]
    col_voc <- col_vars[which(col_vars %in% names(iotable))]

    row_vocab <- getdata(row_voc)
    col_vocab <- getdata(col_voc)

    row_mismatch <- unique(iotable$ind_ava)[
      which(!unique(iotable$ind_ava) %in% unique(row_vocab$id))
    ]

    col_mismatch <- unique(iotable$ind_use)[
      unique(iotable$ind_use)[which(!unique(iotable$ind_use) %in% unique(col_vocab$id))]
    ]

    assertthat::assert_that(
      length(row_mismatch) == 0,
      msg = glue::glue(
        "Mismatch of row name vocabulary in '{source}' geo={geo} year={year}"
      )
    )

    assertthat::assert_that(
      length(col_mismatch) == 0,
      msg = glue::glue(
        "Mismatch of col name vocabulary in '{source}' geo={geo} year={year}"
      )
    )

    iotable <- iotable %>%
      dplyr::rename(
        prod_na     = ind_ava, # rows
        induse      = ind_use, # columns
      )

    if ("ind_ava_lab" %in% names(iotable)) {
      iotable <- rename(iotable, prod_na_lab = ind_ava_lab)
    } else {
      prod_na_lab <- NA_character_
    }

    if ("induse_lab" %in% names(iotable)) {
      iotable <- rename(iotable, induse_lab = ind_use_lab)
    } else {
      induse_lab <- NA_character_
    }

    message("Detected industry × industry (NACE) structure.")
  }

  if (is.na(structure_type)) {
    review <- glue::glue(
      "https://ec.europa.eu/eurostat/databrowser/view/",
      source, "/default/"
    )
    stop(
      "The data received from\n", review,
      "\ndo not align with the expected metadata or structure."
    )
  }

  # --- Ensure numeric -------------------------------------------------------
  if (is.character(iotable$values) || is.factor(iotable$values)) {
    iotable$values <- as.numeric(trimws(as.character(iotable$values)))
  }

  # --- Load metadata  ------------------------------------------------
  # Dynamically choose which vocabularies to use for rows and columns
  metadata_row <- getdata(row_voc)
  metadata_col <- getdata(col_voc)

  meta_rows <- metadata_row %>%
    dplyr::transmute(
      prod_na = as.character(id),
      prod_na_lab_ref = as.character(label),
      row_order = numeric_order,
      iotables_row = as.character(iotables_label)
    ) %>%
    dplyr::distinct(prod_na, .keep_all = TRUE)

  meta_cols <- metadata_col %>%
    dplyr::transmute(
      induse = as.character(id),
      induse_lab_ref = as.character(label),
      col_order = numeric_order,
      iotables_col = as.character(iotables_label)
    ) %>%
    dplyr::distinct(induse, .keep_all = TRUE)

  # --- Join metadata ---------------------------------------------------------
  iotable_labelled <- iotable %>%
    dplyr::left_join(meta_cols, by = "induse") %>%
    dplyr::left_join(meta_rows, by = "prod_na")

  unlabelled_col <- iotable_labelled %>%
    dplyr::filter(is.na(col_order)) %>%
    dplyr::filter(!is.na(values))

  if (nrow(unlabelled_col) > 0) {
    warning(glue::glue(
      "There are unidentified cells in '{source}' geo='{geo}' year='{year}'"
    ))
  }

  if (nrow(iotable_labelled) == 0) {
    stop("No matching rows after metadata join.")
  }

  # --- Reorder factors -------------------------------------------------------
  if (!is.null(iotable_labelled$row_order)) {
    # reorder for iotables labelling ------------------
    iotable_labelled$iotables_row <- forcats::fct_reorder(
      iotable_labelled$iotables_row,
      as.numeric(iotable_labelled$row_order),
      .na_rm = TRUE
    )
  }

  if (!is.null(iotable_labelled$row_order)) {
    # reorder for eurostat short code labelling -----
    iotable_labelled$prod_na <- forcats::fct_reorder(
      iotable_labelled$prod_na,
      as.numeric(iotable_labelled$row_order),
      .na_rm = TRUE
    )
  }

  if (!is.null(iotable_labelled$col_order)) {
    # reorder for iotables labelling ----------------
    iotable_labelled$iotables_col <- forcats::fct_reorder(
      iotable_labelled$iotables_col,
      as.numeric(iotable_labelled$col_order),
      .na_rm = TRUE
    )
  }

  if (!is.null(iotable_labelled$col_order)) {
    # reorder for eurostat short code labelling -----
    iotable_labelled$induse <- forcats::fct_reorder(
      iotable_labelled$induse,
      as.numeric(iotable_labelled$col_order),
      .na_rm = TRUE
    )
  }

  # --- Pivot to wide (SIOT form) --------------------------------------------
  if (labelling == "iotables") {
    out <- iotable_labelled %>%
      dplyr::arrange(iotables_row, iotables_col) %>%
      dplyr::select(iotables_row, iotables_col, values) %>%
      tidyr::pivot_wider(
        names_from = iotables_col,
        values_from = values
      )
  } else {
    out <- iotable_labelled %>%
      dplyr::arrange(prod_na, induse) %>%
      dplyr::select(prod_na, induse, values) %>%
      tidyr::pivot_wider(names_from = induse, values_from = values)
  }

  # --- Attach metadata back --------------------------------------------------

  if (!is.null(labelled_io_data)) {
    out <- out %>%
      tibble::add_column(
        geo = labelled_io_data$geo[[selected_table]],
        year = labelled_io_data$year[[selected_table]],
        unit = labelled_io_data$unit[[selected_table]],
        stk_flow = labelled_io_data$stk_flow[[selected_table]],
        .before = 1
      )
  }

  ver_n <- as.character(utils::packageVersion("iotables"))
  finish_time <- as.character(Sys.time())
  prov_text <- glue::glue(
    "Created with R and the iotables library {ver_n} at {finish_time}."
  )

  attr(out, "dataset_source") <- source
  attr(out, "geo") <- geo
  attr(out, "unit") <- unit
  attr(out, "year") <- year
  attr(out, "stk_flow") <- stk_flow
  attr(out, "structure_type") <- structure_type
  attr(out, "provenance") <- prov_text


  # --- Save if requested -----------------------------------------------------
  if (!is.null(data_directory)) {
    saveRDS(out, file.path(
      data_directory,
      paste0(geo, "_", year, "_", source, "_", stk_flow, "_", unit, ".rds")
    ),
    version = 2
    )
  }
  
  # Normalise row + column names to uppercase to ensure symmetry 
  # and joinability if not the custom iotables snake case names are 
  # used
  if (labelling != "iotables") {
    names(out) <- toupper(names(out))
    if ("prod_na" %in% names(out)) {
      out$prod_na <- toupper(out$prod_na)
    }
  }
  
  out
}

