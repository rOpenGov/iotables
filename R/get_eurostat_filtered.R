#' @keywords internal
#' @importFrom dplyr relocate filter
#' @importFrom httr2 req_user_agent req_retry req_error
#' @importFrom httr2 resp_body_json req_perform resp_content_type
get_eurostat_filtered <- function(id,
                                  filters = list(),
                                  type = "code",
                                  lang = "en",
                                  ...) {
  # ---- Build query URL ------------------------------------------------------
  base_url <- sprintf(
    "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/%s",
    id
  )

  if (length(filters)) {
    query <- paste0(
      names(filters), "=",
      vapply(filters, paste, collapse = ",", FUN.VALUE = character(1)),
      collapse = "&"
    )
    base_url <- paste0(base_url, "?", query)
  }

  # ---- Download JSON --------------------------------------------------------
  jdat <- httr2::request(base_url) %>%
    httr2::req_user_agent("https://github.com/eviota/iotables") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 60) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform() %>%
    {
      if (httr2::resp_content_type(.) != "application/json") {
        stop("Eurostat API did not return JSON", call. = FALSE)
      }
      httr2::resp_body_json(., simplifyVector = TRUE)
    }

  dims <- jdat$dimension
  ids <- jdat$id

  # ---- Construct tidy grid --------------------------------------------------
  dims_list <- lapply(dims[rev(ids)], function(x) {
    lab <- x$category$label
    if (identical(type, "label")) {
      unname(unlist(lab, use.names = FALSE))
    } else if (identical(type, "code")) {
      names(lab)
    } else if (identical(type, "both")) {
      unlist(lab)
    } else {
      stop("Invalid 'type' argument.")
    }
  })

  downloaded <- expand.grid(
    dims_list,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      values = NA_real_,
      values = {
        vals <- unlist(jdat$value, use.names = FALSE)
        inds <- 1 + as.numeric(names(jdat$value))
        replace(values, inds, vals)
      }
    )

  downloaded <- tibble::rowid_to_column(downloaded)

  # ---- Filter sparce matrix  -----------------------------------
  if (all(c("prd_ava", "prd_use") %in% names(downloaded))) {
    # Product x Product Structure  --------

    row_vocab <- getdata("prd_ava")
    col_vocab <- getdata("prd_use")

    not_in_prd_ava <- downloaded %>%
      filter(is.na(values)) %>%
      filter(!prd_ava %in% row_vocab)

    not_in_prd_use <- downloaded %>%
      filter(is.na(values)) %>%
      filter(!prd_ava %in% col_vocab)

    valid_rows <- setdiff(
      downloaded$rowid,
      c(not_in_prd_ava$rowid, not_in_prd_use$rowid)
    )

    downloaded <- downloaded %>%
      filter(rowid %in% valid_rows) %>%
      select(-rowid)
  }

  if (all(c("ind_ava", "ind_use") %in% names(downloaded))) {
    # Industry x Industry Structure  --------

    row_vocab <- getdata("ind_ava")
    col_vocab <- getdata("ind_use")

    not_in_ind_ava <- downloaded %>%
      filter(is.na(values)) %>%
      filter(!ind_ava %in% row_vocab)

    not_in_ind_use <- downloaded %>%
      filter(is.na(values)) %>%
      filter(!ind_ava %in% col_vocab)

    valid_rows <- setdiff(
      downloaded$rowid,
      c(not_in_ind_ava$rowid, not_in_ind_use$rowid)
    )

    downloaded <- downloaded %>%
      filter(rowid %in% valid_rows) %>%
      select(-rowid)
  }


  # ---- Harmonise time field -------------------------------------------------
  if ("time" %in% names(downloaded)) {
    downloaded <- downloaded %>%
      dplyr::rename(TIME_PERIOD = time) %>%
      dplyr::mutate(TIME_PERIOD = as.Date(
        paste0(TIME_PERIOD, "-01-01")
      ))
  }

  attr(downloaded, "dataset") <- id

  downloaded <- tibble::rowid_to_column(downloaded)
  new_names <- names(downloaded)
  change <- which(!names(downloaded) %in% c("rowid", "TIME_PERIOD", "values"))
  new_names[change] <- paste0(new_names[change], "_lab")

  downloaded$year <- lubridate::year(downloaded$TIME_PERIOD)
  invisible(gc())
  downloaded
}


#' Use this function for non-SIOT data like air pollutants or 
#' employment
#' @keywords internal
#' @importFrom dplyr relocate filter
#' @importFrom httr2 req_user_agent req_retry req_error
#' @importFrom httr2 resp_body_json req_perform resp_content_type
get_eurostat_data <- function(id,
                                  filters = list(),
                                  type = "code",
                                  lang = "en",
                                  ...) {
  # ---- Build query URL ------------------------------------------------------
  base_url <- sprintf(
    "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/%s",
    id
  )

  if (length(filters)) {
    query <- paste0(
      names(filters), "=",
      vapply(filters, paste, collapse = ",", FUN.VALUE = character(1)),
      collapse = "&"
    )
    base_url <- paste0(base_url, "?", query)
  }

  # ---- Download JSON --------------------------------------------------------
  jdat <- httr2::request(base_url) %>%
    httr2::req_user_agent("https://github.com/eviota/iotables") %>%
    httr2::req_retry(max_tries = 3, max_seconds = 60) %>%
    httr2::req_error(is_error = function(resp) FALSE) %>%
    httr2::req_perform() %>%
    {
      if (httr2::resp_content_type(.) != "application/json") {
        stop("Eurostat API did not return JSON", call. = FALSE)
      }
      httr2::resp_body_json(., simplifyVector = TRUE)
    }

  dims <- jdat$dimension
  ids <- jdat$id

  # ---- Construct tidy grid --------------------------------------------------
  dims_list <- lapply(dims[rev(ids)], function(x) {
    lab <- x$category$label
    if (identical(type, "label")) {
      unname(unlist(lab, use.names = FALSE))
    } else if (identical(type, "code")) {
      names(lab)
    } else if (identical(type, "both")) {
      unlist(lab)
    } else {
      stop("Invalid 'type' argument.")
    }
  })

  downloaded <- expand.grid(
    dims_list,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      values = NA_real_,
      values = {
        vals <- unlist(jdat$value, use.names = FALSE)
        inds <- 1 + as.numeric(names(jdat$value))
        replace(values, inds, vals)
      }
    )
  
  # ---- Harmonise time field -------------------------------------------------
  if ("time" %in% names(downloaded)) {
    downloaded <- downloaded %>%
      dplyr::rename(TIME_PERIOD = time) %>%
      dplyr::mutate(TIME_PERIOD = as.Date(
        paste0(TIME_PERIOD, "-01-01")
      ))
  }

  attr(downloaded, "dataset") <- id

  downloaded <- tibble::rowid_to_column(downloaded)
  downloaded$year <- lubridate::year(downloaded$TIME_PERIOD)
  invisible(gc())
  downloaded
}
