#' @keywords internal
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
      names(filters),
      "=",
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

  # ---- Authoritative CPA filter ---------------------------------------------
  valid_cpa <- unique(c(
    names(dims$prd_ava$category$label),
    names(dims$prd_use$category$label)
  ))

  valid_cpa_siot <- c(
    "CPA_TOTAL",
    "CPA_A01", "CPA_A02", "CPA_A03", "CPA_B",
    "CPA_C10-12", "CPA_C13-15", "CPA_C16", "CPA_C17", "CPA_C18", "CPA_C19",
    "CPA_C20", "CPA_C21", "CPA_C22", "CPA_C23", "CPA_C24", "CPA_C25", "CPA_C26",
    "CPA_C27", "CPA_C28", "CPA_C29", "CPA_C30", "CPA_C31_32", "CPA_C33",
    "CPA_D", "CPA_E36", "CPA_E37-39", "CPA_F", "CPA_G45", "CPA_G46", "CPA_G47",
    "CPA_H49", "CPA_H50", "CPA_H51", "CPA_H52", "CPA_H53",
    "CPA_I55", "CPA_I56", "CPA_J58", "CPA_J59_60", "CPA_J61", "CPA_J62_63",
    "CPA_K64", "CPA_K65", "CPA_K66", "CPA_L68A", "CPA_L68B",
    "CPA_M69_70", "CPA_M71", "CPA_M72", "CPA_M73", "CPA_M74_75",
    "CPA_N77", "CPA_N78", "CPA_N79", "CPA_N80-82",
    "CPA_O", "CPA_P", "CPA_Q86", "CPA_Q87_88", "CPA_R90-92", "CPA_R93",
    "CPA_S94", "CPA_S95", "CPA_S96", "CPA_T", "CPA_U",
    # balancing and adjustment items
    "CPA_B1G", "CPA_B2A3N", "CPA_B2A3G", "CPA_B3G", "CPA_D1", "CPA_D11",
    "CPA_D21X31", "CPA_D29X39", "CPA_P1", "CPA_P2_ADJ", "CPA_P7",
    "CPA_P51C", "CPA_IMP", "CPA_TS_BP"
  )


  downloaded <- downloaded %>%
    dplyr::filter(
      (!"prd_ava" %in% names(.) | prd_ava %in% valid_cpa_siot),
      (!"prd_use" %in% names(.) | prd_use %in% valid_cpa_siot)
    )


  # ---- Harmonise with eurostat::get_eurostat() output -----------------------
  if ("time" %in% names(downloaded)) {
    downloaded <- downloaded %>%
      dplyr::rename(TIME_PERIOD = time) %>%
      dplyr::mutate(
        TIME_PERIOD = as.Date(paste0(TIME_PERIOD, "-01-01"))
      )
  }

  # ---- Finalise -------------------------------------------------------------
  attr(downloaded, "dataset") <- id
  rm(jdat, dims, dims_list, ids)
  invisible(gc())

  downloaded
}
