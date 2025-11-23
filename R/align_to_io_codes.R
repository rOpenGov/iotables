#' Align External Industry-Based Data to an Input–Output Table
#'
#' @description
#' `align_to_io_codes()` harmonises an external dataset (such as air
#' pollution data, energy use, employment, or satellite accounts)
#' to the industry structure of an input–output (IO) table.
#'
#' The function standardises industry identifiers, drops irrelevant
#' aggregate categories, pads missing industries with zeros, and
#' reorders columns so that identifiers appear first followed by IO
#' industry codes in the correct order.
#'
#' This enables consistent integration of satellite accounts with IO
#' tables for downstream operations such as indicator creation,
#' coefficient matrix construction, or Leontief multiplier analysis.
#'
#' @param ext_data A data frame or tibble containing external data
#'   whose column names correspond to NACE or NACE-like industry
#'   codes. Must include at least one identifier column, such as
#'   `"indicator"`, `"multiplier"`, `"prod_na"`, `"industry"`,
#'   `"sector"`, or another variable name ending in `"_id"` or
#'   `"_code"`.
#'
#' @param io_data A data frame or tibble representing the structured
#'   IO table whose column names define the target industry codes.
#'   These will be used to reorder and pad the external dataset.
#'
#' @return A tibble with:
#'   \itemize{
#'     \item the detected identifier columns first
#'     \item followed by all industry codes from `io_data` in the same order
#'   }
#'   Industry columns missing from `ext_data` are added and filled with
#'   zeros.
#'
#' @details
#' The function performs six harmonisation steps:
#'
#' \enumerate{
#'   \item Detect identifier columns using a combination of known names
#'     (`"indicator"`, `"multiplier"`, `"prod_na"`, etc.) and regular
#'     expressions matching `"_id$"`, `"_code$"`, or `"indicator"`.
#'
#'   \item Remove high-level aggregates (e.g., `"TOTAL"`, `"HH"`,
#'     `"TOTAL_HH"`) that do not correspond to IO industries.
#'
#'   \item Harmonise naming conventions including common underscore/
#'     hyphen inconsistencies (e.g., `"J62-J63"` → `"J62_63"`).
#'
#'   \item Add missing industry columns that exist in `io_data` but not
#'     in `ext_data`. These are initialised to zero.
#'
#'   \item Reorder the resulting dataset so that identifier columns
#'     appear first, followed by industry columns in IO order.
#'
#'   \item Return the cleaned tibble ready for combination with the IO
#'     table (e.g., via `supplementary_add()`).
#' }
#'
#' @section Typical use cases:
#' \itemize{
#'   \item Align NAMEA air emission accounts to IO industries
#'   \item Align energy-use accounts, employment, material flows
#'   \item Prepare satellite accounts for Leontief multiplier analysis
#'   \item Harmonise any NACE-like external dataset to IO structure
#' }
#'
#' @seealso
#'   \code{\link{iotable_get}},
#'   \code{\link{supplementary_add}},
#'   \code{\link{input_indicator_create}},
#'   \code{\link{leontief_inverse_create}}
#'
#' @examples
#' # Minimal fictitious IO table with three industries
#' io_example <- data.frame(
#'   prod_na = c("A", "B", "C31_32"),
#'   A       = c(10,  2,  1),
#'   B       = c( 1, 15,  3),
#'   C31_32  = c( 0,  1, 20),
#'   check.names = FALSE
#' )
#'
#' # External satellite account:
#' # * includes an identifier column
#' # * matches industries A and B
#' # * uses a hyphenated code C31-32 that needs harmonisation
#' # * includes TOTAL which should be dropped
#' ext_example <- data.frame(
#'   indicator = "GHG_emission",
#'   A         = 0.5,
#'   B         = 0.2,
#'   `C31-32`  = 0.3,
#'   TOTAL     = 0.7,
#'   check.names = FALSE
#' )
#'
#' # Align external data to the IO industry codes
#' aligned <- align_to_io_codes(ext_example, io_example)
#'
#' # The aligned table contains:
#' # * a single identifier column named 'prod_na'
#' # * only IO industries (A, B, C31_32)
#' # * renamed and reordered industry columns
#' aligned
#' @importFrom dplyr recode rename_with
#' @export

align_to_io_codes <- function(ext_data, io_data) {
  # --- 1. Identify external and IO structure ---------------------------
  io_codes <- names(io_data)
  ext_codes <- names(ext_data)

  # --- 2. Identify identifier columns dynamically ----------------------
  # Default internal names in iotables outputs
  hardcoded_ids <- c(
    "indicator", "multiplier",
    "prod_na", "ind_use", "t_col", "t_row",
    "industry", "sector"
  )

  # Regex-based fallback for robustness
  regex_ids <- grepl("indicator|multiplier|_id$|_code$|name$",
    ext_codes,
    ignore.case = TRUE
  )

  id_cols <- unique(c(
    intersect(hardcoded_ids, ext_codes),
    ext_codes[regex_ids]
  ))

  # Make sure identifiers come first later
  if (length(id_cols) == 0) {
    warning("No identifier columns detected in external dataset.")
  }

  # --- 3. Drop meaningless aggregates -------------------------------------
  drop_cols <- intersect(ext_codes, c("TOTAL", "TOTAL_HH", "HH", "ALL"))
  ext_data2 <- ext_data[, setdiff(ext_codes, drop_cols)]

  # --- 4. Harmonise naming conventions (underscore/hyphen etc.) ----------
  # key=ext_data coding, values=IOT name convension
  rename_map <- c(
    "C10-C12" = "C10-12",
    "C13-C15" = "C13-15",
    "C31-32"  = "C31_32",
    "E37-E39" = "E37-39",
    "J62-J63" = "J62_63"
  )

  ext_data2 <- ext_data2 %>%
    dplyr::rename_with(~ dplyr::recode(.x, !!!rename_map))

  # --- 5. Pad missing IO sectors with zeros ------------------------------
  missing_cols <- setdiff(io_codes, names(ext_data2))
  for (cc in missing_cols) {
    ext_data2[[cc]] <- 0
  }

  # --- 6. Choose one canonical identifier column ------------------------

  # If 'indicator' exists, this is the row label
  if ("indicator" %in% names(ext_data2)) {
    ext_data2$prod_na <- ext_data2$indicator
    ext_data2$indicator <- NULL
  }

  # If 'prod_na' exists in external data (rare), keep it
  # If neither exists, error out
  if (!"prod_na" %in% names(ext_data2)) {
    stop("No valid identifier column ('indicator' or 'prod_na') found.")
  }

  # Remove other identifier-like columns (industry, sector, etc.)
  id_drop <- setdiff(id_cols, "prod_na")
  ext_data2 <- ext_data2[, setdiff(names(ext_data2), id_drop)]

  # --- 7. Reorder columns: identifiers first, IO codes next --------------
  final_cols <- c(
    intersect(id_cols, names(ext_data2)),
    io_codes
  )

  ext_data2 <- ext_data2[, final_cols]

  ext_data2
}
