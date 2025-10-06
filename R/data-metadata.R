#' Eurostat National Accounts Vocabulary Metadata (legacy)
#'
#' @description
#' The `metadata` dataset provides the **legacy** Eurostat national accounts
#' vocabulary used in earlier versions of the **iotables** package to correctly
#' order wide-format rows and columns when reshaping bulk long-form Eurostat
#' input–output (IO) and supply–use (SUT) tables.
#'
#' It predates the modern SDMX-aligned metadata system introduced in 2025 and
#' has been retained for backward compatibility and reproducibility of analyses
#' built on earlier versions of the Eurostat warehouse (ESA 2010, 2014–2022).
#'
#' The dataset encodes the structure of the pre-2023 vocabularies (`prod_na`,
#' `induse`, `t_rows`, `t_cols`) used by the legacy `iotable_get()` and
#' `iotable_get_builtin()` functions. It provides the column and row ordering
#' necessary for consistent matrix construction.
#'
#' @format A data frame with 8 variables:
#' \describe{
#'   \item{variable}{Vocabulary source identifier (e.g. `t_rows`, `t_cols`,
#'     `prod_na`, `induse`).}
#'   \item{group}{Informal macroeconomic grouping or block label.}
#'   \item{code}{Eurostat SDMX code used as identity.}
#'   \item{label}{Official Eurostat description of the code.}
#'   \item{quadrant}{Numeric indicator of table placement:
#'     `10` = intermediate (Quadrant 1),
#'     `20` = GVA/value added (Quadrant 3),
#'     `30` = final use (Quadrant 2).}
#'   \item{account_group}{Grouping of accounts (not part of the original Eurostat
#'     tables). Used for internal ordering and labelling.}
#'   \item{numeric_label}{Numeric key derived from `quadrant`, `account_group`,
#'     and digit-based components of `code`, used to sort rows and columns.}
#'   \item{iotables_label}{Legacy, machine-readable label in snake_case used
#'     internally for ordering and analysis.}
#' }
#'
#' @details
#' This dataset is maintained only for compatibility with legacy Eurostat
#' vocabularies (ESA 2010 format, pre-2023).
#' For modern workflows, use [`metadata_voc`] — the SDMX-aligned canonical
#' metadata vocabulary introduced in 2025.
#'
#' \strong{Transition note:}
#' The functions [`iotables_download()`] and [`iotable_get_eurostat()`] now use
#' `metadata_voc` automatically. The `metadata` object remains accessible for
#' reproducing results from earlier iotables versions (≤ 0.6.x) or from studies
#' relying on the `naio_10_cp16*` and `naio_10_cp17*` datasets prior to the
#' Eurostat vocabulary migration.
#'
#' @family metadata datasets
#' @seealso [metadata_voc], [iotable_get_eurostat()], [iotables_download()]
#' @keywords internal
#'
#' @examples
#' # Preview structure of legacy metadata
#' head(metadata)
#'
#' # Filter for old 'prod_na' items (legacy ESA 2010 vocabulary)
#' subset(metadata, variable == "prod_na")[1:6, c("code", "label")]
#'
"metadata"
