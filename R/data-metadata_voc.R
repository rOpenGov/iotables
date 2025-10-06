#' Canonical Input–Output Vocabulary Metadata (SDMX-aligned)
#'
#' @description
#' The `metadata_voc` dataset provides the **current canonical vocabulary**
#' used by the **iotables** package to order and label input–output (IO) and
#' supply–use (SUT) tables retrieved from Eurostat and other statistical
#' sources.
#'
#' It replaces the legacy [`metadata`] object and reflects the SDMX-aligned
#' vocabulary updates introduced by Eurostat in 2023–2025. The dataset merges
#' historical ESA 2010 code lists with the modern `prd_use`, `prd_ava`,
#' `ind_use`, and `ind_ava` vocabularies, ensuring that analyses remain
#' reproducible across versions of the Eurostat warehouse and compatible with
#' national extensions (e.g. Czech SIOTs) and OECD harmonised SUT structures.
#'
#' This dataset is stored internally and is automatically accessed by
#' [`iotables_download()`], [`iotable_get_eurostat()`], and related functions.
#' Future updates (v5, v6, …) will refresh its contents without changing its
#' name, so analysis code remains stable.
#'
#' @format A data frame with the following key variables:
#' \describe{
#'   \item{variable}{Vocabulary family name (e.g. `induse`, `prod_na`, `ind_ava`,
#'     `ind_use`, `prd_ava`, `prd_use`, `primary_inputs`, `final_use`).}
#'   \item{code}{Canonical SDMX/ESA/CPA/NACE token identifying each element.}
#'   \item{dimension}{Table axis: `row` or `col`.}
#'   \item{quadrant}{Numeric quadrant ID:
#'     `10` = intermediate (Z, Quadrant 1),
#'     `30` = final use (Y, Quadrant 2),
#'     `20` = value added (GVA, Quadrant 3).}
#'   \item{block}{Functional block name (`intermediate`, `final_use`,
#'     `primary_inputs`, or `extension`).}
#'   \item{position}{Ordinal position for ordering. Gaps (10, 20, 30, …) allow
#'     new national items to be inserted without renumbering.}
#'   \item{label}{Preferred (current) Eurostat label.}
#'   \item{iotables_label}{Legacy-friendly display label used in examples and
#'     plotting.}
#'   \item{provenance}{Source of the record (e.g. `"Eurostat 2025"`,
#'     `"CZ SIOT 2020"`).}
#'   \item{safe_to_use}{Logical flag: `TRUE` = verified and safe for analytics;
#'     `FALSE` = requires review.}
#'   \item{note}{Free-text methodological comment.}
#'   \item{source}{Origin of the record (Eurostat, OECD, national, etc.).}
#'   \item{alt_label_1 … alt_label_4}{Alternative or historical wording of
#'     the label.}
#'   \item{alt_prov_1 … alt_prov_4}{Provenance for each corresponding
#'     alternative label.}
#' }
#'
#' @details
#' `metadata_voc` enforces **code-based identity** following SDMX principles:
#' two entries are considered identical if and only if their `code` values
#' match. Labels and provenance fields may evolve, but code identity ensures
#' that analytics remain semantically consistent.  
#'
#' Key design features:
#' \itemize{
#'   \item Gapped positional numbering for version-stable joins and easy
#'     insertion of national extensions.
#'   \item Provenance tracking (`provenance`, `safe_to_use`) for transparency.
#'   \item Support for up to four alternative labels with documented origins
#'     to prevent semantic drift.
#'   \item Seamless compatibility with Eurostat, OECD, and national SIOT/SUT
#'     vocabularies.
#' }
#'
#' @source
#' Compiled from:
#' \itemize{
#'   \item Eurostat ESA 2010 and SDMX vocabularies (2014 – 2025)
#'   \item Czech Statistical Office SIOT (2015, 2020)
#'   \item OECD SUT/SIOT harmonised structure
#'   \item iotables legacy metadata mappings (2018 – 2024)
#' }
#'
#' @seealso
#' [metadata] for the legacy ESA 2010 vocabulary,
#' [iotables_download()],
#' [iotable_get_eurostat()],
#' and the vignette *"Metadata and Vocabulary Management"*.
#'
#' @family metadata datasets
#' @keywords datasets metadata SDMX Eurostat reproducibility
#'
#' @examples
#' # Preview structure of the canonical vocabulary
#' head(metadata_voc)
#'
#' # Filter for Eurostat 2025 product-use items
#' subset(metadata_voc, 
#'   variable == "prd_use" &
#'    provenance == "Eurostat 2025")[1:6, c("code", "label")]
#'
"metadata_voc"
