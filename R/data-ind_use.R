#' Eurostat Industry Uses Vocabulary (`ind_use`)
#'
#' A reference codelist used in Eurostat’s national accounts framework for
#' **industry × industry** (NACE-based) symmetric input–output tables (SIOTs).
#' This vocabulary enumerates the **columns** of the table, covering the
#' intermediate-use block (Quadrant 1), the value-added block (Quadrant 3), and
#' the final-use and balancing items (Quadrant 2/50).
#'
#' @details The `ind_use` vocabulary corresponds to the official Eurostat SDMX
#' codelist
#' *“Industry uses”*.
#' It is used by the [iotables] package to order the **column dimension** of
#' industry-by-industry tables (datasets such as `naio_10_cp1750`).
#'
#' Codes follow **NACE Rev. 2** for industries and **ESA 2010** for accounting
#' aggregates. Each entry includes its quadrant classification and an ordinal
#' numeric sequence for reproducible column ordering when reshaping long-form
#' data.
#'
#' The vocabulary also includes recent Eurostat additions introduced in 2023
#' (*ITTM* – International trade and transport margins, *MCH* – Merchanting, and
#' *OP* – Purchases by residents abroad). These appear in the final-use block
#' (`quadrant = 30`) and are retained for completeness and balancing, though
#' they are not part of the analytical technology matrix.
#'
#' Because the Eurostat SIOT datasets sometimes contain more detailed industry
#' breakup than their vocabulary, we adopted industry codeing from  [cpa2_1].
#' These codes are marked the "Adopted from CPA2_1" status.
#'
#' @format A tibble (data frame) with 10 variables:
#' \describe{
#'   \item{id}{Canonical Eurostat concept identifier.}
#'   \item{label}{Official Eurostat label (preferred English term).}
#'   \item{status}{"Valid" or "Adopted from CPA2_1".}
#'   \item{status_modified}{Date of last status modification.}
#'   \item{notation}{Alternate human-readable code or short form (e.g. `CPA_A01`).}
#'   \item{quadrant}{Integer designating the analytical block:
#'     `10` = Intermediate (technology block, Q1);
#'     `20` = Primary inputs / GVA (Q3);
#'     `30` = Final use / control totals (Q2);
#'     `50` = Extensions / diagnostics.}
#'   \item{numeric_order}{Ordinal key for ordering columns within quadrants.}
#'   \item{iotables_label}{Simplified legacy label used in the [iotables] package.}
#'   \item{block}{Semantic block category inferred from `quadrant`
#'     (`intermediate`, `primary_inputs`, `final_use`, `extension`).}
#'   \item{uri}{Stable URI linking to the Eurostat SKOS concept at
#'     <https://dd.eionet.europa.eu/>.}
#' }
#'
#' @source Eurostat metadata registry
#'   (<https://dd.eionet.europa.eu/vocabulary/eurostat/ind_use/>)
#'
#' @seealso Related Eurostat vocabularies: [ind_ava], [prd_ava], [prd_use],
#' [cpa2_1].
#'
#' @keywords datasets metadata Eurostat input-output SUT SIOT
#'
#' @examples
#' data(ind_use)
#' dplyr::count(ind_use, quadrant)
"ind_use"
