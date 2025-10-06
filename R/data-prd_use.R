#' Eurostat Product Uses Vocabulary (`prd_use`)
#'
#' A reference codelist used in Eurostat’s national accounts framework for
#' **product × product** symmetric input–output tables (SIOTs).
#' This vocabulary enumerates the **columns** of product-based tables, covering
#' intermediate product uses (Quadrant 1), value-added components (Quadrant 3),
#' and the final-use block (Quadrant 2), as well as balancing or extension items.
#'
#' @details
#' The `prd_use` vocabulary corresponds to the official Eurostat SDMX codelist
#' *“Product uses”*.
#' It is used by the [iotables] package to order the **column dimension**
#' of product-by-product tables (datasets such as `naio_10_cp1700`).
#'
#' Codes follow **CPA 2.1** for products and **ESA 2010** for accounting
#' aggregates.
#' Each entry includes a quadrant identifier and a numeric order key that ensures
#' consistent column ordering when reshaping Eurostat’s long-form data into
#' analytical matrices.
#'
#' The dataset also retains control totals and adjustment items (Quadrant 30/50)
#' such as *Total use at purchasers’ prices*, *CIF/FOB adjustments*, or
#' *Discrepancy*, which are part of Eurostat’s published codelist but are
#' normally excluded from matrix algebra operations.
#'
#' @format A tibble (data frame) with 11 variables:
#' \describe{
#'   \item{id}{Canonical Eurostat concept identifier.}
#'   \item{label}{Official Eurostat label (preferred English term).}
#'   \item{status}{Concept status flag (usually `"A"` for active).}
#'   \item{status_modified}{Date of last status modification.}
#'   \item{notation}{Alternate human-readable code or short form (e.g. `CPA_A01`).}
#'   \item{group}{Optional grouping of related products or aggregates.}
#'   \item{quadrant}{Integer designating the analytical block:
#'     `10` = Intermediate (technology block, Q1);
#'     `20` = Primary inputs / GVA (Q3);
#'     `30` = Final use (Q2);
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
#' (<https://dd.eionet.europa.eu/vocabulary/eurostat/prd_use/>)
#'
#' @seealso
#'   Related Eurostat vocabularies: [prd_ava], [ind_ava], [ind_use].
#'
#' @keywords datasets metadata Eurostat input-output SUT SIOT
#'
#' @examples
#' data(prd_use)
"prd_use"
