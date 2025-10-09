#' Eurostat Product Supply, Adjustments and Value Added Vocabulary (`prd_ava`)
#'
#' A reference codelist used in Eurostat’s national accounts framework for
#' **product × product** symmetric input–output tables (SIOTs).
#' This vocabulary enumerates the **rows** of product-based tables, covering
#' intermediate products (Quadrant 1), value-added components (Quadrant 3), and
#' selected control totals such as *“Total supply at basic prices”*.
#'
#' @details The `prd_ava` vocabulary aligns with the official Eurostat SDMX
#'   codelist
#' *“Products, adjustments and value added”*.
#'   It is used by the [iotables] package to order the **row dimension** of
#'   product-by-product tables (datasets such as `naio_10_cp1700`).
#'
#'   Codes correspond to **CPA 2.1** product classifications and ESA 2010
#'   accounting aggregates. Each entry includes a quadrant identifier and an
#'   ordinal numeric order for consistent placement when reshaping long-form
#'   data.
#'
#'   The dataset retains published control totals and balancing items for
#'   completeness (e.g., *Total supply at basic prices* or *CIF/FOB
#'   adjustments*), but these rows are normally excluded from analytical
#'   matrices.
#'
#'   Because the Eurostat SIOT datasets sometimes contain more detailed industry
#'   breakup than their vocabulary, we adopted industry codeing from  [cpa2_1].
#'   These codes are marked the "Valid in CPA2_1" status.
#'
#' @format A tibble (data frame) with 10 variables:
#' \describe{
#'   \item{id}{Canonical Eurostat concept identifier.}
#'   \item{label}{Official Eurostat label (preferred English term).}
#'   \item{status}{"Valid" or "Valid in CPA2_1".}
#'   \item{status_modified}{Date of last status modification.}
#'   \item{notation}{Alternate human-readable code or short form (e.g. `CPA_A01`).}
#'   \item{ind_ava_notation}{Legacy comparison column retained for internal cross-checks.}
#'   \item{quadrant}{Integer designating the analytical block:
#'     `10` = Intermediate (technology block, Quadrant 1);
#'     `20` = Primary inputs / GVA (Quadrant 3);
#'     `30` = Control totals;
#'     `50` = Extensions / adjustments.}
#'   \item{numeric_order}{Ordinal position key for ordering within quadrants.}
#'   \item{iotables_label}{Simplified legacy label used in the [iotables] package.}
#'   \item{block}{Semantic block category inferred from `quadrant`
#'     (`intermediate`, `primary_inputs`, `control_total`, `extension`).}
#'   \item{uri}{Stable URI linking to the Eurostat SKOS concept at
#'     <https://dd.eionet.europa.eu/>.}
#' }
#'
#' @source Eurostat metadata registry
#'   (<https://dd.eionet.europa.eu/vocabulary/eurostat/prd_ava/>)
#'
#' @seealso Related Eurostat vocabularies: [ind_ava], [ind_use], [prd_use],
#'   [cpa2_1].
#'
#' @keywords datasets metadata Eurostat input-output SUT SIOT
#'
#' @examples
#' data(prd_ava)
"prd_ava"
