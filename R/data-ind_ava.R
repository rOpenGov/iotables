#' Eurostat Industry Supply, Adjustments and Value Added Vocabulary (`ind_ava`)
#'
#' A reference codelist used in Eurostat’s national accounts framework for
#' the **industry × industry** (NACE-based) symmetric input–output tables (SIOTs).
#' This vocabulary enumerates the **rows** of the table, covering both the
#' intermediate (Quadrant 1) and value-added (Quadrant 3) blocks, and includes
#' the control total *“Total supply at basic prices”* published by Eurostat.
#'
#' @details
#' The `ind_ava` vocabulary aligns with the official Eurostat SDMX codelist
#' *“Industries, adjustments and value added”* and is used by the
#' [iotables] package to order the rows of industry-by-industry tables
#' (datasets such as `naio_10_cp1750`).
#'
#' Codes correspond to NACE Rev. 2 divisions and ESA 2010 accounting aggregates.
#' Each entry includes its quadrant classification and an ordinal numeric order
#' to preserve correct row sequencing when reshaping long-form data.
#'
#' The dataset retains the published control total (“Total supply at basic prices”)
#' for consistency with Eurostat’s metadata, but this row is normally excluded
#' from analytical matrix operations.
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
#'   \item{status_modified}{Date of the most recent status update.}
#'   \item{notation}{Alternate human-readable code or short form (e.g. `CPA_A01`).}
#'   \item{group}{Optional grouping of related industries or aggregates.}
#'   \item{quadrant}{Integer designating the analytical block:
#'     `10` = Intermediate (technology block, Quadrant 1);
#'     `20` = Value added (primary inputs, Quadrant 3);
#'     `30` = Control total(s).}
#'   \item{numeric_order}{Ordinal position key for sorting within quadrant.}
#'   \item{iotables_label}{Simplified legacy label used in the [iotables] package.}
#'   \item{block}{Semantic block category inferred from `quadrant`
#'     (`intermediate`, `primary_inputs`, `control_total`, `extension`).}
#'   \item{uri}{Stable URI linking to the Eurostat SKOS concept at
#'     <https://dd.eionet.europa.eu/>.}
#' }
#'
#' @source Eurostat metadata registry (DD EIONET vocabulary:
#' <https://dd.eionet.europa.eu/vocabulary/eurostat/ind_ava/>)
#'
#' @seealso
#'   Other Eurostat vocabularies included in the package:
#'   [ind_use], [prd_use], [prd_ava], [cpa2_1].
#'
#' @keywords datasets metadata Eurostat input-output SUT SIOT
#'
#' @examples
#' data(ind_ava)
"ind_ava"
