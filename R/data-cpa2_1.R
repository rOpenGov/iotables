#' Eurostat Product Classification (CPA 2.1 — Summary and Aggregate Levels)
#'
#' A reference vocabulary derived from Eurostat’s **CPA 2.1 — Statistical Classification
#' of Products by Activity**, restricted to high-level (summary) items, extensions, and
#' 0-, 1-, and 2-digit product aggregates.  
#'
#' This vocabulary provides the **product-side** codes used in Eurostat’s
#' **supply–use** and **input–output** frameworks, consistent with NACE Rev. 2 activity
#' groupings. It is used by the [iotables] package to order and label the **product × product**
#' blocks of symmetric input–output tables (SIOTs) and their supply–use counterparts.
#'
#' @details
#' The `cpa2_1` vocabulary was obtained from the official Eurostat SKOS registry  
#' (<https://dd.eionet.europa.eu/vocabulary/eurostat/cpa2_1/>, filter: `conceptStatusInt = 128`,  
#' i.e. active concepts).  
#'
#' From the full CPA 2.1 hierarchy, this dataset retains:
#'
#' * **0-digit** sections (e.g. `"CPA_L"`, `"CPA_O"`),  
#' * **1-digit** aggregates,  
#' * **2-digit** divisions, and  
#' * published **summary** and **extension** items.  
#'
#' These levels correspond to the structure of Eurostat’s aggregated supply–use tables and
#' are suitable for analytical applications such as cultural, tourism, or environmental
#' satellite accounts.  
#'
#' All entries are marked with status `"valid"`, reflecting their current publication state
#' in the Eurostat metadata repository.
#'
#' @format A tibble (data frame) with 10 variables:
#' \describe{
#'   \item{id}{Canonical Eurostat concept identifier (SKOS URI fragment).}
#'   \item{label}{Official English label from the Eurostat CPA 2.1 vocabulary.}
#'   \item{status}{Concept validity flag (`"valid"` for all entries).}
#'   \item{status_modified}{Date of the most recent status update.}
#'   \item{notation}{Official CPA 2.1 notation (e.g. `"CPA_A01"`, `"CPA_C10"`).}
#'   \item{quadrant}{Integer flag used internally in \pkg{iotables} to identify the analytical
#'     block (`10` = intermediate, `20` = value added, `30` = control totals).}
#'   \item{numeric_order}{Ordinal position key ensuring correct row sequencing in
#'     reshaped input–output matrices.}
#'   \item{iotables_label}{Simplified legacy label used internally by the [iotables] package.}
#'   \item{block}{Semantic block category inferred from `quadrant`
#'     (`intermediate`, `primary_inputs`, `control_total`, `extension`).}
#'   \item{uri}{Stable URI for the SKOS concept at
#'     <https://dd.eionet.europa.eu/vocabulary/eurostat/cpa2_1/>.}
#' }
#'
#' @source
#' Eurostat metadata registry (DD EIONET vocabulary:
#' <https://dd.eionet.europa.eu/vocabulary/eurostat/cpa2_1/>)
#'
#' @seealso
#' Other Eurostat vocabularies included in the package:
#' [ind_use], [ind_ava], [prd_use], [prd_ava].
#'
#' @keywords datasets metadata Eurostat CPA products SUT SIOT
#'
#' @examples
#' data(cpa2_1)
"cpa2_1"
