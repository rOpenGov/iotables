#' @title Simplified input–output table for the Netherlands, 2000 (Spicosa example)
#'
#' @description
#' Aggregated symmetric input–output table (SIOT) for the Netherlands,
#' reference year 2000, reproduced from the *Science Policy Integration
#' for Coastal Systems Assessment* (Spicosa) project’s multiplier
#' specification sheet (D’Hernoncourt, Cordier & Hadley, 2011).
#'
#' This dataset was originally created in the Spicosa project (circa 2006)
#' as a simplified teaching table, based on OECD/Eurostat SIOT data.
#' Column and row names were slightly adjusted to resemble Eurostat
#' conventions and to align with the main example dataset [germany_1995].
#'
#' @details
#' The Spicosa specification sheet demonstrates the derivation of type I
#' and type II multipliers step by step from this table. This dataset
#' corresponds to Table 1 of that report, the domestic transactions
#' input–output table (million EUR, year 2000). It is not an official
#' Statistics Netherlands SIOT, but a simplified, aggregated example for
#' multiplier analysis.
#'
#' @format A data frame with 14 observations and 13 variables:
#' \describe{
#'   \item{prod_na}{Simplified product/industry names.}
#'   \item{agriculture_group}{Aggregated agricultural products.}
#'   \item{mining_group}{Aggregated mining products.}
#'   \item{manufacturing_group}{Aggregated manufacturing products.}
#'   \item{construction_group}{Construction.}
#'   \item{utilities_group}{Aggregated utilities products/services.}
#'   \item{services_group}{Aggregated services products.}
#'   \item{TOTAL}{Row/column sums; a simple summary not present in the
#'     original source.}
#'   \item{final_consumption_private}{Aggregated final private consumption.}
#'   \item{final_consumption_households}{Aggregated final household
#'     consumption.}
#'   \item{final_consumption_government}{Aggregated final government
#'     consumption.}
#'   \item{gross_fixed_capital_formation}{Gross fixed capital formation (GFCF).}
#'   \item{exports}{Aggregated exports.}
#'   \item{total_use}{Aggregated total use.}
#' }
#'
#' @source
#' D’Hernoncourt, J., Cordier, M. & Hadley, D. (2011).
#' *Input–Output Multipliers: Specification sheet and supporting material*.
#' Spicosa Project Report. <https://hal.science/hal-03233439>
#'
#' @family validation datasets
"netherlands_2000"
