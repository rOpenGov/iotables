#' @title Simple input-output table for the Netherlands, 2006
#'
#' @description
#' Simplified symmetric input-output table (SIOT) from the *Science Policy
#' Integration for Coastal Systems Assessment* project's input-output multiplier
#' specification sheet.
#'
#' This dataset provides a minimal SIOT for controlled analytical results.
#' Column names were slightly adjusted to resemble Eurostat conventions and to
#' align with the main example dataset [germany_1995].
#'
#' @format A data frame with 14 observations and 13 variables:
#' \describe{
#'   \item{prod_na}{Product names, simplified, following Eurostat conventions.}
#'   \item{agriculture_group}{Aggregated agricultural products.}
#'   \item{mining_group}{Aggregated mining products.}
#'   \item{manufacturing_group}{Aggregated manufacturing products.}
#'   \item{construction_group}{Construction.}
#'   \item{utilities_group}{Aggregated utilities products/services.}
#'   \item{services_group}{Aggregated services products.}
#'   \item{TOTAL}{Row/column sums; a simple summary not present in the original
#'     source.}
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
#' [Input-Output Multipliers: Specification sheet and supporting material](
#' https://hal.science/hal-03233439), Spicosa Project Report.
#'
#' @family Validation datasets
"netherlands_2006"
