#' @title UK multipliers and effects (product), 2010
#'
#' @description
#' Published multipliers and effects from the United Kingdom
#' Input–Output Analytical Tables, reference year 2010.
#'
#' This dataset contains output, employment cost, and GVA multipliers
#' and effects, together with their published rankings. It is imported
#' from the official ONS Excel release and normalized for use in
#' \pkg{iotables}. It is primarily used in the
#' \code{vignette("united_kingdom_2010", package = "iotables")} to
#' validate the package’s multiplier functions against official UK
#' results.
#'
#' @details
#' The Office for National Statistics (ONS) publishes Input–Output
#' Analytical Tables (IOATs) for the UK. From these, Type I and Type II
#' multipliers and effects are calculated. This dataset contains those
#' published values at the product level for 2010, enabling direct
#' cross-checks with \pkg{iotables} computations.
#'
#' @format A tibble with 127 rows and 12 variables:
#' \describe{
#'   \item{uk_row_label}{Product/industry label.}
#'   \item{output_multiplier}{Output multiplier (published).}
#'   \item{output_multiplier_rank}{Ranking of output multipliers.}
#'   \item{employment_cost_multiplier}{Employment cost multiplier.}
#'   \item{employment_cost_multiplier_rank}{Ranking of employment cost multipliers.}
#'   \item{employment_cost_effects}{Employment cost effects.}
#'   \item{employment_cost_effects_rank}{Ranking of employment cost effects.}
#'   \item{gva_multiplier}{GVA multiplier.}
#'   \item{gva_multiplier_rank}{Ranking of GVA multipliers.}
#'   \item{gva_effects}{GVA effects.}
#'   \item{gva_effects_rank}{Ranking of GVA effects.}
#'   \item{indicator}{Indicator label, usually
#'     "Multipliers and effects (product)".}
#' }
#'
#' @source
#' Office for National Statistics (ONS), UK Input–Output Analytical
#' Tables 2010 (Excel release).
#'
#' @seealso
#' \code{vignette("united_kingdom_2010", package = "iotables")}
#'
#' @family validation datasets
"uk_test_results"

