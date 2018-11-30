#' Simple input-output table for the Netherlands, 2006.
#'
#' This simplified SIOT is taken from the  Science Policy Integration for Coastal
#' Systems Assessment project's input-output multiplier specification sheet.
#' It is used as a simple example SIOT for controlled analytical results.
#' The column names were slightly altered to resemble more the current Eurostat
#' conventions and the main example dataset \code{\link{germany_1990}}.
#' @format A data frame with 14 observations and 13 variables.
#' @usage data(netherlands_2006)
#' @source Source: \href{http://www.coastal-saf.eu/output-step/pdf/Specification sheet I_O_final.pdf}{Input-Output Multipliers Specification Sheet and Supporting Material, Spicosa Project Report}
#'\describe{
#'   \item{prod_na}{Product name, simplified, following the Eurostat conventions}
#'   \item{agriculture_group}{Simple aggregated agricultural products}
#'   \item{mining_group}{Simple aggregated mining products}
#'   \item{manufacturing_group}{Simple aggregated manufacturing products}
#'   \item{construction_group}{Construction}  
#'   \item{services_group}{Simple aggregated services products}
#'   \item{TOTAL}{Column / row sums, simple summary, not included in the original source}
#'   \item{final_consumption_private}{Simple aggregated final private use}
#'   \item{final_consumption_households}{Simple aggregated final household consumption}
#'   \item{final_consumption_government}{Simple aggregated final government consumption}    
#'   \item{gross_fixed_capital_formation}{Gross fixed capital formation 'GFCF'}   
#'   \item{exports}{Simple aggregated exports}
#'   \item{total_use}{Simple aggregated total use}              
#' }
#' @keywords data, datasets, input-output table, Netherlands
"netherlands_2006"

