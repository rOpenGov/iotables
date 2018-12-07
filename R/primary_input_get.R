#' Get primary inputs
#'
#' This function will retrieve any primary input from the input-output table. 
#' @param siot A symmetric input-output table created by 
#' \code{\link{iotable_get}}. 
#' @importFrom dplyr select mutate_if
#' @examples
#' comp_employees_de <- primary_input_get(
#'                             siot = iotable_get( "germany_1990"), 
#'                             primary_input = "compensation_employees")
#'                             )
#' @export

primary_input_get <- function ( siot,
                                primary_input = "compensation_employees") {
  
  if ( is.null(siot)) { 
    stop ( "No input-output table was given as an input")
    }
  
  last_column <- quadrant_separator_find ( siot )
  
  siot <- siot %>% 
    dplyr::mutate_if ( is.factor, as.character ) %>%
    dplyr::select(1:last_column)
  
  if ( primary_input %in% siot[[1]] ) {
    input_row <- which ( siot[[1]] == primary_input )
  } else {
    stop("The input is not found in this data source.")
  }

    siot[input_row, ]
}


