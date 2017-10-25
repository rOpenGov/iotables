#' Get the total output vector
#'
#' The only paramter is a use (input-flow) matrix. If it is a named object, it will remain named.
#' If it has earnings and household private demand, it will handle it. 
#' @param input_flow_matrix An IO data frame created with the get_io_data () function.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate rename
#' @examples
#'  data (germany_1990)
#'  input_flow_de <- input_flow_get(labelled_io_data = germany_1990, 
#'                geo = 'DE', year = 1990,
#'                unit = "M_EUR", named = TRUE)
#'  outlays <- outlay_total( input_flow_matrix = input_flow_de) 
#' @export

outlay_total <- function ( input_flow_matrix ) {
  if ( sapply(input_flow_matrix, class)[1] == "character") {
    outlays <- c(
      "outlays",
      colSums(input_flow_matrix[,2:ncol(input_flow_matrix)])
      )
    names(outlays)[1] <- "outlays"
  } else {
    outlays <- colSums(input_flow_matrix[,1:ncol(input_flow_matrix)])
  }
    return ( outlays )

} #end of function