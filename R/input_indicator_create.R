#' Create input indicator(s)
#' 
#' The function creates the input indicators from the inputs and the outputs.
#' @param data_table A symmetric input-output table, a use table, 
#' a margins or tax table retrieved by the  \code{\link{iotable_get}}
#' function. 
#' @param input_vector The name of inputs for which you want to create the 
#' indicators. They must be found in the \code{data_table}.
#' @param households If the households column should be added, 
#' defaults to \code{FALSE}.
#' @param digits Rounding digits, if omitted, no rounding takes place.
#' @param indicator_names The names of new indicators. Defaults to \code{NULL} when 
#' the names in the key colum of \code{input_matrix} will be used to create the 
#' indicator names.
#' @return A tibble (data frame) containing  \code{input_matrix} devided by the \code{output_vector}
#' with a key column for products or industries.
#' @importFrom dplyr mutate_if
#' @examples  
#' input_indicator_create( data_table = iotable_get(), 
#'                         input_vector = c("gva", "compensation_employees"),
#'                         digits = 4, 
#'                         indicator_names = c("GVA indicator", "Income indicator"))
##' @export

input_indicator_create <- function ( data_table,
                                     input_vector = c('gva_bp','net_tax_production'),
                                     digits = NULL,
                                     households = FALSE,
                                     indicator_names = NULL) { 
  
  data_table <- data_table %>% 
    dplyr::mutate_if ( is.factor, as.character ) 
  
  cm <- coefficient_matrix_create( data_table = data_table, 
                                   households = households )
  
  key_column <- tolower(as.character(unlist(cm[,1])))
  key_column
  
  inputs_present <- which( key_column %in% tolower(input_vector) )
  inputs_present
  
  if ( length(inputs_present) == 0 ) {
    stop ( "The inputs were not found")
  } else if ( length(inputs_present) < length(input_vector)) {
    warning ( "Not all the inputs were found in the data table.")
  }
  
  input_matrix <- cm[inputs_present,  ]
  
  final_names <- NULL
  if (! is.null(indicator_names)) {
    if ( length(indicator_names) == nrow ( input_matrix) ) {
       final_names <- indicator_names
    } else {
      warning ( 'The number of new indicator names is different from indicators, 
                default names are used')
    }
  }
  
  if ( is.null(final_names))  {
    final_names <- paste0(as.character(unlist(input_matrix[,1])), "_indicator")
  }
  
  input_matrix[,1] <- final_names
  
  if ( !is.null(digits)) matrix_round (input_matrix, digits) else  input_matrix
 
}
