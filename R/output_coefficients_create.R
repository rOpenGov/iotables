#' @title Create output coefficients
#' 
#' @description Create coefficients from supplementary data.
#'  
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_cols
#' @importFrom tibble as_tibble
#' @param data_table A matrix or vector that should have a key column.
#' @param output An output vector created with \code{output_get}.
#' @examples 
#' emissions_de <- germany_airpol[, -3] 
#' emissions_de <- vector_transpose_wider( names_from = "iotables_col", values_from = "value")
#' output_bp <- output_get (iotable_get())
#' output_coefficients_create (emissions_de, output_bp)

output_coefficients_create <- function ( data_table, output ) {
  
  assertthat::assert_that(any(names(data_table) %in% names(output)), 
                          msg = "in output_coefficients_create(data_table, output): data_table and output have no common columns.")
  
  fn_coeff_create <- function(x) {
    numeric_cols <- which ( vapply(data_table, is.numeric, logical(1)) & names(data_table) %in% names(output))
    
    coefficient_vector <- data_table[x, as.numeric(numeric_cols)] / output[,as.numeric(numeric_cols)]
    
    key_column_create(names(data_table)[1], paste0(as.character(data_table[x,1]), "_coefficient"))  %>%
      bind_cols ( coefficient_vector ) 
  }
  
  do.call ( rbind, lapply ( 1:nrow(emissions_de), fn_coeff_create)) %>% as_tibble()
  
}
