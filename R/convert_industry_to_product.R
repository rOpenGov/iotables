#' Convert industry columns to product columns
#' 
#' A utility function to adjust column names for supplementary data, 
#' for example, air pollution data, where the industry codes are denoted
#' with `ind_ava` or `ind_use` (or similar codes).
#' 
#' For example, `A01` (agriculture) is converted to `CPA_A01` (product
#' of agriculture).
#'
#' @param data_table A table where the names of the columns are denoted
#' with the `ind_ava` or `ind_use` or `ind_use` vocabularies, for 
#' example, agriculture is denoted with `A01`.
#'
#' @returns A converted, named vector or table where the columns are
#' denoted with `prd_ava` codes, i.e., `A01` is converted to `CPA_A01`.
#' @export
#'
#' @seealso [supplementary_add()]
#' @importFrom dplyr rename select bind_cols
#' 
#' @examples
#' data_table <- data.frame(
#'    ind_use = "A01", 
#'    A01 = 1,  A02 = 3, TOTAL = 4, 
#'    HH = 2
#' )
#' convert_industry_to_product(data_table)
convert_industry_to_product <- function(data_table) {
  
  ind_ava <- getdata("ind_ava")
  intermediate <- filter(ind_ava, block == "intermediate")
  
  intermed_cols <- data_table[, which(names(data_table) 
                                      %in% intermediate$id)]
  
  other_col_n <- which(!names(data_table) %in% names(intermed_cols))
  
  other_cols <- dplyr::select(data_table, all_of(other_col_n))
  names(intermed_cols) <- paste0("CPA_", names(intermed_cols))
  
  first_col <- other_cols %>% select(1)
  first_col_name <- names(first_col)
  
  return_df <- bind_cols(first_col, intermed_cols) %>%
    dplyr::rename( TOTAL = CPA_TOTAL )
  if (ncol(other_cols)>1) {
    dplyr::bind_cols(return_df, other_cols %>% select(-any_of(first_col_name)))
  } else{
    return_df
  }
}

