#' Validate source Parameter
#' @param source Possible data sources.
#' @keywords internal 

validate_source <- function(source) {
  
  possible_download_sources <- c( "naio_10_cp1700",  "naio_10_cp1750", 
                                  "naio_10_pyp1700", "naio_10_pyp1750",
                                  "naio_10_cp15",    "naio_10_cp16",
                                  "naio_10_cp1610",  "naio_10_pyp1610", 
                                  "naio_10_cp1620",  "naio_10_pyp1620", 
                                  "naio_10_cp1630",  "naio_10_pyp1630", 
                                  "uk_2010")
  source <- tolower (source)
  if ( ! source %in%  possible_download_sources ) {
    supported_tables <- paste( possible_download_sources, collapse = ", ")
    stop (source, " is not in supported tables [", supported_tables, "]") 
  }
}

#' @title Is a keyword present in a key column?
#' @param data_table A matrix or vector that should have a key column.
#' @param potential_keywords Potential keywords that should be present in the key column, defaults to
#' \code{NULL} in which case it will asserted that the first column is not numeric.
#' @return A logical variable of length 1, \code{TRUE} or \code{FALSE}.
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @keywords internal 

is_key_column_present <- function (data_table, potential_keywords=NULL) {
 
  assertthat::assert_that( "data.frame" %in% class(data_table), 
                           msg = "The 'data_table', vector or matrix is not a data.frame." )
  
  if ( !is.null(potential_keywords) ) {
    msg_potential_keywords <- paste(potential_keywords, collapse = "', '")
    assertthat::assert_that( 
      any(potential_keywords %in% data_table[,1]), 
      msg = glue::glue("The data_table's has no key_column containing any of '{msg_potential_keywords}'.")
      )
  } else {
    assertthat::assert_that( 
      ! is.numeric(data_table[,1]),
      msg = "The data_table has no key_column (which is always a non-numeric column.)"
    )
  }
}
