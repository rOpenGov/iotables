#' Validate source Parameter
#' @param source Possible data sources. 

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