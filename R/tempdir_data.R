#' @title Download to and retrieve from the temporary directory a Eurostat dataset 
#' @param id The id of a Eurostat product.
#' @keywords internal 

tempdir_data <- function(id) {
  potential_matches <- grepl(id, dir (file.path(tempdir(), "eurostat"))) 
  if ( sum(potential_matches)==1) {
    temp_filename <- dir(file.path(tempdir(), "eurostat"))[which (potential_matches)]
    readRDS(file.path(tempdir(), "eurostat", temp_filename))
  } else {
    eurostat::get_eurostat("env_ac_ainah_r2")
  }
}