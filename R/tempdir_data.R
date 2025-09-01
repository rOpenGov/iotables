#' @title Download to and retrieve from the temporary directory a Eurostat dataset
#'
#' @description To save time of downloading and processing during a session, the download functions
#' rely on the use of saving copies in the tempdir(). The downloads are always placed there and
#' each import looks for them first in the tempdir().
#'
#' @param id The id of a Eurostat product.
#' @param force_download Defaults to \code{FALSE} which will use the existing downloaded file
#' in the \code{data_directory} or the temporary directory, if it exists. \code{TRUE} will
#' try to download the file from the Eurostat warehouse.
#' @keywords internal

tempdir_data <- function(id, force_download) {
  if (is.null(force_download)) {
    force_download <- FALSE
  }
  potential_matches <- grepl(paste0(id, "_processed.rds"), dir(file.path(tempdir())))
  if (sum(potential_matches) == 1 & force_download == FALSE) {
    message("The ", paste0(id, "_processed.rds"), " is retrieved from the temporary directory.")
    readRDS(file.path(tempdir(), paste0(id, "_processed.rds")))
  } else if (sum(grepl(id, dir(file.path(tempdir(), "eurostat")))) == 1 & force_download == FALSE) {
    potential_matches <- grepl(id, dir(file.path(tempdir(), "eurostat")))
    temp_filename <- dir(file.path(tempdir(), "eurostat"))[which(potential_matches)]
    message("The bulk Eurostat file is retrieved from the temporary directory.")
    readRDS(file.path(tempdir(), "eurostat", temp_filename))
  } else {
    downloaded <- tryCatch(eurostat::get_eurostat(id),
      error = function(e) message("No data was found with this identifier.")
    )
  }
}
