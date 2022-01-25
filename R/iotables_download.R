#' @title Download input-output tables
#'
#' @description This function downloads standard input-output table files. Currently only Eurostat files are supported.
#' You are not likely to use this function, because 
#' \code{\link{iotable_get}} will
#' call this function if necessary and properly filter out an 
#' input-output table.
#' 
#' @details The data is downloaded in the \code{tempdir()}under the name the statistical product as an
#' rds file. (For example: \code{naio_10_cp1750.rds}) The temporary directory is emptied at every
#' normal R session exit.
#' 
#' To save the file for further use (which is necessary in analytical work because
#' download times are long) set the  \code{download_directory} [see parameters]. 
#' The function will make a copy of the rds file in this directory.
#'  \itemize{
##'  \item{\code{naio_10_cp1700}}{Symmetric input-output table at basic prices (product by product)}
##'  \item{\code{naio_10_pyp1700}}{Symmetric input-output table at basic prices (product by product) (previous years prices)}
##'  \item{\code{naio_10_cp1750}}{Symmetric input-output table at basic prices (industry by industry)}
##'  \item{\code{naio_10_pyp1750}}{Symmetric input-output table at basic prices (industry by industry) (previous years prices) }
##'  \item{\code{naio_10_cp15}}{Supply table at basic prices incl. transformation into purchasers' prices }
##'  \item{\code{naio_10_cp16}}{Use table at purchasers' prices}
##'  \item{\code{naio_10_cp1610}}{Use table at basic prices}
##'  \item{\code{naio_10_pyp1610}}{Use table at basic prices (previous years prices) (naio_10_pyp1610)}
##'  \item{\code{naio_10_cp1620}}{Table of trade and transport margins at basic prices}
##'  \item{\code{naio_10_pyp1620}}{Table of trade and transport margins at previous years' prices}
##'  \item{\code{naio_10_cp1630}}{Table of taxes less subsidies on products at basic prices}
##'  \item{\code{naio_10_pyp1630}}{Table of taxes less subsidies on products at previous years' prices}
##'  \item{\code{uk_2010_siot}}{United Kingdom Input-Output Analytical Tables data}
##' } 
#' @param source See the available list of sources above in the Description. 
#' @param data_directory Defaults to \code{NULL} when the files will be temporarily stored
#' in the path retrieved by \code{\link[base]{tempdir}}. If it is a different valid directory, 
#' it will try to save the pre-processed data file here with labelling. 
#' @param force_download Defaults to \code{FALSE} which will use the existing downloaded file
#' in the \code{data_directory} or the temporary directory, if it exists. \code{TRUE} will
#' try to download the file from the Eurostat warehouse.
#' @return A nested data frame. Each input-output table is in a separate 
#' row of the nested output, where all the metadata are in columns, and the
#' actual, tidy, ordered input-output table is in the data \code{data} column.
#' The data is saved into the actual \code{tempdir()}, too.
#' @importFrom dplyr filter select mutate left_join rename any_of
#' @importFrom tidyr nest
#' @importFrom eurostat get_eurostat label_eurostat
#' @importFrom lubridate year
#' @importFrom rlang set_names
#' @importFrom glue glue
#' @family import functions
#' @examples
#' \donttest{
#'  io_tables <- iotables_download(source = "naio_10_pyp1750")
#'  }
#' @export

iotables_download <- function ( source = "naio_10_cp1700", 
                                data_directory = NULL,
                                force_download = FALSE ) {

  ## Parameter validation ---------------------------------------------
  if ( ! source %in% c("uk_2010", "germany_1990") ) {
    validate_source(source) 
    downloaded <- tempdir_data(source, force_download)
    } else if ( source == "uk_2010" ) return (uk_2010_get())
  
  if ( all(c("year", "data") %in% names(downloaded)) ) {
    # This is already processed
    message("Returning the processed SIOTs from tempdir. You can override this with force_download=TRUE.")
    return(downloaded)
  }
  
  assertthat::assert_that(
    'data.frame' %in% class(downloaded) & ncol(downloaded)>6 & nrow(downloaded)>1, 
    msg = glue::glue("The download of {source} was not successful.")
  )
  
  lab_names <- paste0(names(downloaded), "_lab")
  
  #label the raw Eurostat file, add rename variables with _lab suffix
  downloaded_labelled <- downloaded  %>%
    eurostat::label_eurostat (fix_duplicated = TRUE) 
  
  assertthat::assert_that(
    length(names(downloaded_labelled)) == length(lab_names), 
    msg = "in iotables_download() ncol(downloaded_labelled) != ncol(downloaded)"
    )
  
  downloaded_labelled <- downloaded_labelled %>%   # add meaningful labels to raw data
    rlang::set_names(lab_names) %>%  
    mutate ( rows = seq_len(nrow(downloaded)) ) %>%  # because long and wide formats are not symmetric
    rename ( values = .data$values_lab ) %>%
    mutate ( year = lubridate::year(.data$time_lab))
  
  # Join the labelled and the not labelled files, so that both versions are avialable
  
  downloaded <- downloaded  %>%
    mutate ( rows = seq_len(nrow(downloaded)) ) %>%
    left_join ( downloaded_labelled, by = c("rows", "values"))
  
  names(downloaded)
  
  if ( source == "naio_cp17_r2" ){
    downloaded$t_cols2 <- plyr::mapvalues(
      downloaded$t_cols2, 
      from = c("CPA_N80-N82", "CPA_R90-R92",  "CPA_E37-E39",
               "CPA_C10-C12", "CPA_C13-C15", 
               "CPA_C31_C32", "CPA_J59_J60", 
               "CPA_J62_J63", "CPA_M69_M70", "CPA_Q87_Q88", 
               "CPA_M74_M75" , "CPA_O84", "CPA_P85", 
               "CPA_D35" ), 
      to = c("CPA_N80-82", "CPA_R90-92", "CPA_E37-39", 
             "CPA_C10-12", "CPA_C13-15", 
             "CPA_C31_32", "CPA_J59_60", 
             "CPA_J62_63", "CPA_M69_70", "CPA_Q87_88", 
             "CPA_M74_75", "CPA_O", "CPA_P", "CPA_D"
      ))
    
    downloaded$t_rows2 <- plyr::mapvalues(
      downloaded$t_rows2, 
      from = c("CPA_N80-N82", "CPA_R90-R92",  "CPA_E37-E39", 
               "CPA_C10-C12", "CPA_C13-C15", 
               "CPA_C31_C32", "CPA_J59_J60", 
               "CPA_J62_J63", "CPA_M69_M70", "CPA_Q87_Q88", 
               "CPA_M74_M75", "CPA_O84", "CPA_P85", "CPA_D35"), 
      to = c("CPA_N80-82", "CPA_R90-92", "CPA_E37-39", 
             "CPA_C10-12", "CPA_C13-15", 
             "CPA_C31_32", "CPA_J59_60", 
             "CPA_J62_63", "CPA_M69_70", "CPA_Q87_88", 
             "CPA_M74_75", "CPA_O", "CPA_P", "CPA_D")
    )
  } #end of _r2 
  
  
  if ( "stk_flow" %in% names ( downloaded ) ) {
    downloaded_nested <- nest (
      downloaded, 
      data = -any_of(c( "geo", "geo_lab", "time", "time_lab", 
                        "year", "unit", "unit_lab", "stk_flow", "stk_flow_lab"))
      ) 
  } else { 
    downloaded_nested <- nest (
      downloaded, 
      data = -any_of(c( "geo", "geo_lab", "time", "time_lab", 
                        "year", "unit", "unit_lab"))
      ) 
  }

  if( !is.null(data_directory) ) {
    assert_that(dir.exists(data_directory), 
                msg = glue::glue("The data_directory={data_directory} does not exist."))
    
    save_file_name <- file.path(data_directory, paste0(source, "_processed.rds"))  # shoud have different name for processed
    message("Saving ", nrow(downloaded_nested), " input-output tables.")
    saveRDS(downloaded_nested, file = save_file_name, version = 2)
    message ( "Saved the raw data of this table type in ",
              save_file_name, "." )
  } else {
    save_file_name <- file.path(tempdir(), paste0(source, "_processed.rds"))
    message("Saving ", nrow(downloaded_nested), " input-output tables into the temporary directory.")
    saveRDS(downloaded_nested, file = save_file_name , version=2)
    message ("Saved the raw data of this table type in temporary directory ",
              save_file_name, "." )
  }
  
  downloaded_nested
}