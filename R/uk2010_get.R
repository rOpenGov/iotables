#' Get United Kingdom Input-Output Analytical Tables, 2010
#'
#' This function will retrieve any primary input from the input-output table.
#' United Kingdom Input-Output Analytical Tables, 2010												
#' (consistent with UK National Accounts Blue Book 2013 &
#'  UK Balance of Payments Pink Book 2013)							
#' by Richard Wild.
#' @param path A path to the downloaded file, if already exists, given with
#' \code{\link{base::file.path}} function. 
#' @source \href{https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2010detailed/ukioanalyticaltablesio1062010detailedpubversion.xls}
#' {ukioanalyticaltablesio1062010detailedpubversion.xls}
#' @importFrom dplyr select mutate_if mutate left_join
#' @importFrom tidyr spread gather 
#' @importFrom tibble rownames_to_column
#' @importFrom purrr set_names
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#' @examples
#' \dontrun{
#' uk2010 <- uk_2010_get()
#' }
#' @export


uk_2010_get <- function ( path = NULL )  {
  
  if ( is.null(path)) { 
    path <- file.path(tempdir(), 
                      'ukioanalyticaltablesio1062010detailedpubversion.xls')
       }
  
  if ( ! file.exists(path) ) {
    utils::download.file("https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2010detailed/ukioanalyticaltablesio1062010detailedpubversion.xls",
                         file.path(tempdir(), "ukioanalyticaltablesio1062010detailedpubversion.xls"), 
                         mod = 'wb') 
    }
    
  
  metadata_skip = 1
  column_spec_skip = 5
  
  for ( i in 2:8) {
    data_skip <- column_spec_skip + 1
    
    uk_metadata <- readxl::read_excel ( path,
                                        sheet = i, 
                                        skip = metadata_skip,
                                        col_names = FALSE, 
                                        n_max = 2) %>%
      dplyr::select ( 1 ) %>%
      dplyr::rename ( values = X__1 ) %>%
      cbind ( tibble ( vars = c("indicator", "unit"))) %>%
      tidyr::spread ( vars, values )
    
    message ( "Reading ...", uk_metadata$indicator )
    
    
    uk_column_specs <- readxl::read_excel ( path,
                                            sheet = i, 
                                            skip = column_spec_skip,
                                            col_names = FALSE, 
                                            n_max = 2) %>%
      dplyr::select ( - 1 ) %>% 
      tibble::rownames_to_column (.) %>% 
      tidyr::gather(var, value, -rowname) %>% 
      tidyr::spread(rowname, value)  %>%
      purrr::set_names(., c("remove", "uk_col", "uk_col_lab")) %>%
      dplyr::select  (-remove) %>%
      dplyr::mutate_if ( is.factor, as.character ) 
    
    uk_data_sheet <- readxl::read_excel ( path,
                                    sheet = i,
                                    skip = data_skip,
                                    col_names = TRUE) %>%
      tidyr::gather( uk_col_lab, values, !!3:ncol(.)) %>%
      purrr::set_names(., "uk_row", "uk_row_lab", 'uk_col_lab', 'values') %>%
      dplyr::mutate(values = as.numeric(as.character(values))) %>%
      dplyr::left_join (.,  uk_column_specs, by = "uk_col_lab") %>%
      dplyr::mutate (indicator = uk_metadata$indicator ) %>%
      dplyr::mutate ( unit = uk_metadata$unit ) %>%
      dplyr::mutate_if ( is.factor, as.character ) 
    
    if (i>2) uk_data <- rbind(uk_data, uk_data_sheet) else uk_data <- uk_data_sheet
  }
  
  uk_data
}


