#' Get United Kingdom Input-Output Analytical Tables, 2010
#'
#' This function will retrieve any primary input from the input-output table.
#' United Kingdom Input-Output Analytical Tables, 2010												
#' (consistent with UK National Accounts Blue Book 2013 &
#'  UK Balance of Payments Pink Book 2013)							
#' by Richard Wild.
#' @param path A path to the downloaded file, if already exists, given with
#' \code{file.path()} function. 
#' @source \href{https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2010detailed/ukioanalyticaltablesio1062010detailedpubversion.xls}{ukioanalyticaltablesio1062010detailedpubversion.xls}
#' @importFrom dplyr select mutate_if mutate left_join mutate_at funs vars one_of
#' @importFrom tidyr spread gather 
#' @importFrom tibble rownames_to_column tibble
#' @importFrom purrr set_names
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#' @examples
#' \dontrun{
#' uk2010 <- uk_2010_get()
#' }

uk_2010_get <- function ( path = NULL )  {
  
  value <- values <- rowname <- remove <- . <- NULL
  geo <- geo_lab <- year <- unit <- unit_lab <- NULL
  uk_col <- uk_col_lab <- uk_row <- uk_row_lab <- X__1 <- var <- NULL
  
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
  
  for ( i in 2:8 ) {
    data_skip <- column_spec_skip + 1
    
    uk_metadata <- readxl::read_excel ( path,
                                        sheet = i, 
                                        skip = metadata_skip,
                                        col_names = FALSE, 
                                        n_max = 2) %>%
      dplyr::select ( 1 ) %>%
      dplyr::rename ( values = X__1 ) %>%
      cbind ( tibble::tibble ( vars = c("indicator", "unit"))) %>%
      tidyr::spread ( vars, values )
    
    message ( "Reading ... ", uk_metadata$indicator )
    
    
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
      dplyr::select  ( -remove ) %>%
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
    
    uk_data_sheet <- uk_data_sheet %>%
      mutate ( uk_col = ifelse ( grepl('on-market', uk_col_lab), 
                                 paste0("NM_", uk_col), 
                                 uk_col), 
               uk_row = ifelse ( grepl('on-market', uk_row_lab), 
                                 paste0("NM_", uk_row), 
                                 uk_row)) %>%
      mutate ( uk_col = ifelse ( grepl('NPISH', uk_col_lab), 
                                 paste0("NPISH_", uk_col), 
                                 uk_col), 
               uk_row = ifelse ( grepl('NPISH', uk_row_lab), 
                                 paste0("NPISH_", uk_row), 
                                 uk_row)) 
    
    if (i>2) uk_data <- rbind(uk_data, uk_data_sheet) else uk_data <- uk_data_sheet
  }
   
  uk_data %>%
    dplyr::mutate ( uk_col_lab = gsub("\n", ' ', uk_col_lab)) %>%
    dplyr::mutate ( uk_col_lab = trimws(uk_col_lab, 'both')) %>%
    dplyr::mutate ( uk_col = ifelse(is.na(uk_col), uk_col_lab, uk_col)) %>%
    dplyr::mutate ( uk_row = ifelse(is.na(uk_row), uk_row_lab, uk_row)) %>%
    dplyr::mutate_at ( dplyr::vars(one_of("uk_row", "uk_col")), 
                dplyr::funs(gsub("\\.", "-", .))) %>%
    dplyr::mutate_at ( dplyr::vars(dplyr::one_of("uk_row", "uk_col")), 
                dplyr::funs(gsub(" & ", "-", .))) %>%
    dplyr::mutate ( values = ifelse (is.na(values), 0, values)) %>%
    dplyr::mutate ( geo = 'UK') %>%
    dplyr::mutate ( year = 2010 ) %>%
    dplyr::mutate ( unit = 'MIO_NAC') %>%
    dplyr::mutate ( unit_lab = "Million national currency") %>%
    dplyr::mutate ( geo_lab = 'United Kingdom')
    
}


