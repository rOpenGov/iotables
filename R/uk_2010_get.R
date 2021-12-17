#' Get United Kingdom Input-Output Analytical Tables, 2010
#'
#' This function will retrieve any primary input from the input-output 
#' table: United Kingdom Input-Output Analytical Tables, 2010												
#' (consistent with UK National Accounts Blue Book 2013 &
#'  UK Balance of Payments Pink Book 2013)							
#' by Richard Wild.
#' @param path A path to the downloaded file, if already exists, given with
#' \code{file.path()} function. 
#' @source \href{https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2010detailed/ukioanalyticaltablesio1062010detailedpubversion.xls}{ukioanalyticaltablesio1062010detailedpubversion.xls}
#' @importFrom dplyr select mutate across left_join
#' @importFrom dplyr bind_cols
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble rownames_to_column tibble
#' @importFrom rlang set_names
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#' @examples
#' \dontrun{
#' uk2010 <- uk_2010_get()
#' }
#' @keywords internal

uk_2010_get <- function ( path = NULL )  {
  
  ## Non-standard evaluation variable initiatlization -----------------
  
  if ( is.null(path)) { 
    path <- file.path(tempdir(), 
                      'ukioanalyticaltablesio1062010detailedpubversion.xls')
       }
  
  if ( ! file.exists(path) ) {
    utils::download.file("https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2010detailed/ukioanalyticaltablesio1062010detailedpubversion.xls",
                         file.path(tempdir(),
                                   "ukioanalyticaltablesio1062010detailedpubversion.xls"), 
                         mod = 'wb') 
    }
    
  metadata_skip <- 1
  column_spec_skip <- 5
  
  for ( i in 2:8 ) {
    data_skip <- column_spec_skip + 1
    
    uk_metadata <- readxl::read_excel (path,
                                       sheet = i, 
                                       skip = metadata_skip,
                                       col_names = FALSE, 
                                       n_max = 2) %>%
      rlang::set_names ( "values") %>%
      bind_cols ( tibble::tibble ( vars = c("indicator", "unit"))) %>%
      pivot_wider ( names_from = .data$vars, values_from = .data$values)
      #tidyr::spread ( vars, values )
    
    message ( "Reading ... ", uk_metadata$indicator )
    
    
    uk_column_specs <- readxl::read_excel (path,
                                           sheet = i, 
                                           skip = column_spec_skip,
                                           col_names = FALSE, 
                                           n_max = 2) %>%
      select (- 1) %>% 
      tibble::rownames_to_column () %>% 
      pivot_longer( -all_of("rowname"), names_to = "var", values_to="value") %>%
      tidyr::pivot_wider(names_from = .data$rowname, values_from = .data$value)  %>%
      rlang::set_names(c("remove", "uk_col", "uk_col_lab")) %>%
      dplyr::select  ( -.data$remove ) %>%
      mutate(across(where(is.factor), as.character)) 
    
    uk_data_sheet <- readxl::read_excel ( path,
                                          sheet = i,
                                          skip = data_skip,
                                          col_names = TRUE) %>%
      pivot_longer ( cols = 3:ncol(.), 
                     names_to = "uk_col_lab", 
                     values_to = "values") %>%
      #tidyr::gather( uk_col_lab, values, !!3:ncol(.)) %>%
      rlang::set_names(c("uk_row", "uk_row_lab", 'uk_col_lab', 'values')) %>%
      mutate(values = as.numeric(as.character(values))) %>%
      dplyr::left_join (uk_column_specs, 
                        by = "uk_col_lab") %>%
      mutate (indicator = uk_metadata$indicator ) %>%
      mutate (unit = uk_metadata$unit ) %>%
      mutate (across(where(is.factor), as.character) ) 
    
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
   
  remove_dot <- function(x) gsub("\\.", "-", x)
  
  uk_data %>%
    mutate ( uk_col_lab = gsub("\n", ' ', uk_col_lab)) %>%
    mutate ( uk_col_lab = trimws(uk_col_lab, 'both')) %>%
    mutate ( uk_col = ifelse(is.na(uk_col), uk_col_lab, uk_col)) %>%
    mutate ( uk_row = ifelse(is.na(uk_row), uk_row_lab, uk_row)) %>%
    mutate ( across(all_of(c("uk_row", "uk_col")), remove_dot)) %>%
    mutate ( values = ifelse (is.na(values), 0, values)) %>%
    mutate ( geo = 'UK') %>%
    mutate ( year = 2010 ) %>%
    mutate ( unit = 'MIO_NAC') %>%
    mutate ( unit_lab = "Million national currency") %>%
    mutate ( geo_lab = 'United Kingdom')
    
}


