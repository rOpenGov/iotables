#' Get United Kingdom Multipliers and Effects, 2010
#'
#' This function will retrieve the published effects and multipliers from the 
#' United Kingdom Input-Output Analytical Tables, 2010												
#' (consistent with UK National Accounts Blue Book 2013 &
#'  UK Balance of Payments Pink Book 2013)							
#' by Richard Wild.
#' @param path A path to the downloaded file, if already exists, given with
#' \code{file.path()} function.
#' @source \href{https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2010detailed/ukioanalyticaltablesio1062010detailedpubversion.xls}{ukioanalyticaltablesio1062010detailedpubversion.xls}
#' @importFrom dplyr select across mutate rename
#' @importFrom tibble tibble
#' @importFrom rlang set_names
#' @importFrom utils download.file
#' @importFrom tidyr pivot_wider
#' @importFrom readxl read_excel
#' @examples
#' \dontrun{
#' uk_results <- iotables:::uk_2010_results_get ()
#' }

uk_2010_results_get <- function ( path = NULL )  {
  

  if ( is.null(path)) { 
    path <- file.path(tempdir(), 
                      'ukioanalyticaltablesio1062010detailedpubversion.xls')
       }
  
  if ( ! file.exists(path) ) {
    utils::download.file("https://www.ons.gov.uk/file?uri=/economy/nationalaccounts/supplyandusetables/datasets/ukinputoutputanalyticaltablesdetailed/2010detailed/ukioanalyticaltablesio1062010detailedpubversion.xls",
                         file.path(tempdir(), "ukioanalyticaltablesio1062010detailedpubversion.xls"), 
                         mod = 'wb') 
    }
    
  metadata_skip <- 1
  column_spec_skip <- 5
  i <- 9
  data_skip <- column_spec_skip + 1
  
  uk_metadata <- readxl::read_excel (path,
                                     sheet = i, 
                                     skip = metadata_skip,
                                     col_names = FALSE, 
                                     n_max = 2) %>%
    select (1) %>%
    set_names( "values" ) %>%
    cbind ( tibble::tibble ( vars = c("indicator", "unit"))) %>%
    pivot_wider ( names_from = .data$vars, values_from = .data$values)
    
  message ( "Reading ... ", uk_metadata$indicator )
  
  
  uk_published_multipliers <- readxl::read_excel ( path,
                                          sheet = i, 
                                          skip = 4,
                                          col_names = TRUE) %>%
    select ( - 1 ) %>% 
    dplyr::rename ( uk_row_label = .data$Product, 
                    output_multiplier_rank = .data$Rank...4, 
                    employment_cost_multiplier = .data$Rank...6, 
                    gva_multiplier_rank = .data$Rank...8, 
                    employment_cost_effects_rank = .data$Rank...10,
                    gva_effects_rank = .data$Rank...12 ) %>%
    mutate ( indicator = uk_metadata$indicator[1]) %>%
    mutate(across(where(is.factor), as.character))
  
  uk_published_multipliers
    
}


