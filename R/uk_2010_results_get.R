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
#' @importFrom dplyr select mutate_if mutate rename
#' @importFrom tidyr spread gather 
#' @importFrom tibble tibble
#' @importFrom purrr set_names
#' @importFrom utils download.file
#' @importFrom readxl read_excel
#' @examples
#' \dontrun{
#' uk_results <- iotables:::uk_2010_results_get ()
#' }

uk_2010_results_get <- function ( path = NULL )  {
  
  value <- values <- rowname <- remove <- . <- NULL
  geo <- geo_lab <- year <- unit <- unit_lab <- NULL
  uk_col <- uk_col_lab <- uk_row <- uk_row_lab <- X__1 <- var <- NULL
  Product <- Rank  <- Rank__1 <- Rank__2 <- Rank__3 <- Rank__4 <- NULL
  
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
  
  
  uk_published_multipliers <- readxl::read_excel ( path,
                                          sheet = i, 
                                          skip = 4,
                                          col_names = TRUE) %>%
    dplyr::select ( - 1 ) %>% 
    dplyr::rename ( uk_row_label = Product, 
                    output_multiplier_rank = Rank, 
                    employment_cost_multiplier = Rank__1, 
                    gva_multiplier_rank = Rank__2, 
                    employment_cost_effects_rank = Rank__3,
                    gva_effects_rank = Rank__4 ) %>%
    dplyr::mutate ( indicator = uk_metadata$indicator[1]) %>%
    dplyr::mutate_if ( is.factor, as.character ) 
  
  uk_published_multipliers
    
}


