#' Validate examples
#' @importFrom  stringi stri_enc_toascii 
#' @imoprtFrom dplyr mutate_if
#' @param dat Possible data sources. 

convert_to_ascii <- function(dat) { 
  
  names(dat) <- stringi::stri_enc_toascii(names(dat))
  
  fct_to_ascii <- function(x) {
    levels(x) <- stringi::stri_enc_toascii(levels(x))
  }
  
  dat %>%
    mutate_if ( is.character, stringi::stri_enc_toascii ) %>%
    mutate_if ( is.factor, fct_to_ascii )
  
}