#' Reorganize employment data according to the SIOT aggregation.
#'
#' There are two issues to resolve here. The employment data has aggregation rows that are
#' duplicates and do not conform the IO tables. The IO tables also have aggregations that
#' cannot be found in the employment data. 
#' It may be the case that you have nationsl employment figures in a slightly different
#' format. In this case read in the data anyway in a tidy format and run the function to remove the
#' unnecessary aggregations and to re-aggregate according to the SIOT format. 
#' For this filtering the Croatian (English language) employment data filters are included in
#' the package.
#' @param employment_df A data frame containing the employment data.
#' @param matching  A table to match employment data to the aggregation scheme of the Eurostat IO tables.  
#' @param label Label name used for matching employment_df and matching, defaults to "employment_label", as used in the Croatia example.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter distinct full_join rename group_by summarize
#' @importFrom tidyr spread
#' @examples
#' data (croatia_employment_aggregation)
#' data (croatia_employment_2013)
#' employment_vector <- employment_aggregate (employment_df = croatia_employment_2013,
#'                       matching = croatia_employment_aggregation) 
#' @export

employment_aggregate <- function ( employment_df, matching,
                                   label = "employment_label" ) {
  t_cols2 = NULL; employment = NULL; summarize = NULL; 
  t_rows2 = NULL; group_by = NULL; employment_label = NULL
  
  names(employment_df)[which(names(employment_df) == label)] <- "employment_label"
  names(matching)[which(names(matching) == label)] <- "employment_label"
  
  employment_matched <- full_join (matching, employment_df, 
                                   by = "employment_label" ) %>%
    filter (employment_label != "aggregate" ) %>%
    filter ( t_cols2 != "" ) %>%
    filter ( !is.na(t_cols2)) %>%
    distinct ( t_cols2, employment ) %>%
    dplyr::group_by ( t_cols2 ) %>%
    dplyr::summarize ( employment = sum(employment, na.rm = TRUE))%>%
    tidyr::spread ( t_cols2, employment)
  
  t_rows2 = "employment"
  
  employment_matched <-cbind( t_rows2, employment_matched)
  employment_matched$t_rows2 <- as.character(employment_matched$t_rows2)
  names(employment_matched)[which(names(employment_matched) == "employment_label")] <- label #reset user label name

  return ( employment_matched )
}
