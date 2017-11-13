#' Create a use (input flow) matrix
#' 
#' @param source A data source, for example "naio_10_cp1700".
#' @param geo A country code or a country name.  For example, "SK" or as "Slovakia".
#' @param year A numeric variable containing the year. Defaults to 2010, because this year has the most data. 
#' @param unit A character string containing the currency unit, defaults to "MIO_NAC" (million national currency unit). The alternative is "MIO_EUR". 
#' @param households If you need to make household demand endogenous, or "close the households off", TRUE selects 
#' wages and final household consumption. This is needed for induced-effects calculations.
#' @param labelling Defaults to "iotables" which gives standard row and column names regardless of the
#' source of the table, or if it is a product x product, industry x industry or product x industry table.
#' The alternative is "short" which is the original short row or column code of Eurostat or OECD.
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join mutate_if arrange
#' @importFrom tidyr gather spread 
#' @importFrom forcats fct_reorder
#' @examples 
#' use_de <- use_table_get( source = "germany_1990", geo = 'DE', year = 1990,
#'                        unit = "MIO_EUR", labelling  = 'iotables')
#' \dontrun{
#' sk_use <- use_table_get ( source = "naio_cp17_r2", geo = "SK",
#'                           year = 2010, unit = "MIO_EUR", households = "FALSE",
#'                           labelling = "iotables" )
#'  }

#' @export 

use_table_get <- function ( source = "germany_1990", geo = "DE",
                            year = 1990, unit = "MIO_EUR",
                            households = FALSE, labelling = "iotables" ){  
  
  time = NULL; t_cols2 = NULL; t_rows2 = NULL; values = NULL ;.= NULL #non-standard evaluation creates a varning in build. 
  ordering_r = NULL ; ordering_c = NULL; r_quadrant = NULL; c_quadrant = NULL;
  iotables_label_r =NULL; iotables_label_c = NULL; 
  
  tmp_rds <- paste0(tempdir(), "\\", source, "_", labelling, ".rds")
  source_inputed <- source ; unit_input = unit
  
  ##Veryfing source parameter and loading the labelling  ----
  prod_ind <- c("naio_10_cp1700", "naio_10_cp1750", "naio_10_pyp1700", "naio_10_pyp1750")
  trow_tcol <-  c("naio_cp17_r2", "naio_17_agg_60_r2", "naio_17_agg_10_r2")
  
  if ( source %in% prod_ind ) { 
    metadata_rows <- p_rows_data 
    metadata_cols <- p_cols_data  
  } else if ( source %in% trow_tcol ) {
    metadata_rows <- t_rows_data  
    metadata_cols <- t_cols_data 
  } else if ( source == "germany_1990") {
    metadata_rows <-  germany_metadata_rows  
    metadata_cols <-  germany_metadata_cols 
  } else {
    stop ("This type of input-output database is not (yet) recognized by iotables.")
  }
  
  metadata_rows <- dplyr::mutate_if ( metadata_rows, is.factor, as.character )
  metadata_cols <- dplyr::mutate_if ( metadata_cols, is.factor, as.character )
  
  if ( source == "germany_1990") {
    labelled_io_table <- iotable_get ( source, geo, year, unit, labelling )     # use germany example 
    
  } else {
    if ( tmp_rds %in% list.files (path = tempdir()) ) {
      labelled_io_table <- readRDS( tmp_rds ) 
    } else { 
      labelled_io_table <- iotable_get ( source, geo, year, unit, labelling )  }
  } # use eurostat files 

 if (households == TRUE) {
    if ( any(c('consumption_expenditure_household', 'P3_S14') %in% 
            metadata_cols $t_cols2)) {
     metadata_cols$c_quadrant[which(
       metadata_cols$t_cols2 %in% c('consumption_expenditure_household', 'P3_S14')) ] <- "1_2"
     message ("Added household consumption.")
      } else {
     stop ("Household consumption expenditure is not found in the table.")
   }

   if ( any(c('d11', 'D11') %in% metadata_rows$t_rows2) ) { 
     metadata_rows$r_quadrant[which(metadata_rows$t_rows2 %in% c('d11', 'D11'))] <- "1_3"
     message ("Added wages and salaries (because social security benefits do not induce short-term demand.)")
     } else if ( any(c('d1', 'D1') %in% metadata_rows$t_rows2) ) { 
     metadata_rows$r_quadrant[which(metadata_rows$t_rows2  %in% c('d1', 'D1'))] <- "1_3"
     message ("Added total compensation of employees.")
     } else {
       stop ("Employee compensation was not found in the data frame.")
     }
     
    }
 if ( labelling == "iotables" ) {
    labelled_use_table <- labelled_io_table %>%
      tidyr::gather ( iotables_label_c, values, !! 2:ncol(.)) %>%
      dplyr::mutate_if ( is.factor, as.character ) %>%
      dplyr::left_join(., metadata_rows, by = "iotables_label_r") %>%
      dplyr::left_join(., metadata_cols, by = "iotables_label_c") %>%
      dplyr::mutate ( iotables_label_r = forcats::fct_reorder(iotables_label_r, 
                                              as.numeric(ordering_r))) %>%
      dplyr::mutate ( iotables_label_c = forcats::fct_reorder(iotables_label_c, 
                                              as.numeric(ordering_c))) %>%
      dplyr::arrange (  iotables_label_r,  iotables_label_c ) %>%
      filter ( r_quadrant == "1_3") %>%
      filter ( c_quadrant == "1_2") %>%
      dplyr::select ( iotables_label_r,  iotables_label_c,  values ) %>%
      tidyr::spread ( iotables_label_c, values ) 
    
  } else if ( labelling == "short") {
    labelled_use_table <- labelled_io_table %>%
      tidyr::gather ( t_cols2, values, !! 2:ncol(.)) %>%
      dplyr::mutate_if ( is.factor, as.character ) %>%
      dplyr::left_join(., metadata_rows, by = "t_rows2") %>%
      dplyr::left_join(., metadata_cols, by = "t_cols2") %>%
      dplyr::mutate ( t_rows2 = forcats::fct_reorder(t_rows2, 
                                                      as.numeric(ordering_r))) %>%
      dplyr::mutate ( t_cols2 = forcats::fct_reorder(t_cols2, 
                                                       as.numeric(ordering_c))) %>%
      dplyr::arrange (  t_rows2,  t_cols2 ) %>%
      dplyr::filter ( r_quadrant == "1_3") %>%
      dplyr::filter ( c_quadrant == "1_2") %>%
      dplyr::select ( t_rows2,  t_cols2,  values ) %>%
      tidyr::spread ( t_cols2, values ) 
  }  else {
    stop ("This type of input-output database is not (yet) recognized by iotables.")
  }
  
  #names ( labelled_use_table)[!names (labelled_use_table ) %in% labelled_use_table$iotables_label_r]
  #names ( labelled_use_table)[!names (labelled_use_table ) %in% labelled_use_table$t_rows2]
  
  if(households == TRUE) {
    labelled_use_table[nrow(labelled_use_table ), ncol(labelled_use_table )] <- 0
  }
  
  return ( labelled_use_table ) 
}


