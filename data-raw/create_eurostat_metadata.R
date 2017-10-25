cc

collect_metadata_60 
   possible_sources <- c( "naio_cp17_r2", "naio_10_cp1700", "naio_10_cp1750", 
                         "naio_10_pyp1700", "naio_10_pyp1750", 
                         "naio_17_agg_60_r2", "naio_18_agg_60_r2", 
                         "naio_19_agg_60_r2")
   metadata_collector_t <- NULL
   metadata_collector_p <- NULL
   
   for (i in 1:length(possible_sources)) {
     message ( i," ", possible_sources[i])
     tmp <- iotables_download ( possible_sources[i]) 
     tmp$source <- source
     if ( "stk_flow" %in% names(tmp)) {
       if ( is.null (metadata_collector_p) ) { 
         metadata_collector_p <- distinct(tmp, source, stk_flow, induse, prod_na, 
                                          stk_flow_lab, induse_lab, prod_na_lab)  } else {
                                            tmp_narrow <- distinct(tmp, source, stk_flow, induse, prod_na, 
                                                                   stk_flow_lab, induse_lab, prod_na_lab)                                   
                                            metadata_collector_p  <- rbind ( metadata_collector_p, 
                                                                             tmp_narrow )
                                            message ("Successfuly joined.")
                                          }
     } else {
       if ( is.null(metadata_collector_t)) {
         metadata_collector_t <- distinct(tmp, source, t_rows2, t_cols2, 
                                          t_rows2_lab, t_cols2_lab) 
       } else { metadata_collector_t  <-rbind ( metadata_collector_t, 
                                                distinct(tmp, source, t_rows2, t_cols2, 
                                                         t_rows2_lab, t_cols2_lab))
       message ("Successfuly joined.")
       }
       
     }
   }


##P type checking 
rows_3 <- read.csv ( "rows_3.csv", stringsAsFactors = F) %>%
  distinct ( t_rows2, t_rows2_lab, iotables_label_r, ordering_r )
cols_3 <- read.csv ( "cols_3.csv", stringsAsFactors = F) %>%
  distinct ( t_cols2, t_cols2_lab, iotables_label_c, ordering_c )

m_t <- metadata_collector_t %>%
  left_join ( ., rows_3) %>%
  left_join (., cols_3)

t_rows_data <- m_t %>%
  select ( t_rows2, t_rows2_lab, iotables_label_r, ordering_r) %>%
  rbind ( ., germany_metadata_rows) %>%
  distinct (t_rows2, t_rows2_lab, iotables_label_r, ordering_r ) 
 
t_rows_data <- write.csv ('t_rows_data.csv', stringsAsFactors = F)
t_cols_data <- write.csv  ('t_cols_data.csv', stringsAsFactors = F)

t_cols_data  <- metadata_collector_t %>%
  left_join ( ., rows_3) %>%
  left_join (., cols_3)  %>%
  select ( t_cols2, t_cols2_lab, iotables_label_c, ordering_c) %>%
  rbind ( ., germany_metadata_cols) %>%
  distinct ( t_cols2, t_cols2_lab, iotables_label_c, ordering_c ) 

p_cols_data <- read.csv ( 'data_raw/p_cols_data.csv', stringsAsFactors = F)
p_rows_data <- read.csv ("data_raw/p_rows_data.csv", stringsAsFactors = F)

devtools::use_data(t_rows_data, t_cols_data, p_cols_data, p_rows_data,
                   internal = TRUE, overwrite = TRUE)
  
