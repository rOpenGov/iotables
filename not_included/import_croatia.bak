library ( dplyr) ; library (tidyr) ; library ( readxl)

#1800 - Symmetric input-output table for domestic production (product x product)
croatiafile <- "data-raw/Croatia_2010.xlsx"

#read in the col labels
croatia_technology_cols_1700 <- data.frame (
  t_cols2 = as.character(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "C521:CF521", 
      col_names = FALSE
    )), 
  t_cols2_lab = as.character(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "C520:CF520", 
      col_names = FALSE
    )), 
  stringsAsFactors = FALSE
)

add_preffix = croatia_technology_cols_1700$t_cols2[1:66] 

#read in the rows and the labels.

croatia_technology_rows_1700 <- data.frame (
  t_rows2 = c(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "A431:A512", 
      col_names = FALSE
    )), 
  t_row2_lab =  c(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "B431:B512", 
      col_names = FALSE
    )), 
  stringsAsFactors = FALSE
)
names (croatia_technology_rows_1700) = c("t_rows2", "t_rows2_lab")

croatia_technology_data_1700 <- cbind ( 
  croatia_technology_rows_1700, 
  croatia_technology_cols_1700
  )

croatia_2010_1700 <- readxl::read_excel (
  path = croatiafile,
  sheet = 1, 
  range = "A430:CF512", 
  col_names = TRUE
)
names ( croatia_2010_1700)[1:2] <- c('t_rows2', "t_rows2_lab") 

croatia_2010_1700 <- croatia_2010_1700 %>%
  tidyr::gather ( t_cols2, values, !! 3:ncol(.) ) %>%
  dplyr::left_join ( ., croatia_technology_cols_1700, by = "t_cols2") %>%
  dplyr::mutate ( unit = "T_NAC") %>%
  dplyr::mutate ( geo = "HR") %>%
  dplyr::mutate ( geo_lab = "Croatia") %>%
  dplyr::mutate ( time  = as.Date('2010-01-01')) %>%
  dplyr::mutate ( t_cols2 = ifelse (t_cols2 %in% add_preffix, 
                    yes = paste0("CPA_", t_cols2), 
                    no = t_cols2 ))

devtools::use_data(croatia_2010_1700, overwrite = TRUE)
