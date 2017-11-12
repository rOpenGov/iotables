library ( dplyr) ; library (tidyr) ; library ( readxl)

metadata <- readxl::read_excel("C:/Users/Daniel Antal/OneDrive - Visegrad Investments/2017 Projektek/iotables/data-raw/metadata.xlsx", 
                               sheet = "all")%>%
  dplyr::arrange( numeric_label )
#1800 - Symmetric input-output table for domestic production (product x product)
croatiafile <- "data-raw/Croatia_2010.xlsx"

#read in the col labels
croatia_technology_cols_1700 <- data.frame (
 code = as.character(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "C430:CF430", 
      col_names = FALSE
    )), 
  label = as.character(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "C429:CF429", 
      col_names = FALSE
    )), 
  stringsAsFactors = FALSE
)

croatia_technology_cols_1700 <- left_join (croatia_technology_cols_1700, 
                                           filter ( metadata, variable == "t_cols"), 
                                           by = c("code")) %>%
  dplyr::rename( t_cols2 = code ) %>%
  dplyr::rename ( t_cols2_lab = label.y) %>%
  dplyr::select (t_cols2 , t_cols2_lab)


croatia_technology_cols_1800 <- data.frame (
  code = as.character(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "C521:CF521", 
      col_names = FALSE
    )), 
  label = as.character(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "C520:CF520", 
      col_names = FALSE
    )), 
  stringsAsFactors = FALSE
)

croatia_technology_cols_1800 <- left_join (croatia_technology_cols_1800, 
                                           filter ( metadata, variable == "t_cols"), 
                                           by = c("code")) %>%
  dplyr::rename( t_cols2 = code ) %>%
  dplyr::rename ( t_cols2_lab = label.y) %>%
  dplyr::select ( t_cols2 , t_cols2_lab)


croatia_technology_cols_1900 <- data.frame (
  code = as.character(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "C604:CF604", 
      col_names = FALSE
    )), 
  label = as.character(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "C603:CF603", 
      col_names = FALSE
    )), 
  stringsAsFactors = FALSE
)

croatia_technology_cols_1900 <- left_join (croatia_technology_cols_1900, 
                                           filter ( metadata, variable == "t_cols"), 
                                           by = c("code")) %>%
  dplyr::rename( t_cols2 = code ) %>%
  dplyr::rename ( t_cols2_lab = label.y) %>%
  dplyr::select ( t_cols2 , t_cols2_lab)

  
#read in the rows and the labels.

croatia_technology_rows_1700 <- data.frame (
  code = unlist(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "A431:A512", 
      col_names = FALSE
    )), 
  label  =  unlist(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "B431:B512", 
      col_names = FALSE
    )), 
  stringsAsFactors = FALSE
)


croatia_technology_rows_1700 <- left_join (croatia_technology_rows_1700, 
                                           filter ( metadata, variable == "t_rows"), 
                                           by = c("code")) %>%
  dplyr::rename( t_rows2 = code ) %>%
  dplyr::rename ( t_rows2_lab = label.y) %>%
  dplyr::select ( t_rows2, t_rows2_lab )

croatia_technology_rows_1800 <- data.frame (
  code = unlist(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "A522:A598", 
      col_names = FALSE
    )), 
  label  =  unlist(
    readxl::read_excel(
      path = croatiafile, 
      sheet = 1 ,
      range = "B522:B598", 
      col_names = FALSE
    )), 
  stringsAsFactors = FALSE
)


croatia_technology_rows_1800 <- left_join (croatia_technology_rows_1800, 
                                           filter ( metadata, variable == "t_rows"), 
                                           by = c("code")) %>%
  dplyr::rename( t_rows2 = code ) %>%
  dplyr::rename ( t_rows2_lab = label.y) %>%
  dplyr::select ( t_rows2, t_rows2_lab )





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
  dplyr::mutate ( time  = as.Date('2010-01-01')) 
  #dplyr::mutate ( t_cols2 = ifelse (t_cols2 %in% add_preffix, 
  #                  yes = paste0("CPA_", t_cols2), 
  #                  no = t_cols2 ))

croatia_2010_1800 <- readxl::read_excel (
  path = croatiafile,
  sheet = 1, 
  range = "A521:CF598", 
  col_names = TRUE
)
names ( croatia_2010_1800)[1:2] <- c('t_rows2', "t_rows2_lab") 

croatia_2010_1800 <- croatia_2010_1800 %>%
  tidyr::gather ( t_cols2, values, !! 3:ncol(.) ) %>%
  dplyr::left_join ( ., croatia_technology_cols_1800, by = "t_cols2") %>%
  dplyr::mutate ( unit = "T_NAC") %>%
  dplyr::mutate ( geo = "HR") %>%
  dplyr::mutate ( geo_lab = "Croatia") %>%
  dplyr::mutate ( time  = as.Date('2010-01-01')) 

croatia_2010_1900 <- readxl::read_excel (
  path = croatiafile,
  sheet = 1, 
  range = "A604:CF670", 
  col_names = TRUE
)
names ( croatia_2010_1900)[1:2] <- c('t_rows2', "t_rows2_lab") 

croatia_2010_1900 <- croatia_2010_1900 %>%
  tidyr::gather ( t_cols2, values, !! 3:ncol(.) ) %>%
  dplyr::left_join ( ., croatia_technology_cols_1900, by = "t_cols2") %>%
  dplyr::mutate ( unit = "T_NAC") %>%
  dplyr::mutate ( geo = "HR") %>%
  dplyr::mutate ( geo_lab = "Croatia") %>%
  dplyr::mutate ( time  = as.Date('2010-01-01')) 

devtools::use_data(croatia_2010_1700, overwrite = TRUE)
devtools::use_data(croatia_2010_1800, overwrite = TRUE)
devtools::use_data(croatia_2010_1900, overwrite = TRUE)
