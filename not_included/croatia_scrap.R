
library ( dplyr ) ; library  (tidyr) ; library ( iotables)


voc <- readRDS("not_included/t_rows2.rdf") 

doc = XML::xmlParse("not_included/t_rows2.rdf") 

t_rows_vocabulary <- readxl::read_excel(path = "not_included/t_rows_vocabulary.xlsx")
names (t_rows_vocabulary  ) <- gsub( ":", "_", names (t_rows_vocabulary  ))



t_rows_vocabulary$ns5:label

croatia_temp_file <- paste0(tempdir(),
                            "\\croatia.xlsx" )

?download.file 
download.file(url = "https://www.dzs.hr/Hrv_Eng/publication/2015/12-01-04_01_2015.xlsx",
              destfile = croatia_temp_file,
              mode = 'wb' )

t_rows_hr_1700 <- readxl::read_excel(
  path = croatia_temp_file, 
  sheet = 3,
  range = "a430:b512", 
  col_names = TRUE)

##all fine

t_rows_hr_1700  <- t_rows_hr_1700 %>%
  dplyr::rename( t_rows2 = Code) %>%
  left_join (., t_rows_data)
  

io <- readxl::read_excel(
  path = croatia_temp_file, 
  sheet = 3,
  range = "C521:CF521", 
  col_names = FALSE
)P1

dzs_2010_1700  
t_rows are identitcal
a<-iotables::t_rows_data

View (t_rows_data)
#1800 - Symmetric input-output table for domestic production (product x product)
t_rows_hr_1800 <- readxl::read_excel(
  path = croatia_temp_file, 
  sheet = 3,
  range = "a521:b598", 
  col_names = TRUE) 

t_rows_hr_1800  <- t_rows_hr_1800 %>%
  dplyr::rename( t_rows2 = Code) %>%
  left_join (., t_rows_data)


#DP6A = Use of imported products, cif


#1900 - Symmetric input-output table for imports (product x product)
t_rows_hr_1900 <- readxl::read_excel(
  path = croatia_temp_file, 
  sheet = 3,
  range = "a604:b670", 
  col_names = TRUE) 

t_rows_hr_1900  <- t_rows_hr_1900 %>%
  dplyr::rename( t_rows2 = Code) %>%
  left_join (., t_rows_data)
