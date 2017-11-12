
digits <- as.character ( c("0", seq(1:9)) )

prod_na_construct <- read.csv("not_included/prod_na_construct.csv", stringsAsFactors = F)
names (prod_na_construct) <- c("part_1", "code_1")

prod_na_q2 <- prod_na_construct %>%
  filter ( code_1 > 32)

prod_na_q1 <- prod_na_construct %>%
  filter ( code_1 < 33) %>%
  dplyr::rename( code_2 = code_1) %>%
  dplyr::rename ( part_3  = part_1)

prod_na_vocabulary <- readxl::read_excel(path = "not_included/prod_na.xlsx")
names (prod_na_vocabulary  ) <- gsub( ":", "_", names (prod_na_vocabulary  ))
prod_na_code <- prod_na_vocabulary %>%
  select ( ns4_notation3, 	ns4_prefLabel ) %>%
  mutate ( part_1 = ns4_notation3) %>%
  left_join ( ., prod_na_q2) %>%
  mutate ( part_2 = ifelse ( is.na(code_1),ns4_notation3, NA )) %>%
  mutate ( part_2 = ifelse ( stringr::str_sub( part_2, 1,4) == "CPA_", 
                            yes = stringr::str_sub( part_2, 5, -1), 
                            no =  part_2)) %>%
  mutate ( part_3 = ifelse ( is.na(code_1), 
                             yes = stringr::str_sub( part_2, 1,1), 
                             no = NA )) %>%
  left_join ( ., prod_na_q1 ) %>%
  mutate ( temp_code = ifelse ( nchar(part_2)>=3, 
                              yes = stringr::str_sub( part_2, 2,3), 
           no = part_2)) %>%
  mutate ( digit_3  = ifelse ( stringr::str_sub(temp_code, 1,1) %in% digits, 
                               stringr::str_sub(temp_code, 1,1), "0")) %>%
  mutate ( digit_4  = ifelse ( stringr::str_sub(temp_code, 2,2) %in% digits, 
                               stringr::str_sub(temp_code, 2,2), "0")) %>%
  mutate ( code_2 = ifelse ( !is.na(code_1), code_1, code_2 )) %>%
  mutate ( ordering_r = as.numeric(paste0(code_2, digit_3, digit_4)))

