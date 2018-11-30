library (dplyr) ; library(tidyr) ; library (devtools)

netherlands_2006  <- read.csv2 ( "data-raw/netherlands_2006.csv") 

usethis::use_data ( netherlands_2006, 
                    overwrite = TRUE)


load ( file.path( 'data', 'netherlands_2006.rda'))
