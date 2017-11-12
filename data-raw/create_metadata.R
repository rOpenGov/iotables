
library (dplyr)
metadata <- readxl::read_excel("C:/Users/Daniel Antal/OneDrive - Visegrad Investments/2017 Projektek/iotables/data-raw/metadata.xlsx", 
                               sheet = "all") %>%
  dplyr::arrange( numeric_label )


devtools::use_data(metadata,
                   internal = FALSE, overwrite = TRUE)