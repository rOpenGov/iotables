
library (dplyr)
metadata <- readxl::read_excel("data-raw/metadata.xlsx", 
                               sheet = "all") %>%
  dplyr::arrange( numeric_label )


devtools::use_data(metadata,
                   internal = FALSE, overwrite = TRUE)
