library ( dplyr)
metadata <- readxl::read_excel("C:/Users/Daniel Antal/OneDrive - Visegrad Investments/2017 Projektek/iotables/data-raw/metadata.xlsx", 
                       sheet = "all")%>%
  dplyr::arrange( numeric_label )


sk_io <- iotable_get2 ( source = "naio_cp17_r2", geo = "SK",
                           year = 2010, unit = "MIO_EUR",
                           labelling = "iotables" )

sk_use <- use_table_get2 ( source = "naio_cp17_r2", geo = "SK",
                                                    year = 2010, unit = "MIO_EUR", households = "FALSE",
                                                    labelling = "iotables" )
