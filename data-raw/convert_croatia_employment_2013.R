
data("croatia_employment")


croatia_employment_2013 <- convert_to_ascii(croatia_employment_2013)
usethis::use_data(croatia_employment_2013, overwrite = TRUE)
