library(readr)

brown_depend <- read_csv('data-raw/brown_depend.csv')

devtools::use_data(brown_depend, overwrite = TRUE)
