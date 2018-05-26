# library(readr)

bh_depend <- read.csv('data-raw/bh_depend.csv')

devtools::use_data(bh_depend, overwrite = TRUE)
