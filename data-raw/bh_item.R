# library(readr)

bh_item <- read.csv('data-raw/bh_item.csv')

devtools::use_data(bh_item, overwrite = TRUE)
