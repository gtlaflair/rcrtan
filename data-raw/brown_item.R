library(readr)

brown_item <- read_csv('data-raw/brown_item.csv')

devtools::use_data(brown_item, overwrite = TRUE)
