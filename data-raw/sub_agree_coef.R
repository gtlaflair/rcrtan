library(readr)

sub_agree_coef <- read_csv('data-raw/sub_agree_coef.csv')

devtools::use_data(sub_agree_coef, overwrite = TRUE)
