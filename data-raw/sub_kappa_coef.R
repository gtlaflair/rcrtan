library(readr)

sub_kappa_coef <- read_csv('data-raw/sub_kappa_coef.csv')

devtools::use_data(sub_kappa_coef, overwrite = TRUE)
