data <- readRDS('data/brown-item-data.rds')

library(plyr)
library(dplyr)
library(tidyr)


IFpass <- function(data, pass_score){
  data$pass <- ifelse(data$Total >= pass_score, 'pass', 'fail')
  
  IF_pass <- data %>%
    filter(pass %in% 'pass') %>%
    select(contains("Q")) %>%
    colMeans()  

  return(IF_pass)
}

IFfail <- function(data, pass_score){
  data$pass <- ifelse(data$Total >= pass_score, 'pass', 'fail')

  IF_fail <- data %>%
    filter(pass %in% 'fail') %>%
    select(contains("Q")) %>%
    colMeans()
  
  return(IF_fail)
}

bindex <- function(data, pass_score){
  data$pass <- ifelse(data$Total >= pass_score, 'pass', 'fail')
  
  IF_pass <- data %>%
    filter(pass %in% 'pass') %>%
    select(contains("Q")) %>%
    colMeans()
  
  IF_fail <- data %>%
    filter(pass %in% 'fail') %>%
    select(contains("Q")) %>%
    colMeans()
  
  return(IF_pass - IF_fail)
}

agree <- function(data, pass_score){
data$pass <- ifelse(data$Total >= pass_score, 'pass', 'fail')

}

