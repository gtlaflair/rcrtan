

library(plyr)
library(dplyr)
library(tidyr)

# Item facility -------------------------------------------------------


IFpass <- function(data, items, cut_score, scale){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  IF_pass <- data %>%
    filter(pass %in% 'pass') %>%
    .[items] %>%
    colMeans()

  return(IF_pass)
}


IFfail <- function(data, items, cut_score){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  IF_fail <- data %>%
    filter(pass %in% 'fail') %>%
    .[items] %>%
    colMeans()
  
  return(IF_fail)
}

# B-index ---------------------------------------------------

bindex <- function(data, items, cut_score){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  IF_pass <- data %>%
    filter(pass %in% 'pass') %>%
    .[items] %>%
    colMeans()
  
  IF_fail <- data %>%
    filter(pass %in% 'fail') %>%
    .[items] %>%
    colMeans()
  
  return(IF_pass - IF_fail)
}

# Agreement index --------------------------------------------

agree_index <- function(data, items, cut_score){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')

  PiT <- data %>%
    filter(pass == 'pass') %>%
    .[items] %>%
    summarise_each(funs(sum)) %>%
    .[] / length(data[[1]])

  Qi <- data %>%
    .[items] %>%
    summarise_each(funs(counts=sum(. == 0,na.rm=TRUE))) %>%
    .[] / length(data[[1]])

  Pt <- data %>%
    summarise(pass_prop = length(which(pass == 'pass'))/length(pass)) %>%
    rep(.,length(items)) %>%
    unlist

  Agree <- (2*PiT) + Qi - Pt

  return(Agree)
}

# Item Phi ---------------------------------------------

item_phi <- function(data, items, cut_score){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  PiT <- data %>%
    filter(pass == 'pass') %>%
    .[items] %>%
    summarise_each(funs(sum)) %>%
    .[] / length(data[[1]])
  
  Qi <- data %>%
    .[items] %>%
    summarise_each(funs(counts=sum(. == 0,na.rm=TRUE))) %>%
    .[]/length(data[[1]])
  
  Pi <- 1 - Qi
  
  Pt <- data %>%
    summarise(pass_prop = length(which(pass == 'pass'))/length(pass)) %>%
    rep(.,length(items)) %>%
    unlist
  
  Qt <- 1 - Pt
  
  Phi <- (PiT - (Pi*Pt)) / (sqrt(Pi * Qi * Pt * Qt))
  
  return(Phi)
  
}
