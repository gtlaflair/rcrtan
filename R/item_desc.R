
#' Calculate item facility
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @return Item_facility Item facility values for test items

IF <- function(data, items){
  
  Item_facility <- data %>%
    dplyr::filter(pass %in% 'pass') %>%
    .[items] %>%
    colMeans()
  
  return(Item_facility)
  
}

#' Calculate item facility for passing students
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Item_facility_pass Item facility values for test items of
#' of test takers who passed the test

IF_pass <- function(data, items, cut_score, scale){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  Item_facility_pass <- data %>%
    dplyr::filter(pass %in% 'pass') %>%
    .[items] %>%
    colMeans()

  return(Item_facility_pass)
}

#' Calculate item facility for failing students
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Item_facility_fail Item facility values for test items of
#' of test takers who failed the test
#' 
IF_fail <- function(data, items, cut_score, scale){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  Item_facility_fail <- data %>%
    dplyr::filter(pass %in% 'fail') %>%
    .[items] %>%
    colMeans()
  
  return(Item_facility_fail)
}

#' Calculate B-index
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Bindex B-index values for items on the test

b_index <- function(data, items, cut_score, scale){
  
  # data$pass <- ifelse(data$Total >= cut_score, 'pass', 'fail') # For testing against Brown (2002)
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$Total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$Total >= cut_score, 'pass', 'fail')
  
  Item_facility_pass <- data %>%
    dplyr::filter(pass %in% 'pass') %>%
    .[items] %>%
    colMeans()
  
  Item_facility_fail <- data %>%
    filter(pass %in% 'fail') %>%
    .[items] %>%
    colMeans()
  
  Bindex <- Item_facility_pass - Item_facility_fail
  
  return(Bindex)
}

#' Calculate Agreement statistic
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Agree Agreement statistic values for items on the test

agree_index <- function(data, items, cut_score, scale){
  
  # data$pass <- ifelse(data$Total >= cut_score, 'pass', 'fail') # For testing against Brown (2002)
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')

  PiT <- data %>%
    dplyr::filter(pass == 'pass') %>%
    .[items] %>%
    dplyr::summarise_each(funs(sum)) %>%
    .[] / length(data[[1]])

  Qi <- data %>%
    .[items] %>%
    dplyr::summarise_each(funs(counts=sum(. == 0,na.rm=TRUE))) %>%
    .[] / length(data[[1]])

  Pt <- data %>%
    dplyr::summarise(pass_prop = length(which(pass == 'pass'))/length(pass)) %>%
    rep(.,length(items)) %>%
    unlist

  Agree <- (2*PiT) + Qi - Pt

  return(Agree)
}

#' Calculate Agreement statistic
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Phi Item Phi values for items on the test

item_phi <- function(data, items, cut_score, scale){
  
  # data$pass <- ifelse(data$Total >= cut_score, 'pass', 'fail') # For testing against Brown (2002)
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  PiT <- data %>%
    dplyr::filter(pass == 'pass') %>%
    .[items] %>%
    dplyr::summarise_each(funs(sum)) %>%
    .[] / length(data[[1]])
  
  Qi <- data %>%
    .[items] %>%
    dplyr::summarise_each(funs(counts=sum(. == 0,na.rm=TRUE))) %>%
    .[]/length(data[[1]])
  
  Pi <- 1 - Qi
  
  Pt <- data %>%
    dplyr::summarise(pass_prop = length(which(pass == 'pass'))/length(pass)) %>%
    rep(.,length(items)) %>%
    unlist
  
  Qt <- 1 - Pt
  
  Phi <- (PiT - (Pi*Pt)) / (sqrt(Pi * Qi * Pt * Qt))
  
  return(Phi)
  
}
