

# Item facility -------------------------------------------------------


IF <- function(data, items){
  
  Item_facility <- data %>%
    filter(pass %in% 'pass') %>%
    .[items] %>%
    colMeans()
  
  return(Item_facility)
  
}

IF_pass <- function(data, items, cut_score, scale){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  Item_facility_pass <- data %>%
    filter(pass %in% 'pass') %>%
    .[items] %>%
    colMeans()

  return(Item_facility_pass)
}


IF_fail <- function(data, items, cut_score, scale){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  Item_facility_fail <- data %>%
    filter(pass %in% 'fail') %>%
    .[items] %>%
    colMeans()
  
  return(Item_facility_fail)
}

# B-index ---------------------------------------------------
# Checks out with Brown (2002, p. 124)

b_index <- function(data, items, cut_score, scale){
  
  # data$pass <- ifelse(data$Total >= cut_score, 'pass', 'fail') # For testing against Brown (2002)
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$Total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$Total >= cut_score, 'pass', 'fail')
  
  Item_facility_pass <- data %>%
    filter(pass %in% 'pass') %>%
    .[items] %>%
    colMeans()
  
  Item_facility_fail <- data %>%
    filter(pass %in% 'fail') %>%
    .[items] %>%
    colMeans()
  
  return(Item_facility_pass - Item_facility_fail)
}

# Agreement index --------------------------------------------
# Checks out with Brown (2002, p. 124) except for Q4

agree_index <- function(data, items, cut_score, scale){
  
  # data$pass <- ifelse(data$Total >= cut_score, 'pass', 'fail') # For testing against Brown (2002)
  
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
# Slightly off from with Brown (2002, p. 124)

item_phi <- function(data, items, cut_score, scale){
  
  # data$pass <- ifelse(data$Total >= cut_score, 'pass', 'fail') # For testing against Brown (2002)
  
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
