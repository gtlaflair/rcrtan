
#' Calculate item facility
#' 
#' @importFrom magrittr %>%
#' 
#' @export IF_total
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @return Item_facility Item facility values for test items
#' 
#' @examples 
#' IF_total(brown_depend, 2:31)

IF_total <- function(data, items){
  
  Item_facility <- data %>%
    .[items] %>%
    colMeans()
  
  return(Item_facility)
  
}

#' Calculate item facility for passing students
#' 
#' @importFrom magrittr %>% 
#' @importFrom dplyr filter
#' 
#' @export IF_pass
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Item_facility_pass Item facility values for test items of
#' of test takers who passed the test
#' 
#' @examples 
#' IF_pass(brown_depend, 2:31, 21, scale = 'raw')

IF_pass <- function(data, items, cut_score, scale = 'raw'){
  
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

#' Calculate item facility for failing students
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' 
#' @export IF_fail
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Item_facility_fail Item facility values for test items of
#' of test takers who failed the test
#' 
#' @examples 
#' IF_fail(brown_depend, 2:31, 21, scale = 'raw')
IF_fail <- function(data, items, cut_score, scale = 'raw'){
  
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

#' Calculate B-index
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' 
#' @export b_index
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Bindex B-index values for items on the test
#' 
#' @examples 
#' b_index(brown_depend, 2:31, 21, scale = 'raw')

b_index <- function(data, items, cut_score, scale = 'raw'){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  Item_facility_pass <- data %>%
    filter(pass %in% 'pass') %>%
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
#' @importFrom magrittr %>% 
#' @importFrom dplyr filter
#' @importFrom dplyr summarise_all
#' @importFrom dplyr summarise
#' @importFrom dplyr funs
#' 
#' @export agree_stat
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Agree Agreement statistic values for items on the test
#' 
#' @examples 
#' agree_stat(brown_depend, 2:31, 21, scale = 'raw')

agree_stat <- function(data, items, cut_score, scale = 'raw'){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')

  PiT <- data %>%
    filter(pass %in% 'pass') %>%
    .[items] %>%
    summarise_all(funs(sum)) %>%
    .[] / length(data[[1]])

  Qi <- data %>%
    .[items] %>%
    summarise_all(funs(counts=sum(. == 0,na.rm=TRUE))) %>%
    .[] / length(data[[1]])

  Pt <- data %>%
    summarise(pass_prop = length(which(pass %in% 'pass'))/length(pass)) %>%
    rep(.,length(items)) %>%
    unlist

  Agree <- (2 * PiT) + Qi - Pt

  return(Agree)
}

#' Calculate Item Phi
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr summarise_all
#' @importFrom dplyr funs
#' 
#' @export item_phi
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return Phi Item Phi values for items on the test
#' 
#' @examples 
#' item_phi(brown_depend, 2:31, 21, scale = 'raw')

item_phi <- function(data, items, cut_score, scale = 'raw'){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  PiT <- data %>%
    filter(pass %in% 'pass') %>%
    .[items] %>%
    summarise_all(funs(sum)) %>%
    .[] / length(data[[1]])
  
  Qi <- data %>%
    .[items] %>%
    summarise_all(funs(counts=sum(. == 0,na.rm=TRUE))) %>%
    .[]/length(data[[1]])
  
  Pi <- 1 - Qi
  
  Pt <- data %>%
    summarise(pass_prop = length(which(pass %in% 'pass'))/length(pass)) %>%
    rep(.,length(items)) %>%
    unlist
  
  Qt <- 1 - Pt
  
  Phi <- (PiT - (Pi*Pt)) / (sqrt(Pi * Qi * Pt * Qt))
  
  return(Phi)
  
}

#' Calculate item discrimination indices
#' 
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' 
#' @export crt_iteman
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicataing wheter the cut-score
#' is 'raw' (default) or 'percent'
#' @return IF_pass Item facility values for test items for students who
#'     passed the test
#' @return IF_fail Item facility values for test items for students who
#      failed the test
#' @return Item_total Item facility values for test items 
#' @return B_index B-index values for items on the test
#' @return Agree_stat Agreement statistic values for items on the test
#' @return Item_Phi Item Phi values for items on the test
#' 
#' @examples 
#' crt_iteman(brown_depend, 2:31, 21, scale = 'raw')

crt_iteman <- function(data, items, cut_score, scale = 'raw'){

  iteman <- data %>% {
    
    item_fac <- IF_total(., items = items) %>%
      data.frame(.) %>%
      setNames(., 'IF_total')
    
    item_fac_pass <- IF_pass(., items = items, cut_score = cut_score, scale = scale) %>%
      data.frame(.) %>%
      setNames(., 'IF_pass')
    
    item_fac_fail <- IF_fail(., items = items, cut_score = cut_score, scale = scale) %>%
      data.frame(.) %>%
      setNames(., 'IF_fail')
    
    b <- b_index(.,items = items, cut_score = cut_score, scale = scale) %>%
      data.frame(.) %>%
      setNames(., 'B_index')
    
    a <- agree_stat(.,items = items, cut_score = cut_score, scale = scale) %>%
      t() %>%
      data.frame(.) %>%
      setNames(., 'Agree_stat')
    
    p <- item_phi(.,items = items, cut_score = cut_score, scale = scale) %>%
      t() %>%
      data.frame(.) %>%
      setNames(., 'Item_Phi')
    
    res <- bind_cols(item_fac_pass, item_fac_fail, item_fac, b, a, p)
    
    row.names(res) <- names(data[items])
    
    return(res)
  }
  
  return(iteman)
}

globalVariables(c('pass', '.'))