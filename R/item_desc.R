
#' Calculate item facility
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr summarise_all
#' 
#' @export if_total
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @return Item_facility Item facility values for test items
#' 
#' @examples 
#' if_total(bh_depend, 2:31)

if_total <- function(data, items){
  
  Item_facility <- data %>%
    select(., items) %>%
    summarise_all(funs(mean)) %>%
    t(.) %>%
    as_data_frame(.) %>%
    setNames(., 'if_total')
  
  return(Item_facility)
  
}

#' Calculate item facility for passing students
#' 
#' @importFrom stats setNames
#' @importFrom magrittr %>% 
#' @importFrom dplyr filter
#' @importFrom dplyr summarise_all
#' @export if_pass
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicating whether the cut-score
#' is 'raw' (default) or 'percent'
#' @return Item_facility_pass Item facility values for test items of
#' of test takers who passed the test
#' 
#' @examples 
#' if_pass(bh_depend, 2:31, 21, scale = 'raw')

if_pass <- function(data, items, cut_score, scale = 'raw'){
  
  data$raw_total <- data %>%
    select(., items) %>%
    rowSums(.)
  
  data$perc_total <- data %>%
    select(., items) %>% {
      (rowSums(.) / length(items)) * 100
    }
  
  if (scale == 'percent') {
    data$total <- data$perc_total
  }
  
  if (scale == 'raw') {
    data$total <- data$raw_total
  }
  
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  Item_facility_pass <- data %>%
    filter(pass %in% 'pass') %>%
    select(., items) %>%
    summarise_all(funs(mean)) %>%
    t(.) %>%
    as_data_frame(.) %>%
    setNames(., 'if_pass')
    

  return(Item_facility_pass)
}

#' Calculate item facility for failing students
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr summarise_all
#' 
#' @export if_fail
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicating whether the cut-score
#' is 'raw' (default) or 'percent'
#' @return Item_facility_fail Item facility values for test items of
#' of test takers who failed the test
#' 
#' @examples 
#' if_fail(bh_depend, 2:31, 21, scale = 'raw')
if_fail <- function(data, items, cut_score, scale = 'raw'){
  
  data$raw_total <- data %>%
    select(., items) %>%
    rowSums(.)
  
  data$perc_total <- data %>%
    select(., items) %>% {
      (rowSums(.) / length(items)) * 100
    }
  
  if (scale == 'percent') {
    data$total <- data$perc_total
  }
  
  if (scale == 'raw') {
    data$total <- data$raw_total
  }
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  Item_facility_fail <- data %>%
    filter(pass %in% 'fail') %>%
    select(., items) %>%
    summarise_all(funs(mean)) %>%
    t(.) %>%
    as_data_frame(.) %>%
    setNames(., 'if_fail')
  
  return(Item_facility_fail)
}

#' Calculate B-index
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom tibble as_data_frame
#' @importFrom dplyr summarise_all
#' 
#' @export b_index
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicating whether the cut-score
#' is 'raw' (default) or 'percent'
#' @return Bindex B-index values for items on the test
#' 
#' @examples 
#' b_index(bh_depend, 2:31, 21, scale = 'raw')

b_index <- function(data, items, cut_score, scale = 'raw'){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  Item_facility_pass <- data %>%
    filter(pass %in% 'pass') %>%
    select(., items) %>%
    summarise_all(., mean)
  
  Item_facility_fail <- data %>%
    filter(pass %in% 'fail') %>%
    select(., items) %>%
    summarise_all(., mean)
  
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
#' @param scale A character vector indicating whether the cut-score
#' is 'raw' (default) or 'percent'
#' @return Agree Agreement statistic values for items on the test
#' 
#' @examples 
#' agree_stat(bh_depend, 2:31, 21, scale = 'raw')

agree_stat <- function(data, items, cut_score, scale = 'raw'){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')

  PiT <- data %>%
    filter(pass %in% 'pass') %>%
    select(., items) %>%
    summarise_all(funs(sum)) %>%
    .[] / length(data[[1]])

  Qi <- data %>%
    select(., items) %>%
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
#' @param scale A character vector indicating whether the cut-score
#' is 'raw' (default) or 'percent'
#' @return Phi Item Phi values for items on the test
#' 
#' @examples 
#' item_phi(bh_depend, 2:31, 21, scale = 'raw')

item_phi <- function(data, items, cut_score, scale = 'raw'){
  
  data$raw_total <- rowSums(data[items])
  data$perc_total <- (rowSums(data[items]) / length(items)) * 100
  
  ifelse(scale == 'percent', data$total <- data$perc_total, data$total <- data$raw_total)
  
  data$pass <- ifelse(data$total >= cut_score, 'pass', 'fail')
  
  PiT <- data %>%
    filter(pass %in% 'pass') %>%
    select(., items) %>%
    summarise_all(funs(sum)) %>%
    .[] / length(data[[1]])
  
  Qi <- data %>%
    select(., items) %>%
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

#' Calculate criterion-referenced item discrimination indices
#' 
#' @importFrom stats setNames
#' @importFrom magrittr %>%
#' @importFrom tibble as_data_frame
#' 
#' @export crt_iteman
#' 
#' @param data A data frame of dichotomously scored test times
#' @param items Raw column indices representing the test items
#' @param cut_score A raw or percentage cut-score
#' @param scale A character vector indicating whether the cut-score
#' is 'raw' (default) or 'percent'
#' @return \code{if_pass} contains item facility values for test items for students who 
#' passed the test
#' @return \code{if_fail} contains item facility values for test items for students who did not pass the test
#' @return \code{if_total} contains item facility values for test items 
#' @return \code{b_index} contains b-index values for items on the test
#' @return \code{agree_stat} contains agreement statistic values for items on the test
#' @return \code{item_phi} contains item phi values for items on the test
#' 
#' @examples 
#' crt_iteman(bh_depend, 2:31, 21, scale = 'raw')

crt_iteman <- function(data, items, cut_score, scale = 'raw'){

  iteman <- data %>% {
    
    item_fac <- if_total(., items = items) %>%
      as_data_frame(.) %>%
      setNames(., 'if_total')
    
    item_fac_pass <- if_pass(., items = items, cut_score = cut_score, scale = scale) %>%
      as_data_frame(.) %>%
      setNames(., 'if_pass')
    
    item_fac_fail <- if_fail(., items = items, cut_score = cut_score, scale = scale) %>%
      as_data_frame(.) %>%
      setNames(., 'if_fail')
    
    b <- b_index(.,items = items, cut_score = cut_score, scale = scale) %>%
      t() %>%
      as_data_frame(.) %>%
      setNames(., 'b_index')
    
    a <- agree_stat(.,items = items, cut_score = cut_score, scale = scale) %>%
      t() %>%
      as_data_frame(.) %>%
      setNames(., 'agree_stat')
    
    p <- item_phi(.,items = items, cut_score = cut_score, scale = scale) %>%
      t() %>%
      as_data_frame(.) %>%
      setNames(., 'item_phi')
    
    res <- bind_cols(item_fac_pass, item_fac_fail, item_fac, b, a, p)
    
    res$items <- names(data[items])
    
    res <- res %>%
      dplyr::tbl_df(.)
    
    return(res)
  }
  
  return(iteman)
}

globalVariables(c('pass', '.', 'total'))