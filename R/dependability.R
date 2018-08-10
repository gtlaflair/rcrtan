#' Calculate Subkoviak's (1988) single administration consistency indices
#' 
#' @param data A data frame of dichotomously scored test items
#' @param items Raw column indices representing the test items or 
#' number of items on the test (see Details)
#' @param total Total score column of the test (see Details)
#' @param raw_cut_score The raw cut-score for the test
#' @param look_up If TRUE, the agreement and kappa tables from Subkoviak (1988) are returned with the results
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test 
#' @return The estimated alpha coefficient. K-R21 is used when there is no item-level information.
#' Otherwise, K-R20 is used.
#' @return The rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr summarise_at
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr bind_cols
#' @importFrom magrittr %>%
#' @importFrom purrrlyr by_row
#' @importFrom stats sd
#' @importFrom stats var
#' @importFrom tidyr gather
#' 
#' @export subkoviak
#' 
#' @details When the item-level information is available, Kuder-Richardson 20 is used
#' as an estimate of alpha. If only the total scores on the test are available and the
#' number of items is known, Kuder-Richardson 21 is used as an estimate of alpha.
#' 
#' @examples 
#' subkoviak(data = bh_depend, items = 2:31, raw_cut_score = 21)

subkoviak <- function(data, items, raw_cut_score, total = NULL, look_up = FALSE){
  
  c <- raw_cut_score

  if(!is.null(total)){
    colnames(data)[which(colnames(data) == total)] <- "total"
  }
  
  if(length(items) == 1 & is.null(total)){
    stop("If you do not have item-level data, you need to fill the 'total' argument with the name of a column that contains the total raw scores.")
  }
  
  if(length(items) == 1){
    n_items <- items
  }
  
  if(length(items) == 1){
    M <- data %>%
      # select(., total) %$%
      summarise(m = mean(total)) %>%
      as.numeric()
    
    S <- data %>%
      # select(., total) %$%
      summarise(s = sd(total)) %>%
      as.numeric()
    
    kr21 <- (n_items / (n_items - 1)) * (1 - ((M * (n_items - M)) / (n_items * (S^2)))) 

  }
  
  if(length(items) > 1){
    M <- data %>%
      dplyr::select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %>%
      summarise(m = mean(total)) %>%
      as.numeric()
    
    S <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %>%
      summarise(s = sd(total)) %>%
      as.numeric()
    
    sigma_y <- data %>%
      summarise_at(., items, var) %>%
      gather(., key, value) %>%
      summarise(., sigma_y = sum(value))
    
    K <- length(items)
    
    sigma_x <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %>%
      summarise(v = var(total)) %>%
      as.numeric()
    
    kr20 <- (K / (K - 1)) * (1 - (sigma_y / sigma_x)) 
  }
  
  if(length(items) == 1){
    rel <- kr21
  }
  
  if(length(items) > 1){
    rel <- kr20
  }
  
  rel_rounded <- ifelse(round(rel, 1) < 1, round(rel, 1), 0.9)
  
  rel_rounded <- as.character(rel_rounded)
  
  z <- (c - .5 - M) / (S)
  
  z_cut <- round(abs(z), digits = 2)
  
  z_cut_rounded <- ifelse(abs(z) <= 2.0, round(z, digits = 1), 2) %>%
    abs()
  
  agree_coef <- sub_agree_coef %>%
    filter(., z %in% z_cut_rounded) %>%
    select(., contains(rel_rounded)) 
 
  kappa_coef <- sub_kappa_coef %>%
    filter(., z %in% z_cut_rounded) %>%
    select(.,contains(rel_rounded))
    
  subkoviak_consistency <- c('z' = z_cut, 'z_rounded' = z_cut_rounded, 'KR_est' = as.numeric(rel), 'agree_coef' = agree_coef, 'kappa_coef' = kappa_coef) %>%
    data.frame(.)
  
  if(look_up == TRUE) {
    
  subk <- list(subkoviak_consistency, subkoviak_data$sub_agree_coef, subkoviak_data$sub_kappa_coef)
  
  }
  
  if(look_up == FALSE) {
    
    subk <- subkoviak_consistency
    
  }
 
  return(subk)

}

#' Calculate Brown's (1990) short-cut estimate for phi dependability 
#' 
#' @param data A data frame of dichotomously scored test items
#' @param items Raw column indices representing the test items or 
#' number of items on the test (see Details).
#' @param total Total score column name of the test (see Details)
#' @return The \code{phi} estimate for domain score dependability.
#' 
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @importFrom stats var
#' 
#' @export phi_domain
#' 
#' @details When the item-level information is available, Kuder-Richardson 20 is used
#' as an estimate of alpha. If only the total scores on the test are available and the
#' number of items is known, Kuder-Richardson 21 is used as an estimate of alpha.
#' 
#' @examples 
#' phi_domain(bh_depend, 2:31)
phi_domain <- function(data, items, total = NULL){
  

  
  n <- length(data[[1]])

  if(!is.null(total)){
    colnames(data)[which(colnames(data) == total)] <- "total"
  }
  
  if(length(items) == 1 & is.null(total)){
    stop("If you do not have item-level data, you need to fill the 'total' argument with the name of a column that contains the total raw scores.")
  }
  
  if(length(items) == 1){
    n_items <- items
  }
  
  if(length(items) > 1) {
  
    k <- length(items)
    
    mp <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %>%
      summarise(m = mean(total) / k) %>%
      as.numeric()
  
    sp2 <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %>%
      summarise(s_p = (sd_pop(total, n = length(total))/k)^2) %>%
      as.numeric()
  
    sigma_y <- data %>%
      summarise_at(., items, var) %>%
      gather(., key, value) %>%
      summarise(s = sum(value)) %>%
      as.numeric()
  
    sigma_x <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %>%
      summarise(v = var(total)) %>%
      as.numeric()
  
    rel <- (k / (k - 1)) * (1 - (sigma_y / sigma_x)) # kr-20
  
    phi <- (((n * sp2)/(n - 1)) * rel)/((((n * sp2)/(n - 1)) * rel) + ((mp * (1 - mp) - sp2)/(k - 1))) %>%
      as_data_frame(.) %>%
      setNames(., 'Domain Phi')
  
  }
  
  if(length(items) == 1){
    
    k <- items
    
    mp <- data %>%
      # select(., total) %$%
      summarise(mean_p = mean(total) / k) %>%
      as.numeric()
    
    sp2 <- data %>%
      # select(., items) %>%
      # by_row(., sum, .collate = 'rows', .to = 'total') %>%
      summarise(s_p = (sd_pop(total, n = length(total))/k)^2) %>%
      as.numeric()
    
    M <- data %>%
      # select(., total) %$%
      summarise(mean = mean(total)) %>%
      as.numeric()
    
    S <- data %>%
      # select(., total) %$%
      summarise(sd = sd(total)) %>%
      as.numeric()
    
    rel <- (k / (k - 1)) * (1 - ((M * (k - M)) / (k * (S^2)))) # kr-21
    
    phi <- (((n * sp2)/(n - 1)) * rel)/((((n * sp2)/(n - 1)) * rel) + ((mp * (1 - mp) - sp2)/(k - 1))) %>%
      as_data_frame(.) %>%
      setNames(., 'Domain Phi')
    
  }
  
  return(phi)
  
}


#' Calculate Brennan's (1984) estimate for phi lambda
#' 
#' @param data A data frame of dichotomously scored test items
#' @param total Column name of raw test scores.
#' @param cut_score Cut-score of the test expressed as a proportion (e.g., 0.70)
#' @param items Raw column indices representing the test items or 
#' number of items on the test
#' 
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @importFrom purrrlyr by_row
#' 
#' @export phi_lambda
#' 
#' @examples 
#' phi_lambda(data = bh_item, items = 100, total = "Total", cut_score = 0.70)
phi_lambda <- function(data, items, cut_score, total = NULL){

  lambda <- cut_score
  n_persons <- length(data[[1]])

  if(is.null(total)){
    data <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total')
  }
  
  if(!is.null(total)){
    colnames(data)[which(colnames(data) == total)] <- "total"
  }
  
  if(length(items) > 1){
    k <- length(items)
  }
  
  if(length(items) == 1){
    k <- items
  }
  
  mp <- data %>%
    # select(., scores) %>%
    summarise(mean = mean(total) / k) %>%
    as.numeric()
  
  sp2 <- data %>%
    # select(., scores) %$%
    summarise(sd_pop = (sd_pop(total, length(total)) / k) ^2) %>%
    as.numeric()
  
  phi <- 1 - ((1 / (k - 1)) * ((mp * (1 - mp) - sp2) / ((mp - lambda)^2 + sp2))) %>%
    as_data_frame(.) %>%
    setNames(., 'Phi Lambda')
  
  return(phi)

}

#' Calculate standard deviation for the population
#' 
#' @param x A vector of total scores from a dichotomously score test.
#' @param n The number of people who took the test
#' 
#' @export sd_pop
#' 
#' @examples 
#' sd_pop(bh_item$Total, nrow(bh_item))

sd_pop <- function(x, n){
  sdpop <- sqrt(sum((x - mean(x))^2) / n)
  return(sdpop)
}


globalVariables(c('pass', '.', 'key', 'value', 'total', 'raw_alpha', 'sub_agree_coef', 'sub_kappa_coef'))