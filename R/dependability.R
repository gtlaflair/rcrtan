#' Calculate Subkoviak's (1988) single administration consistency indices
#' 
#' @param data A data frame of dichotomously scored test items
#' @param items Raw column indices representing the test items or the index of a column with the total score
#' @param n_items Number of items on the test (needed if data does not contain item-level information)
#' @param raw_cut_score The raw cut-score for the test
#' @param look_up If TRUE, the agreement and kappa tables from Subkoviak (1988) are returned with the results
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test and rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test and rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test and rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr summarise_at
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr bind_cols
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom purrrlyr by_row
#' @importFrom stats sd
#' @importFrom stats var
#' @importFrom tidyr gather
#' 
#' @export subkoviak
#' 
#' @examples 
#' subkoviak(data = brown_depend, items = 2:31, raw_cut_score = 21)

subkoviak <- function(data, items, n_items = NULL, raw_cut_score, look_up = FALSE){
  
  c <- raw_cut_score 
  
  if(is.character(items) == TRUE){
    M <- data %>%
      select(., items) %$%
      mean(.[[items]])
    
    S <- data %>%
      select(., items) %$%
      sd(.[[items]])
    
    kr21 <- (n_items / (n_items - 1)) * (1 - ((M * (n_items - M)) / (n_items * (S^2)))) 
  
  }
  
  if(is.character(items) == FALSE){
    M <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      mean(total)
    
    S <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      sd(total)
    
    sigma_y <- data %>%
      summarise_at(., items, var) %>%
      gather(., key, value) %>%
      summarise(., sigma_y = sum(value))
    
    K <- length(items)
    
    sigma_x <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      var(total)
    
    kr20 <- (K / (K - 1)) * (1 - (sigma_y / sigma_x)) 
  }
  
  if(is.character(items) == TRUE){
    rel <- kr21
  }
  
  if(is.character(items) == FALSE){
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

#' Calculate Brennan's short-cut estimate for domain score dependability 
#' 
#' @param data A data frame of dichotomously scored test items
#' @param items Raw column indices representing the test items
#' @param n_items Number of items on the test (needed if data does not contain item-level information)
#' @return The \code{phi} estimate for domain score dependability
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom stats sd
#' @importFrom stats var
#' 
#' @export phi_domain
#' 
#' @examples 
#' phi_domain(brown_depend, 2:31)
phi_domain <- function(data, items, n_items = NULL){
  
  n <- length(data[[1]])
  
  if(is.character(items) == FALSE) {
  
    k <- length(items)
    
    mp <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      mean(total)/k 
  
    sp <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      (sd(total)/k)^2
  
    M <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      mean(total)
  
    S <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      sd(total)
  
    sigma_y <- data %>%
      summarise_at(., items, var) %>%
      gather(., key, value) %>%
      summarise(., sigma_y = sum(value))
  
    sigma_x <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      var(total)
  
    rel <- (k / (k - 1)) * (1 - (sigma_y / sigma_x)) # kr-20
  
    phi <- round((((n * sp)/(n - 1)) * rel)/(((n * sp)/(n - 1))+((mp * (1 - mp) - sp)/(k - 1))), 2) %>%
      as_data_frame(.) %>%
      setNames(., 'Dependability Phi')
  
  }
  
  if(is.character(items) == TRUE){
    
    k <- n_items
    
    mp <- data %>%
      select(., items) %$%
      mean(.[[items]]) / k
    
    sp <- data %>%
      select(., items) %$%
      (sd(.[[items]]) / k)^2
    
    M <- data %>%
      select(., items) %$%
      mean(.[[items]])
    
    S <- data %>%
      select(., items) %$%
      sd(.[[items]])
    
    rel <- (n_items / (n_items - 1)) * (1 - ((M * (n_items - M)) / (n_items * (S^2)))) # kr-21
    
    phi <- round((((n * sp)/(n - 1)) * rel)/(((n * sp)/(n - 1))+((mp * (1 - mp) - sp)/(k - 1))), 2) %>%
      as_data_frame(.) %>%
      setNames(., 'Domain Phi')
    
  }
  
  return(phi)
  
}


#' Calculate Brennan's short-cut estimate for phi lambda
#' 
#' @param data A data frame of dichotomously scored test items
#' @param scores Column name of raw test scores
#' @param cut_score Cut-score of the test expressed as a proportion (e.g., 0.70)
#' @param n_items Number of items on the test (needed if data does not contain item-level information)
#' @return The phi lambda estimate for dependability
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom stats sd
#' 
#' @export phi_lambda
#' 
#' @examples 
#' phi_lambda(data = brown_item, n_items = 100, scores = "Total", cut_score = 0.70)
phi_lambda <- function(data, n_items, scores, cut_score){
  k <- n_items
  lambda <- cut_score
  n_persons <- length(data[[1]])
  lambda <- cut_score
  
  mp <- data %>%
    select(., scores) %$%
    mean(.[[scores]]) / k
  
  sp <- data %>%
    select(., scores) %$%
    (sd(.[[scores]]) / k)^2
  
  phi <- 1 - ((1 / (k - 1)) * ((mp * (1 - mp) - sp) / ((mp - lambda)^2 + sp))) %>%
    as_data_frame(.) %>%
    setNames(., 'Phi Lambda')
  
  return(phi)

}


globalVariables(c('pass', '.', 'key', 'value', 'total', 'raw_alpha', 'sub_agree_coef', 'sub_kappa_coef'))