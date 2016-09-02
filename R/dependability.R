#' Calculate Subkoviak's (1988) single administration consistency indices
#' 
#' @param data A data frame of dichotomously scored test items
#' @param items Raw column indices representing the test items
#' @param raw_cut_score The raw cut-score for the test
#' @param look_up If TRUE, the agreement and kappa tables from Subkoviak (1988) are returned with the results
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test and rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test and rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test and rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr bind_cols
#' @importFrom stats sd
#' 
#' @export subkoviak
#' 
#' @examples 
#' subkoviak(brown_depend, 2:31, 21)

subkoviak <- function(data, items, raw_cut_score, look_up = FALSE){
  
  data_temp <- data[items]
  
  c <- raw_cut_score 
  
  M <- data %>%
    .[items] %>%
    purrr::by_row(., sum, .collate = 'rows', .to = 'total') %$%
    mean(total)
  
  S <- data %>%
    .[items] %>%
    purrr::by_row(., sum, .collate = 'rows', .to = 'total') %$%
    sd(total)
  
  z <- (c - .5 - M) / (S)
  
  z_cut <- round(z, digits = 2)
  
  z_cut_rounded <- round(z, digits = 1)
  
  rel <- data %>%
    .[items]%>%
    as.matrix(.) %>%
    psych::alpha(., check.keys = FALSE, warnings = FALSE) %$%
    total %$%
    round(raw_alpha, 1) %>%
    as.character(.)
  
  agree_coef <- subkoviak_data$sub_agree_coef %>%
    filter(., z %in% z_cut_rounded) %>%
    select(., contains(rel)) 
 
  kappa_coef <- subkoviak_data$sub_kappa_coef %>%
    filter(., z %in% z_cut_rounded) %>%
    select(.,contains(rel))
    
  subkoviak_consistency <- c('z' = z_cut, 'z_rounded' = z_cut_rounded, 'raw_alpha' = as.numeric(rel), 'agree_coef' = agree_coef, 'kappa_coef' = kappa_coef) %>%
    data.frame(.)
  
  if(look_up == TRUE) {
    
  subk <- list(subkoviak_consistency, subkoviak_data$sub_agree_coef, subkoviak_data$sub_kappa_coef)
  
  }
  
  if(look_up == FALSE) {
    
    subk <- subkoviak_consistency
    
  }
 
  return(subk)

}

#' Calculate Brown's short-cut estimate for domain score dependability 
#' 
#' @param data A data frame of dichotomously scored test items
#' @param items Raw column indices representing the test items
#' @return The \code{phi} estimate for domain score dependability
#' 
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom stats sd
#' 
#' @export short_phi
#' 
#' @examples 
#' short_phi(brown_depend, 2:31)
short_phi <- function(data, items){
  
  n <- length(data[[1]])
  
  k <- length(items)
  
  Mp <- data %>%
    .[items] %>%
    purrr::by_row(., sum, .collate = 'rows', .to = 'total') %$%
    mean(total)/k 
  
  Sp <- data %>%
    .[items] %>%
    purrr::by_row(., sum, .collate = 'rows', .to = 'total') %$%
    (sd(total)/k)^2
  
  rel <- data %>%
    .[items]%>%
    as.matrix(.) %>%
    psych::alpha(., check.keys = FALSE, warnings = FALSE) %$%
    total %$%
    raw_alpha
  
  phi <- round((((n*Sp)/(n-1))*rel)/(((n*Sp)/(n-1))+((Mp*(1-Mp)-Sp)/(k-1))), 2) %>%
    data.frame(.) %>%
    setNames(., 'Dependability Phi')
  
  return(phi)
  
}




globalVariables(c('pass', '.', 'total', 'raw_alpha'))