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
#' @importFrom magrittr %>%
#' @importFrom magrittr %$%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr contains
#' @importFrom dplyr bind_cols
#' @importFrom stats sd
#' @importFrom purrrlyr by_row
#' 
#' @export subkoviak
#' 
#' @examples 
#' subkoviak(data = brown_depend, items = 2:31, raw_cut_score = 21)

subkoviak <- function(data, items, n_items = NULL, raw_cut_score, look_up = FALSE){
  
  c <- raw_cut_score 
  
  if(length(items) < 2 & !is.null(n_items)){
    M <- data %>%
      select(., items) %$%
      mean(.[[1]])
  
  }
  
  if(length(items) > 1 & is.null(n_items)){
    M <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      mean(total)
  }
  
  if(length(items) < 2 & !is.null(n_items)){
    S <- data %>%
      select(., items) %$%
      sd(.[[1]])
    
  }
  
  if(length(items) > 1 & is.null(n_items)){
    S <- data %>%
      select(., items) %>%
      by_row(., sum, .collate = 'rows', .to = 'total') %$%
      sd(total)
  }
  
  z <- (c - .5 - M) / (S)
  
  z_cut <- round(abs(z), digits = 2)
  
  z_cut_rounded <- ifelse(abs(z) <= 2.0, round(z, digits = 1), 2) %>%
    abs()
  
  if(length(items) < 2 & !is.null(n_items)){
    rel <- (n_items / (n_items - 1)) * (1 - ((M * (n_items - M)) / (n_items * (S^2))))
  }
  
  
  if(length(items) > 1 & is.null(n_items)){
    rel <- data %>%
      select(., items)%>%
      as.matrix(.) %>%
      psych::alpha(., check.keys = FALSE, warnings = FALSE) %$%
      total %$%
      round(raw_alpha, 1) %>%
      as.character(.)
  }
  
  agree_coef <- sub_agree_coef %>%
    filter(., z %in% z_cut_rounded) %>%
    select(., contains(rel)) 
 
  kappa_coef <- sub_kappa_coef %>%
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
  
  mp <- data %>%
    select(., items) %>%
    by_row(., sum, .collate = 'rows', .to = 'total') %$%
    mean(total)/k 
  
  sp <- data %>%
    select(., items) %>%
    by_row(., sum, .collate = 'rows', .to = 'total') %$%
    (sd(total)/k)^2
  
  rel <- data %>%
    select(., items) %>%
    as.matrix(.) %>%
    psych::alpha(., check.keys = FALSE, warnings = FALSE) %$%
    total %$%
    raw_alpha
  
  phi <- round((((n * sp)/(n - 1)) * rel)/(((n * sp)/(n - 1))+((mp * (1 - mp) - sp)/(k - 1))), 2) %>%
    as_data_frame(.) %>%
    setNames(., 'Dependability Phi')
  
  return(phi)
  
}




globalVariables(c('pass', '.', 'total', 'raw_alpha', 'sub_agree_coef', 'sub_kappa_coef'))