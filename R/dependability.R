#' Calculate Subkoviak's (1988) single administration consistency indices
#' 
#' @param data A data frame of dichotomously scored test items
#' @param items Raw column indices representing the test items
#' @param raw_cut_score The raw cut-score for the test
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test and rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test and rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables
#' @return The \code{z_cut} score and the rounded \code{z_cut_rounded} score for the test and rounded values for the \code{agree_coef} (agreement) and \code{kappa_coef} (kappa) coefficients from Subkoviak's (1988) tables

subkoviak <- function(data, items, raw_cut_score){
  
  c <- raw_cut_score
  
  M <- mean(rowSums(data[items]))
  
  S <- sd(rowSums(data[items]))
  
  z <- (c - .5 - M) / (S)
  
  z_cut <- round(z, digits = 2)
  
  z_cut_rounded <- round(z, digits = 1)
  
  rel <- round(alpha(data[items], check.keys = FALSE, warnings = FALSE)$total$std.alpha,1) %>%
    as.character()
  
  agree_coef <- subkoviak_data$agree_tab %>%
    select(z, contains(rel)) %>%
    filter(z %in% z_rounded) %>%
    .[[2]] %>%
    round(., 2)
  
  kappa_coef <- subkoviak_data$kappa_tab %>%
    select(z, contains(rel)) %>%
    filter(z %in% z_rounded) %>%
    .[[2]] %>%
    round(., 2)
    
  
  subkoviak_consistency <- c('z' = Z_cut, 'z_rounded' = z_cut_rounded, 
                             'agree_coef' = agree_coef, 'kappa_coef' = kappa_coef) 
  return(subkoviak_consistency)
  }
