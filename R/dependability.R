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
#' @import dplyr
#' @import tidyr
#' 
#' @export subkoviak
#' 
#' @examples 
#' subkoviak(brown_depend, 2:31, 21)

subkoviak <- function(data, items, raw_cut_score, look_up = FALSE){
  
  data_temp <- data[items]
  
  c <- raw_cut_score <- 21
  
  M <- mean(rowSums(data[items]))
  
  S <- stats::sd(rowSums(data[items]))
  
  z <- (c - .5 - M) / (S)
  
  z_cut <- round(z, digits = 2)
  
  z_cut_rounded <- round(z, digits = 1)
  
  rel <- round(psych::alpha(as.matrix(data[items]), check.keys = FALSE, warnings = FALSE)$total$std.alpha,1) %>%
    as.character()
  
  agree_coef <- subkoviak_data$agree_tab %>%
    dplyr::select(z, contains(rel)) %>%
    dplyr::filter(z %in% z_cut_rounded) %>%
    .[[2]] %>%
    round(., 2)
  
  kappa_coef <- subkoviak_data$kappa_tab %>%
    dplyr::select(z, contains(rel)) %>%
    dplyr::filter(z %in% z_cut_rounded) %>%
    .[[2]] %>%
    round(., 2)
    
  
  subkoviak_consistency <- c('z' = z_cut, 'z_rounded' = z_cut_rounded, 'alpha' = as.numeric(rel),
                             'agree_coef' = agree_coef, 'kappa_coef' = kappa_coef) 
  
  ifelse(look_up == TRUE, return(list(subkoviak_consistency, subkoviak_data$agree_tab, subkoviak_data$kappa_tab)),
         return(subkoviak_consistency))

  }
