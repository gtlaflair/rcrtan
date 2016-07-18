# data2 <- readRDS('data/brown-T_5_1a.rda')

# agree_tab <- readRDS('data/subkoviak-agreement-coefficient.rds')
# kappa_tab <- readRDS('data/subkoviak-kappa-coefficient.rds')

# subkoviak_data <- lst(agree_tab, kappa_tab)

subkoviak <- function(data, items, raw_cut_score){
  
  c <- raw_cut_score
  
  M <- mean(rowSums(data[items]))
  
  S <- sd(rowSums(data[items]))
  
  z <- (c - .5 - M) / (S)
  
  z_round_2 <- round(z, digits = 2)
  
  z_rounded <- round(z, digits = 1)
  
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
    
  
  subkoviak_consistency <- c('z' = z_round_2, 'z_rounded' = z_rounded, 
                             'agree_coef' = agree_coef, 'kappa_coef' = kappa_coef) 
  return(subkoviak_consistency)
  }
