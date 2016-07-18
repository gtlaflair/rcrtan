data2 <- readRDS('data/brown-T_5_1a.rda')

agree_tab <- readRDS('data/subkoviak-agreement-coefficient.rds')
kappa_tab <- readRDS('data/subkoviak-kappa-coefficient.rds')

saveRDS(kappa_tab, 'data/subkoviak-kappa-coefficient')

subkoviak <- function(data, items, raw_cut_score){
  
  c <- raw_cut_score
  
  M <- mean(rowSums(data[items]))
  
  S <- sd(rowSums(data[items]))
  
  z <- (c - .5 - M) / (S)
  
  z_rounded <- round(z, digits = 1)
  
  rel <- round(alpha(data[items], check.keys = FALSE)$total$std.alpha,2)
  
  return(z)
  }
