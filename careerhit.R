## This function 
## 1. takes the processed batting data and generate a list of career hits.
## 2. filter out the players according to their career hits.

library(dplyr)

careerhit <- function(data, min = 0, max = 10000){
        data %>%
        group_by(., playerID) %>%
        summarise(., total_hit = sum(H, na.rm = T)) %>%
        filter(., total_hit > min & total_hit < max)
}
        
        