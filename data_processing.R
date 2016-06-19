## This function 
## 1. take variables of "playerID", "birthYear", "nameFirst", 
## "nameLast", "nameGiven" from Master data sheet and merge with 
## batting data; 
## 2. generate cumulative hit across year for each players; 
## 3. create three variables including batting average, age and seniority in MLB. 

library(dplyr)

data_processing <- function(batting_data = Batting, master_data = Master){
        # combine data year of birth with batting data sheet
        Master.year <- master_data[, c("playerID", "birthYear", "nameFirst", "nameLast", "nameGiven", "nameFull")]
        Batting.data <- merge(batting_data, Master.year)
        Batting.data <- tbl_df(Batting.data)
        Batting.data <- Batting.data %>% arrange(., yearID)
        
        # Generate cumulative hit data
        # This dataset is big, so one progress bar is embeded.
        print("Generating variables: H_cum, AB_cum and firstyear")
        playerID_list <- unique(Batting.data$playerID)
        pb <- txtProgressBar(min = 0, max = length(playerID_list), style = 3)
        
        x <- 0
        for (i in playerID_list){
                Batting.data$H_cum[Batting.data$playerID == i] <- 
                        cumsum(Batting.data$H[Batting.data$playerID == i])
                Batting.data$AB_cum[Batting.data$playerID == i] <- 
                        cumsum(Batting.data$AB[Batting.data$playerID == i])
                Batting.data$firstyear[Batting.data$playerID == i] <- 
                        min(Batting.data$yearID[Batting.data$playerID == i])
                x <- x + 1
                setTxtProgressBar(pb, x)
        }
        print("Variables generating completed")
        
        # create three variables: 
        # batting average ("battingaverage"), 
        # age ("age"),   
        # and seniority in MLB ("seniority") 
        print("Generating variables: battingaverage, age and seniority")
        Batting.2 <- Batting.data %>% 
                mutate(., battingaverage = H/AB) %>%
                mutate(., age = yearID - birthYear) %>%
                mutate(., seniority = yearID - firstyear + 1 )
        print("Variables generating completed")
        
        # remove duplication
        print("remove duplication")
        source("remove_dup.R")
        Batting.2 <- remove_dup(Batting.2)
        print("end remove duplication")
        
        print("start round up")
        Batting.2$battingaverage <- round(Batting.2$battingaverage, digits = 3)
        print("end round up")
        
        return(Batting.2)
}