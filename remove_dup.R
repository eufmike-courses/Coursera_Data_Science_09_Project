# remove the year when player transfer between multiple teams

remove_dup <- function(data){
        data.new <- data.frame()
        # create the unique player list
        playerlist <- unique(data$playerID)
        
        # Embed a progress bar
        pb <- txtProgressBar(min = 0, max = length(playerlist), style = 3)
        
        x <- 0
        
        for (i in playerlist){
                
                # extract the data of each player
                data_player <- data[data$playerID == i, ]
                # create the table indicating the transfer year.
                transfer_frequency <- data.frame(table(data_player$yearID))
                
                # The year with freq > 1 is the year of transfering during the season
                transfering_year <- transfer_frequency[transfer_frequency$Freq >1, "Var1"]
                
                if (length(transfering_year) == 0){
                        x <- x + 1
                        data.new <- rbind(data.new, data_player)
                        next # skip current for loop if there is no transfer happened during the season.
                } else {
                
                        # Assuming a player transfering during the season happened multiple time in his career,
                        # this loop can resolve the issue. 
                        vec <- c()
                        for (j in transfering_year){
                                D <- which(data_player$yearID == j)
                                E <- D[D != max(D)] 
                                # E is the location of data before transfering in specified year
                                vec <- c(vec, E) # put those location into a vector
                        }
                data_player <- data_player[-vec, ] # remove those data
                data.new <- rbind(data.new, data_player) # store the data into a new data frame
                }
                x <- x + 1
                setTxtProgressBar(pb, x)
        }
        return(data.new)
        
        }

