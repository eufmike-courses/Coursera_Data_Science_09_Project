# This function plot merge dataset from package "Lahman"
# data: data 

library(ggplot2)

battingplot <- function(data, playerID, x, y){
        Batting.playerID <- data[data$playerID == playerID, ]
        p <- ggplot(data, aes_string(x = x, y = y, group = "nameFull")) + 
                geom_line() +
                geom_point() +  
                geom_point(data = Batting.playerID, 
                           aes_string(x = x, y = y), color = "red") + 
                geom_line(data = Batting.playerID, 
                          aes_string(x = x, y = y), color = "red")
        return(p)
}