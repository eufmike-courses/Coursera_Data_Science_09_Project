library(Lahman)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(rCharts)

# check if data file exist
if ("Batting_Master.csv" %in% list.files("./data/")){
        Batting.data <- read.csv("./data/Batting_Master.csv")    
        Batting.data <- tbl_df(Batting.data)
} else {
        # Create a complied batting dataset with the name of player, the birth year
        # (for age calculation) and the cumulative hit record.
        # import data from 1871 to 2015
        Master <- read.csv("./data/2015/Master.csv")
        Master <- Master %>% mutate(., nameFull = paste(nameGiven, nameLast))
        
        source("names.R")
        Master$nameFull <- middle_abbre(FML_name(Master$nameFull))
        
        Batting <- read.csv("./data/2015/Batting.csv")
        
        source("data_processing.R")
        Batting.data <- data_processing(Batting, Master)
        write.csv(Batting.data, "./data/Batting_Master.csv")
        
}



# Create a career hit record follow the given max and min career hit.
source("careerhit.R")
career_hit <- careerhit(Batting.data, 2800) 

Batting.all.total <- Batting.data[Batting.data$playerID %in% career_hit$playerID, ]

# merge hitting result in the same year
source("remove_dup.R")
batting_data <- remove_dup(Batting.all.total)

# length(unique(batting_data$playerID))
# length(unique(Batting.all.total$playerID))
source("battingplot.R")
p1 <- battingplot(batting_data, "suzukic01", "yearID", "H_cum")
p1
p2 <- battingplot(batting_data, "suzukic01", "age", "H_cum")
p2
p3 <- battingplot(batting_data, "suzukic01", "seniority", "H_cum")
p3
p4 <- battingplot(batting_data, "suzukic01", "AB_cum", "H_cum")
p4

write.csv(batting_data, "./data/Batting.data.csv")

# P1 <- nPlot(H_cum ~ yearID, 
#             data = batting_data, 
#             group = "nameFull",
#             type = "lineChart")
# 
# P1$xAxis(axisLabel = "Year")
# P1$yAxis(axisLabel = "Cumulative Hits", width = 62)
# P1$chart(showLegend = FALSE)
# P1
# 
# P2 <- nPlot(H_cum ~ age, 
#             data = batting_data, 
#             group = "nameFull",
#             type = "lineChart")
# 
# P2$xAxis(axisLabel = "Age")
# P2$yAxis(axisLabel = "Cumulative Hits", width = 62)
# P2$chart(showLegend = FALSE)
# P2
# 
# P3 <- nPlot(H_cum ~ seniority, 
#             data = batting_data, 
#             group = "nameFull",
#             type = "lineChart")
# 
# P3$xAxis(axisLabel = "Seniority in MLB")
# P3$yAxis(axisLabel = "Cumulative Hits", width = 62)
# P3$chart(showLegend = FALSE)
# # P3$wrap(H_cum ~ seniority, 
# #          data = batting_data[batting_data$playerID == "suzukic01", ], 
# #          type = 'lineChart')
# 
# P3
# P4 <- nPlot(H_cum ~ AB_cum, 
#             data = batting_data, 
#             group = "nameFull",
#             type = "lineChart")
# 
# P4$xAxis(axisLabel = "Cumulative number at bat")
# P4$yAxis(axisLabel = "Cumulative Hits", width = 62)
# P4$chart(showLegend = FALSE)
# P4