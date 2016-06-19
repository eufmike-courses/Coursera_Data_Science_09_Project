# This is a simple app for exploring batting record of MLB from 1871 to 2015.
library(dplyr)
library(rCharts)
library(shiny)

# check if data file exist
if ("Batting_Master.csv" %in% list.files("./data/")){
        Batting.data <- read.csv("./data/Batting_Master.csv")
        Batting.data <- tbl_df(Batting.data)
} else {
        # Create a complied batting dataset with the name of player, the birth year
        # (for age calculation) and the cumulative hit record.
        # import data from 1871 to 2015
        
        if ("Master_mod.csv" %in% list.files("./data/")){
                print("Load Master_mod.csv")
                Master_mod <- read.csv("./data/Master_mod.csv")
        } else {
                print("Load Master.csv")
                Master <- read.csv("./data/2015/Master.csv")
                Master_mod <- Master %>% mutate(., nameFull = paste(nameGiven, nameLast))
                print("renaming players")
                source("names.R")
                Master_mod$nameFull <- middle_abbre(FML_name(Master_mod$nameFull))
                write.csv(Master_mod, "./data/Master_mod.csv")
        }
        print("Load Batting.csv")
        Batting <- read.csv("./data/2015/Batting.csv")
        
        print("start data_processing")
        source("data_processing.R")
        Batting.data <- data_processing(Batting, Master_mod)
        write.csv(Batting.data, "./data/Batting_Master.csv")
        print("end data processing")
}


shinyServer(
        function(input, output) {
                output$H_cum <- renderPrint({input$H_cum})
                
                # Create a career hit record follow the given max and min career hit.
                
                batting_data <- reactive({
                        source("careerhit.R")
                        career_hit <- careerhit(Batting.data, input$H_cum[1], input$H_cum[2])
                        Batting.all.total <- Batting.data[Batting.data$playerID %in% career_hit$playerID, ]
                })
                
                output$plot1 <- renderChart2({
                        P1 <- nPlot(H_cum ~ yearID,
                                    data = batting_data(),
                                    group = "nameFull",
                                    type = "lineChart", 
                                    width = 650) 

                        # P1$chart(margin = list(left = 100, right = 100))
                        P1$chart(showLegend = FALSE)
                        P1$xAxis(axisLabel = "Year")
                        P1$yAxis(axisLabel = "Cumulative Hits", width = 62)
                        P1$chart(tooltipContent = "#! function(key, x, y, e){
                                 return '<P><h5><b>Name</b>: ' + e.point.nameFull + '<br>' +
                                 '<b>Year</b>: ' + e.point.yearID + '<br>' +
                                 '<b>Age</b>: ' + e.point.age + '<br>' +
                                 '<b>Seniority</b>: ' + e.point.seniority + '<br>' +
                                 '<b>Hits</b>: ' + e.point.H_cum + '<br>' +
                                 '<b>Batting Average</b>: ' + e.point.battingaverage + '<br>' +
                                 '</h5></P>'} !#")
                        return(P1)
                })
                
                output$plot2 <- renderChart2({
                        P2 <- nPlot(H_cum ~ age,
                                    data = batting_data(),
                                    group = "nameFull",
                                    type = "lineChart",
                                    width = 650)

                        # P2$chart(margin = list(left = 100, right = 100))
                        P2$chart(showLegend = FALSE)
                        P2$xAxis(axisLabel = "Age")
                        P2$yAxis(axisLabel = "Cumulative Hits", width = 62)
                        P2$chart(tooltipContent = "#! function(key, x, y, e){
                                 return '<P><h5><b>Name</b>: ' + e.point.nameFull + '<br>' +
                                 '<b>Year</b>: ' + e.point.yearID + '<br>' +
                                 '<b>Age</b>: ' + e.point.age + '<br>' +
                                 '<b>Seniority</b>: ' + e.point.seniority + '<br>' +
                                 '<b>Hits</b>: ' + e.point.H_cum + '<br>' +
                                 '<b>Batting Average</b>: ' + e.point.battingaverage + '<br>' +
                                 '</h5></P>'} !#")
                        return(P2)

                        })

                output$plot3 <- renderChart2({
                        P3 <- nPlot(H_cum ~ seniority,
                                    data = batting_data(),
                                    group = "nameFull",
                                    type = "lineChart", 
                                    width = 650)

                        # P3$chart(margin = list(left = 100, right = 100))
                        P3$chart(showLegend = FALSE)
                        P3$xAxis(axisLabel = "Seniority")
                        P3$yAxis(axisLabel = "Cumulative Hits", width = 62)
                        P3$chart(tooltipContent = "#! function(key, x, y, e){
                                 return '<P><h5><b>Name</b>: ' + e.point.nameFull + '<br>' +
                                 '<b>Year</b>: ' + e.point.yearID + '<br>' +
                                 '<b>Age</b>: ' + e.point.age + '<br>' +
                                 '<b>Seniority</b>: ' + e.point.seniority + '<br>' +
                                 '<b>Hits</b>: ' + e.point.H_cum + '<br>' +
                                 '<b>Batting Average</b>: ' + e.point.battingaverage + '<br>' +
                                 '</h5></P>'} !#")
                        return(P3)
                })
                
                output$plot4 <- renderChart2({
                        P4 <- nPlot(battingaverage ~ age,
                                    data = batting_data(),
                                    group = "nameFull",
                                    type = "lineChart", 
                                    width = 650)
                        
                        # P4$chart(margin = list(left = 100, right = 100))
                        P4$chart(showLegend = FALSE)
                        P4$xAxis(axisLabel = "Age")
                        P4$yAxis(axisLabel = "Batting Average", width = 62)
                        P4$chart(tooltipContent = "#! function(key, x, y, e){
                                 return '<P><h5><b>Name</b>: ' + e.point.nameFull + '<br>' +
                                 '<b>Year</b>: ' + e.point.yearID + '<br>' +
                                 '<b>Age</b>: ' + e.point.age + '<br>' +
                                 '<b>Seniority</b>: ' + e.point.seniority + '<br>' +
                                 '<b>Hits</b>: ' + e.point.H_cum + '<br>' +
                                 '<b>Batting Average</b>: ' + e.point.battingaverage + '<br>' +
                                 '</h5></P>'} !#")
                        return(P4)
        })
                        
        })
