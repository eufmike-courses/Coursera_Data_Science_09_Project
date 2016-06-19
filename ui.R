library(shiny)
library(rCharts)
shinyUI(fluidPage(
        h4("Records for Hits in MLB"), 
        h5("Players with all time hits over 2800"),
        sidebarPanel(
                sliderInput("H_cum", "Hits:",
                            min = 2800, max = 4500, value = c(3500, 4500))
        ),
        
    mainPanel(
                # h4("Cumulative Hits"),
                # verbatimTextOutput("H_cum"),
                h5('Information can be checked by hovering over each point', 
                   align ="center"),
                
                h4('Hits by Year', align = "center"),
                showOutput("plot1", "nvd3"),
                h4('Hits by Age', align = "center"),
                showOutput("plot2", "nvd3"),
                h4('Hits by seniority in MLB', align = "center"),
                showOutput("plot3", "nvd3"),
                h4('Batting average by age', align = "center"),
                showOutput("plot4", "nvd3")
                )
))
