library(shiny)
library(janitor)
library(tidyr)
library(stringr)
library(httr)
library(RCurl)
library(lubridate)
library(readxl)
library(gt)
library(reshape2)
library(ggplot2)
library(purrr)
library(moderndive)
library(fs)
library(infer)
library(googlesheets4)
library(scales)
library(TeachBayes)
library(sf)
library(shiny)
library(hexbin)
library(dplyr)
library(httr)
library(jsonlite)
library(tidyverse)

nba_stats <- read_csv("nba_stats_clean.csv") %>% clean_names()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Point Sources by Season"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "type", label = strong("Season"),
                        choices = unique(nba_stats$season),
                        selected = "1981")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        point_source  <- nba_stats %>% 
            group_by(season) %>% 
            summarise(total_2points = sum(x2p * 2), total_3points = sum(x3p * 3), total_ft = sum(ft)) %>% 
            pivot_longer(-season, names_to = "source", values_to = "count") %>% 
            filter(season == input$type)

        # draw the histogram with the specified number of bins
        ggplot(point_source, aes(source, count)) + geom_bar(stat="identity", width = 0.5, fill="tomato2")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
