#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rsconnect)
library(tools)
library(tidyverse)
library(ggplot2)
library(ggalt)
library(DT)
library(sf)
library(leaflet)
library(leaflet.extras)

# Read in data

trails <- st_read("./Allegheny_County_Trails/Parks_PARKS_OWNER_Trails.shp")
attractions <- st_read("Allegheny_County_Park_Features.geojson")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Allegheny County Trails"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("length",
                        "Trail Length:",
                        min = min(trails$Mileage, na.rm = TRUE),
                        max = max(trails$Mileage, na.rm = TRUE),
                        value = 3),
            checkboxGroupInput("diff",
                               "Trail Difficulty",
                               choices = unique(trails$Difficulty),
                               selected = unique(trails$Difficulty)),
            selectInput("facility",
                        "Facility Type",
                        choices = unique(attractions$Facility_Type))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tabPanel("Trail Map", 
                    leafletOutput("map"),
                    plotOutput("miles"),
                    plotOutput("difficulties")),
           tabPanel("Data", DT::dataTableOutput("parks"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
