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
library(dplyr)
library(ggplot2)
library(ggalt)
library(DT)
library(sf)
library(leaflet)
library(leaflet.extras)

# Read in data

trails <- st_read("./Allegheny_County_Trails_Locations.geojson")
facilities <- st_read("Allegheny_County_Park_Features.geojson")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Allegheny County Trails"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("length",
                        "Maximum Trail Length:",
                        min = ceiling(min(trails$Mileage, na.rm = TRUE)),
                        max = ceiling(max(trails$Mileage, na.rm = TRUE)),
                        value = 3
                        ),
            checkboxGroupInput("diff",
                               "Trail Difficulty",
                               choices = c("Easy", "Easy-Moderate", "Moderate", "Moderate-Difficult", "Difficult", "NA"),
                               selected = c("Easy", "Easy-Moderate", "Moderate", "Moderate-Difficult", "Difficult")),
            selectInput("facility",
                        "Facility Type",
                        choices = sort(unique(facilities$Facility_Type)),
                        selected = "Shelter",
                        multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          # Using Shiny JS
          shinyjs::useShinyjs(),
          # Style the background and change the page
          tags$style(type =  "text/css", 
                      ".leaflet {height: 600px !important;}
                      body {background-color: #D9E2D0;}"),
          
          tabsetPanel(
            
           tabPanel("Trail Map", 
                    leafletOutput("map"),
                    br(),
                    plotOutput("miles"),
                    plotOutput("difficulties")),
           tabPanel("Data", 
                    DT::dataTableOutput("parks"),
                    downloadButton(outputId = "dlButton", label = "Download Data"))
        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # Trail Filtered data
  trailData <- reactive({
    newTrails <- trails
    
    # Mileage
    newTrails <- filter(newTrails, Mileage <= input$length)

    # Difficulty    
    if(length(input$diff) > 0){
      newTrails <- subset(newTrails, Difficulty %in% input$diff) 
    }

    return(newTrails)
  })
  
  
  
  # Trail subset for plotting
  topTen <- reactive({
    newTrails <- trails
    
    # Mileage
    newTrails <- filter(newTrails, Mileage <= input$length)
    
    # Difficulty    
    if(length(input$diff) > 0){
      newTrails <- subset(newTrails, Difficulty %in% input$diff) 
    }
    
    # Get head
    newTrails <- newTrails %>%
      drop_na(Trail_Name) %>%
      filter(Trail_Name != "") %>%
      head(n = 10)
    
    return(newTrails)
  })
  
  
  # Facility Filtered Data
  facData <- reactive(({
    facilities <- subset(facilities, Facility_Type %in% input$facility)
    return(facilities)
  }))
  
  # Add a basemap
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("OpenStreetMap.HOT") %>%
        setView(-79.9, 40.45, 10)
    })
    
  # Add trail layer
    observe({
      newTrail <- trailData()
      
      leafletProxy("map", data = newTrail) %>%
        clearGroup(group = "newTrail") %>%
        addPolylines(group = "newTrail", color = "#ee5c42", popup = ~paste0("<b>", "Trail Name: ", "</b>", Trail_Name))
    })
    
    # Add facilities layer
    observe({
      facs <- facData()
      
      leafletProxy("map", data = facs) %>%
        clearMarkers() %>%
        addMarkers(clusterOptions = markerClusterOptions(), popup = ~paste0("<b>", "Facility Name: ", "</b>", Name))
    })
    
    # Generate histogram of trail difficulties
    output$difficulties <- renderPlot({
      
      ggplot(trailData(), aes(x = Difficulty)) +
        geom_bar(stat = "count") + 
        theme_classic()
      
    })
      
    # Plot trail lengths

    output$miles <- renderPlot({
      ggplot(trailData(), aes(x=Mileage)) + 
      geom_histogram(binwidth = 1) +   # Draw points
      labs(title="Trail Lengths (Miles)") +  
      theme_classic() +
      coord_flip()
    })
    
    # Produce data table for download
    output$parks <- DT::renderDataTable(facData())
    
    # Add download button functionality
    output$dlButton <- downloadHandler(
      filename = "Facility_Data.csv",
      content = function(file) {
        write.csv(facData(), file)
      }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
