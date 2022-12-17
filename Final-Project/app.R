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

icons <- awesomeIcons(
  icon = "circle",
  markerColor = "#2b680c"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$style(".well {background-color:#D9E2D0;}"),
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: green}")),

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
                    plotOutput("difficulties"),
                    br(),
                    br()),
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
    facilities <- facilities %>%
      subset(Facility_Type %in% input$facility) %>%
      subset(select = c("FeatureName", "Facility_Type", "Center", "Website", "Capacity", "ADA_Accessible_Facility"))
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
        addAwesomeMarkers(clusterOptions = markerClusterOptions(), 
                   popup = ~paste0("<b>", "Facility Name: ", "</b>", FeatureName),
                   icon=icons)
    })
    
    
    # Plot trail lengths
    
    output$miles <- renderPlot({
      ggplot(trailData(), aes(x=Mileage)) + 
        geom_histogram(binwidth = 1, col = "#D9E2D0") +   # Draw points
        labs(title="Trail Lengths (Miles)") +  
        xlab("Miles\n\n\n\n\n") + # Addline breaks so that plots line up vertically
        ylab("") +
        theme_classic() +
        theme(
          panel.background = element_rect(fill='#D9E2D0'), #transparent panel bg
          plot.background = element_rect(fill='#D9E2D0', color=NA), #transparent plot bg
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(fill='#D9E2D0'), #transparent legend bg
          legend.box.background = element_rect(fill='#D9E2D0'), #transparent legend panel
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 20),
          axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
        ) +
        coord_flip()
    })
    
    # Generate histogram of trail difficulties
    output$difficulties <- renderPlot({
      
      ggplot(trailData(), aes(x = Difficulty)) +
        geom_bar(stat = "count") + 
        labs(title = "Trail Difficulties") +
        ylab("Number of Trails") + 
        theme_classic() +
        theme(
          panel.background = element_rect(fill='#D9E2D0'), #transparent panel bg
          plot.background = element_rect(fill='#D9E2D0', color=NA), #transparent plot bg
          panel.grid.major = element_blank(), #remove major gridlines
          panel.grid.minor = element_blank(), #remove minor gridlines
          legend.background = element_rect(fill='#D9E2D0'), #transparent legend bg
          legend.box.background = element_rect(fill='#D9E2D0'), #transparent legend panel
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 20)
        ) +
        coord_flip()
      
    })

    # Produce data table for download
    output$parks <- DT::renderDataTable(facData(), 
                                        rownames = FALSE, 
                                        options=list(columnDefs = list(list(visible=FALSE, targets=c(6)))) # Hide geometry column
                                        )
    
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
