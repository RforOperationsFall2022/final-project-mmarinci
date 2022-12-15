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
library(wesanderson)

# Read in data files adn concatenate budgets for years 2012-2022
ob2022 <- read.csv(file = "2022-amended-operating.csv")
ob2021 <- read.csv(file = "2021-amended-operating.csv")
ob2020 <- read.csv(file = "2020-operating.csv")
ob2019 <- read.csv(file = "2019-operating.csv")
ob2018 <- read.csv(file = "2018-operating.csv")
ob2017 <- read.csv(file = "2017-operating.csv")
ob2016 <- read.csv(file = "2016-operating.csv")
ob2015 <- read.csv(file = "2015-operating.csv")
ob2014 <- read.csv(file = "2014-operating.csv")
ob2013 <- read.csv(file = "2013-operating.csv")
ob2012 <- read.csv(file = "2012-operating.csv")

budget <- rbind(ob2022, ob2021, ob2020, ob2019, ob2018, ob2017, ob2016, ob2015)

# Filter and clean data
data1 <- budget %>% 
  filter(Type == "Expenditure") %>%
  select(Year, Department, Amount)

data1$Department <- data1$Department %>%
  replace(data1$Department == "PS - ADMIN AND SUPPORT SERVICE", "ADMIN AND SUPPORT SERVICES") %>%
  replace(data1$Department == "PS - ADMIN AND SUPPORT SERVICES", "ADMIN AND SUPPORT SERVICES") %>%
  replace(data1$Department == "BUREAU OF NEIGHBORHOOD EMPOWER", "BUREAU OF NEIGHBORHOOD EMPOWERMENT") %>%
  replace(data1$Department == "OFFICE OF MANAGEMENT AND BUDGE", "OFFICE OF MANAGEMENT AND BUDGET") %>%
  replace(data1$Department == "OFFICE OF MUNICIPAL INVESTIGAT", "OFFICE OF MUNICIPAL INVESTIGATIONS") %>%
  replace(data1$Department == "PERMITS LICENSES AND INSPECTIO", "PERMITS LICENSES AND INSPECTIONS") %>%
  replace(data1$Department == "PS - BBI", "BUREAU OF BUILDING INSPECTION") %>%
  replace(data1$Department == "PS - BUREAU OF ANIMAL CARE AND", "BUREAU OF ANIMAL CARE AND CONTROL") %>%
  replace(data1$Department == "PS - BUREAU OF ANIMAL CARE AND CONTROL", "BUREAU OF ANIMAL CARE AND CONTROL") %>%
  replace(data1$Department == "PS - EMERGENCY MED SERVICES", "EMERGENCY MED SERVICES") %>%
  replace(data1$Department == "PS - FIRE BUREAU", "FIRE BUREAU") %>%
  replace(data1$Department == "PS - POLICE BUREAU", "POLICE BUREAU") %>%
  replace(data1$Department == "PW- BUREAU OF ADMINISTRATION", "BUREAU OF ADMINISTRATION") %>%
  replace(data1$Department == "PW- BUREAU OF PW OPERATIONS", "BUREAU OF PW OPERATIONS") %>%
  replace(data1$Department == "PW- ENVIRONMENTAL SERVICES", "ENVIRONMENTAL SERVICES") %>%
  replace(data1$Department == "PW - BUREAU OF ADMINISTRATION", "BUREAU OF ADMINISTRATION") %>%
  replace(data1$Department == "PW - BUREAU OF FACILITIES", "BUREAU OF FACILITIES") %>%
  replace(data1$Department == "PW - BUREAU OF PW OPERATIONS", "BUREAU OF PW OPERATIONS") %>%
  replace(data1$Department == "PW - ENGINEERING & CONSTRUCTIO", "ENGINEERING AND CONSTRUCTION") %>%
  replace(data1$Department == "PW - ENVIRONMENTAL SERVICES", "ENVIRONMENTAL SERVICES")

data <- data1 %>%
  group_by(Year, Department) %>%
  summarize(Total = sum(Amount)) %>%
  arrange(Department) %>%
  group_by(Year) %>%
  mutate(Percentage = round(Total / sum(Total) * 100, 1))

# Create a wide version of the data
data_wide <- data %>%
  pivot_wider(names_from = Year, values_from = c(Total, Percentage)) %>%
  arrange(Percentage_2022)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Operating Budget for Pittsburgh 2012-2022"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # Select a year of data to show ----------------------------------
          selectInput(inputId = "year", 
                      label = "Choose a budget year:",
                      choices = c(2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012), 
                      selected = "2022"),
        
          # Select a department to show ----------------------------------
          checkboxGroupInput(inputId = "selected_dept",
                           label = "Select department(s):",
                           choices = unique(data$Department),
                           selected = c(data_wide$Department[19:29]))
          
          # Create a download button---------------------------------------
          
        
        ),

        # Show plots, data table and download button
        mainPanel(
           plotOutput("deptBarplot"),
           br(),
           br(),
           br(),
           plotOutput("linechart"),
           br(),
           br(),
           br(),
           plotOutput("piechart"),
           br(),
           br(),
           br(),
           DT::dataTableOutput(outputId = "budgetTable"),
           downloadButton(outputId = "dlButton", label = "Download Data"),
           br(),
           br()
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  year_subset <- reactive({
    req(input$year, input$selected_dept)
    filter(data, Year %in% input$year & Department %in% input$selected_dept)
  })
  
  lineplot_subset <- reactive({
    req(input$selected_dept)
    filter(data, Department %in% input$selected_dept)
  })

  # Generate a barplot--------------------------------------------------------  
  output$deptBarplot <- renderPlot({
      ggplot(year_subset(), aes(Department, Total)) +
        geom_col(fill="lightblue") +
        labs(x=NULL,
             y=NULL,
             title = "Budget by Department and Year") +
        theme(axis.text.x = element_text(angle = 90, size = 10)) +
        scale_y_continuous(labels = scales::comma) + 
        ylab("Total Budget ($)") +
        theme(plot.title = element_text(hjust=0.5, face="bold", size = 20),
            plot.background=element_rect(fill="#FFFFFF"),
            panel.background=element_rect(fill="#FFFFFF"),
            panel.grid.minor=element_blank(),
            panel.grid.major.y=element_blank(),
            panel.grid.major.x=element_line(),
            panel.border=element_blank()
        )
        })

    # Generate a line chart----------------------------------------------------
    output$linechart <- renderPlot({  
      ggplot(data = lineplot_subset(), aes(x=Year, y=Total, group=Department, color=Department)) + 
        geom_line() +
        geom_point() +
        scale_y_continuous(labels = scales::comma) + 
        labs(x=NULL,
             y=NULL,
             title = "Budget by Department, 2012-2022") +
        theme(plot.title = element_text(hjust=0.5, face="bold", size = 20),
              plot.background=element_rect(fill="#FFFFFF"),
              panel.background=element_rect(fill="#FFFFFF"),
              panel.grid.major.y=element_line(color = "lightgrey"),
              panel.border=element_blank()
        )
          })
    
    # Generate a pie chart----------------------------------------------------
    # https://r-graph-gallery.com/piechart-ggplot2.html
    output$piechart <- renderPlot({  
      ggplot(year_subset(), aes(x = '', y = Total, fill = Department)) +
        geom_bar(width = 1, stat = "identity", color = "white") +
        coord_polar("y", start = 0) +
        labs(x=NULL,
             y=NULL,
             title = "Total Budget for Selected Departments and Year") +
        scale_y_continuous(labels = scales::comma) + 
        scale_fill_manual(values = wes_palette("Darjeeling2", 30, type = "continuous")) +
        theme_void() +
        theme(plot.title = element_text(hjust=0.5, face="bold", size = 20))
    })
    
    # Print data table----------------------------------------------------------
    output$budgetTable <- DT::renderDataTable(
        DT::datatable(data = lineplot_subset(), 
                      options = list(pageLength = 10), 
                      rownames = FALSE)
    )
  
    # Add download button functionality
    output$dlButton <- downloadHandler(
      filename = "Budget_Data.csv",
      content = function(file) {
        write.csv(lineplot_subset(), file)
      }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
