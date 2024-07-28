library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(DT)

data.all <- read_csv("https://raw.githubusercontent.com/skuiper/NYPD-Labs_2020/master/Data/nypd_arrestBar.csv")

all_year <- sort(unique(data.all$Year))


# UI
ui <- fluidPage(
  
  titlePanel("Get NYPD Data"),
  mainPanel(
    
    selectInput(inputId = "year",
                label = "Year:", 
                choices = c("all", all_year),
                multiple = FALSE,
                selectize = TRUE,
                selected = "2020"),
    
    downloadButton('downloadData', label = "Download Data")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to store filtered data
  filteredData <- reactive({
    req(data.all)  # Ensure the dataset is loaded
    
    # Filter by Group ID
    if (length(input$year) > 0) {
      if ("all" %in% input$year) {
        data <- data.all
      } else {
        data <- data.all[data.all$Year %in% input$year, ]
      }
    } else {
      data <- data.all  # No filtering if no group selected
    }
    
    data
  })
  
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('Data-', Sys.Date(), '.csv', sep="")
    },
    content = function(con) {
      write.csv(filteredData(), con)
    }
  )
}

# Running Shiny App
shinyApp(ui = ui, server = server)