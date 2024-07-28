library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)
library(reshape2)
library(readr)

nypd_arr <- read_csv("https://raw.githubusercontent.com/skuiper/NYPD-Labs_2020/master/Data/nypd_arrestBar.csv")

#Remove an outlier
nypd_arr <- subset(nypd_arr, pct<=200)

nypd <- nypd_arr
nypd <- mutate(nypd, CrimeType = replace(CrimeType, CrimeType == "Tresspass" ,"Trespass"))

####### Custom Colors #######
customColors <- c("#a6cee3", "#1f78b4", "#b2df84", "#33a02c",
                  "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")

####### Force Variables #######
ForceVariables <- c(
  "HandCuff" = "HandCuff",
  "Firearm" = "Firearm",
  "CEW" = "CEW",
  "PepperSpray" = "PepperSpray",
  "Other" = "Other",
  "Verbal" = "Verbal",
  "Summons" = "Summons"
)

####### Race Variables #######
RaceVariables <- c("Black" = "Black",
                   "Hispanic" = "Hispanic",
                   "White" = "White",
                   "Other" = "Other")

####### Gender Variables #######
GenderVariables <- c("Female" = "Female",
                     "Male" = "Male",
                     "Other" = "Other")

####### Crime Type Variables #######
CrimeVariables <- c("Assault" = "Assault",
                    "Trespass" = "Trespass",
                    "Weapon" = "Weapon",
                    "Theft" = "Theft",
                    "Substance" = "Substance",
                    "Other" = "Other")

####### Categorical Variables #######
CatOptions = c("Race" = "Race",
               "Gender" = "Gender",
               "Crime Type" = "CrimeType",
               "Year" = "Year")

####### Quantitative Variables #######
QuanOptions = c("Stopped" = "Stopped",
                "Frisked" = "Frisked",
                "Searched" = "Searched",
                "Arrested" = "Arrested",
                ForceVariables)

#--------------------------------------------------------------------------------------------------------------------#
#                                             User Interface                                            #
#--------------------------------------------------------------------------------------------------------------------#


####### User Interface #######
ui <- fluidPage(
  titlePanel("NYPD Bar Charts"),
  fluidRow(
    column(4, tabsetPanel(
      tabPanel("Axes", wellPanel(
        selectInput("Yaxis", "Y-axis Variable", choices = QuanOptions),
        conditionalPanel(condition = "input.Yaxis == 'Force'",
                         selectizeInput("ModifyForce", 
                                        "Select the Type of Force",
                                        choices = ForceVariables,
                                        selected = ForceVariables,
                                        multiple = TRUE)),
        selectInput("Xaxis", "X-axis Variable", choices = CatOptions),
        radioButtons("YType", label = "Y-axis Measurement", inline = TRUE,
                     choices = c("Counts" = "Counts", 
                                 "Percentage Of Stops" = "Percentage",
                                 "Percentage of Arrests" = "ArrestPercent"),
                     selected = "Counts"),
        sliderInput("Year", "Choose the Years", 2005, 2020, 
                    value = c(2005, 2020), sep = "", animate = TRUE),
        
        selectInput("Facet", "Facet By", choices = c("None", CatOptions)),
        conditionalPanel(condition = "input.Yaxis != 'Force'",
                         selectInput("Color", "Color By", 
                                     choices = c("None", CatOptions))),
        downloadButton("DownloadingData", "Download")
      )),
      
      tabPanel("Filters", wellPanel(
        selectizeInput("ModifyRace",
                       "Filter by Race",
                       choices = RaceVariables,
                       selected = RaceVariables,
                       multiple = TRUE),
        selectizeInput("ModifyGender",
                       "Filter by Gender",
                       choices = GenderVariables,
                       selected = GenderVariables,
                       multiple = TRUE),
        selectizeInput("ModifyCrime",
                       "Filter by Crime Type",
                       choices = CrimeVariables,
                       selected = CrimeVariables,
                       multiple = TRUE)
      ))
    )),
    
    mainPanel(
      h4("Code for Data Cleaning"),
      verbatimTextOutput("filterCode"),
      h4("Code for Data Visualizations"),
      verbatimTextOutput("plotCode"),
      plotOutput("BarChart", height = "500px"))
    
  ))

server <- function(input, output){
  
  FilterNYPD <- function(dataset){
    dataset <- dataset[dataset$Year >= input$Year[1] &
                         dataset$Year <= input$Year[2], ]
    
    dataset <- dataset[dataset$Race %in% input$ModifyRace &
                         dataset$Gender %in% input$ModifyGender &
                         dataset$CrimeType %in% input$ModifyCrime, ]
    
    dataset
  }
  
  ModifyForce <- reactive({
    if (input$YType == "Counts") {
      nypd <- FilterNYPD(nypd)
      
      meltNYPD <- melt(nypd, id.vars = c("pct", CatOptions),
                       measure.vars = input$ModifyForce)
      
      currentData <- data.frame(
        "Year" = meltNYPD$Year,
        "Xvar" = meltNYPD[[input$Xaxis]],
        "variable" = meltNYPD$variable,
        "value" = meltNYPD$value
      )
      
      if (input$Facet != "None") {
        currentData$Facet <- meltNYPD[[input$Facet]]
      }
    } else {
      if (input$YType == "ArrestPercent") {
        nypd <- nypd_arr
      }
      nypd <- FilterNYPD(nypd)
      
      currentData <- data.frame(
        "Year" = nypd$Year,
        "Xvar" = nypd[[input$Xaxis]],
        "Weapon" = nypd$Weapon,
        "HandCuff" = nypd$HandCuff,
        "Firearm" = nypd$Firearm,
        "CEW" = nypd$CEW,
        "PepperSpray" = nypd$PepperSpray,
        "Other" = nypd$Other
      )
      
      if (input$YType == "ArrestPercent") {
        currentData <- currentData %>%
          mutate(DivideBy = nypd$Arrested)
      } else if (input$YType == "Percentage") {
        currentData <- currentData %>%
          mutate(DivideBy = nypd$Stopped)
      }
      
      ddplyVariables <- c("Xvar")
      
      if (input$Facet != "None") {
        currentData$Facet <- nypd[[input$Facet]]
        ddplyVariables <- c(ddplyVariables, "Facet")
      }
      
      currentData <- currentData %>%
        group_by(across(all_of(ddplyVariables))) %>%
        summarise(
          DivideBy = sum(DivideBy),
          Weapon = sum(Weapon) / sum(DivideBy),
          CEW = sum(CEW) / sum(DivideBy),
          HandCuff = sum(HandCuff) / sum(DivideBy),
          Firearm = sum(Firearm) / sum(DivideBy),
          PepperSpray = sum(PepperSpray) / sum(DivideBy),
          Other = sum(Other) / sum(DivideBy),
          .groups = 'drop'
        )
      
      currentData <- melt(currentData, id.vars = ddplyVariables,
                          measure.vars = input$ModifyForce)
    }
    
    currentData
  })
  
prepareCountsData <- reactive({
  nypd <- FilterNYPD(nypd)
  if(input$Color != "None"){
    nypd <- arrange(nypd, nypd[[input$Color]])
  }
  
  if(!is.null(input$ModifyForce) & input$Yaxis == "Force"){
    currentData <- ModifyForce()
  } else{
    currentData <- data.frame("Year" = nypd$Year,
                              "Xvar" = nypd[[input$Xaxis]],
                              "Yvar" = nypd[[input$Yaxis]],
                              "Stopped" = nypd$Stopped,
                              "Arrested" = nypd$Arrested
    )
    
    if(input$Facet != "None"){
      currentData$Facet <- nypd[[input$Facet]]
    }
    
    if(input$Color != "None"){
      currentData$Color <- nypd[[input$Color]]
    }
  }

  # Group by Xvar, Color (if present), and Facet (if present)
  group_vars <- c("Xvar")
  if(input$Color != "None") group_vars <- c(group_vars, "Color")
  if(input$Facet != "None") group_vars <- c(group_vars, "Facet")
  
  currentData <- currentData %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(Yvar = sum(Yvar), .groups = "drop")
  
  currentData
})
  
  library(data.table)
  
  preparePercentData <- reactive({
    if (input$YType == "ArrestPercent") {
      nypd <- nypd_arr
    }
    nypd <- FilterNYPD(nypd)
    if (input$Color != "None") {
      nypd <- arrange(nypd, nypd[[input$Color]])
    }
    
    if (!is.null(input$ModifyForce) & input$Yaxis == "Force") {
      currentData <- ModifyForce()
    } else {
      currentData <- data.frame(
        "Year" = nypd$Year,
        "Xvar" = nypd[[input$Xaxis]],
        "Yvar" = nypd[[input$Yaxis]],
        "Stopped" = nypd$Stopped,
        "Arrested" = nypd$Arrested
      )
      
      if (input$YType == "ArrestPercent") {
        currentData <- mutate(currentData, DivideBy = nypd$Arrested)
      } else if (input$YType == "Percentage") {
        currentData <- mutate(currentData, DivideBy = nypd$Stopped)
      }
      
      group_vars <- c("Xvar")
      
      if (input$Facet != "None") {
        currentData <- mutate(currentData, Facet = nypd[[input$Facet]])
        group_vars <- c(group_vars, "Facet")
      }
      
      if (input$Color != "None") {
        currentData <- mutate(currentData, Color = nypd[[input$Color]])
        group_vars <- c(group_vars, "Color")
      }
      
      currentData <- currentData %>%
        group_by(across(all_of(group_vars))) %>%
        summarise(
          totalYvar = sum(Yvar),
          DivideBy = sum(DivideBy),
          Stopped = sum(Stopped),
          Arrested = sum(Arrested),
          .groups = 'drop'
        )
      
      currentData <- mutate(currentData, Percentage = totalYvar / DivideBy)
      
      if (input$Color != "None") {
        setDT(currentData)
        if (input$YType == "ArrestPercent") {
          currentData[, sum.value := sum(Arrested), by = Xvar]
        } else if (input$YType == "Percentage") {
          currentData[, sum.value := sum(Stopped), by = Xvar]
        }
        
        if (input$Facet == "None") {
          currentData <- mutate(currentData, true_perc = totalYvar / sum.value)
        } else {
          currentData[, sum.value := sum(DivideBy), by = .(Xvar, Facet)]
          currentData <- mutate(currentData, true_perc = totalYvar / sum.value)
        }
        
        currentData <- currentData %>%
          select(-Percentage) %>%
          mutate(
            Percentage = true_perc,
            DivideBy = sum.value
          ) %>%
          select(-true_perc, -sum.value)
      }
    }
    
    currentData
  })
  
  filterCode <- reactive({
    req(input$Year, input$ModifyRace, input$ModifyGender, input$ModifyCrime)
    
    # Initialize code with dataset assignment
    code <- "data <- nypd\n"
    
    # Apply year filter
    code <- paste0(code, sprintf("data <- data[data$Year >= %d & data$Year <= %d, ]\n", input$Year[1], input$Year[2]))
    
    # Apply race filter
    if (length(input$ModifyRace) > 0 && !("All" %in% input$ModifyRace)) {
      code <- paste0(code, sprintf("data <- data[data$Race %%in%% c('%s'), ]\n", paste(input$ModifyRace, collapse = "', '")))
    }
    
    # Apply gender filter
    if (length(input$ModifyGender) > 0 && !("All" %in% input$ModifyGender)) {
      code <- paste0(code, sprintf("data <- data[data$Gender %%in%% c('%s'), ]\n", paste(input$ModifyGender, collapse = "', '")))
    }
    
    # Apply crime filter
    if (length(input$ModifyCrime) > 0 && !("All" %in% input$ModifyCrime)) {
      code <- paste0(code, sprintf("data <- data[data$CrimeType %%in%% c('%s'), ]\n", paste(input$ModifyCrime, collapse = "', '")))
    }
    
    # Select columns based on Xaxis, Yaxis, Color, and Facet
    selected_columns <- unique(c("Year", input$Xaxis, input$Yaxis, 
                                 if(input$Color != "None") input$Color, 
                                 if(input$Facet != "None") input$Facet))
    select_code <- paste("c('", paste(selected_columns, collapse = "', '"), "')", sep = "")
    code <- paste0(code, sprintf("data <- data[, %s, drop = FALSE]\n", select_code))
    
    code
  })
  
  
  # Display the filter code
  output$filterCode <- renderText({
    req(filterCode())
    filterCode()  # Display the dynamically generated R code for data cleaning
  })
  
  output$BarChart <- renderPlot({
    validate(
      if(is.null(input$ModifyForce)){
        "Please select a type of force"
      },
      if(is.null(input$ModifyRace)){
        "Please select at least one race"
      },
      if(is.null(input$ModifyGender)){
        "Please select at least one gender"
      },
      if(is.null(input$ModifyCrime)){
        "Please select at least one crime type"
      }
    )
    
    if(input$YType == "Counts"){
      currentData <- prepareCountsData()
    } else{
      currentData <- preparePercentData()
    }
    
    if(!is.null(input$ModifyForce) & input$Yaxis == "Force"){
      currentPlot <- ggplot(
        data = currentData,
        aes(x = Xvar, y = value, fill = variable)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_colour_manual(values=customColors) +
        xlab("Force")+
        theme_minimal()+
        theme(text = element_text(size = 20))
    } else{
      if(input$YType == "Counts"){
        currentPlot <- ggplot(
          data=currentData,
          aes(x = Xvar, y = Yvar)) +
          theme_minimal() +
          theme(text = element_text(size = 20))
        
        # Add fill aesthetic if Color is selected
        if(input$Color != "None"){
          currentPlot <- currentPlot + 
            aes(fill = Color) +
            scale_colour_manual(values = customColors)
        }
        
        currentPlot <- currentPlot + 
          geom_bar(stat = "identity", position = "stack") +
          xlab(input$Xaxis) + 
          scale_y_continuous(labels = comma) +
          theme(legend.position = "right", axis.title = element_text(size = 18))
        
      } else if (input$YType == "Percentage" | input$YType == "ArrestPercent") {
        #setDT(currentData)[ , sum.value := sum(DivideBy), by = Xvar]
        #currentData <- mutate(currentData, true_perc = totalYvar/sum.value)
        #currentData <- select(currentData, -c(Percentage))
        #currentData <- mutate(currentData, Percentage = true_perc*0.01)
        currentPlot <- ggplot(
          data=currentData,
          aes(x = Xvar, y = Percentage))+
          theme_minimal()+
          theme(text = element_text(size = 20))
      }
      
      currentPlot <- currentPlot + 
        geom_bar(stat = "identity", position="stack") +
        xlab(input$Xaxis) + scale_y_continuous(labels = comma)
      
      ##Color:
      if(input$Color != "None"){
        currentPlot <- currentPlot + aes(fill=Color) +
          theme(legend.position="right", axis.title=element_text(size=18)) +
          scale_colour_manual(values=customColors)
      }
    }
    
    currentPlot <- currentPlot + 
      theme(axis.title=element_text(size=18))
    
    if(input$Xaxis == "Year"){
      currentPlot <- currentPlot + scale_x_continuous(breaks = 2005:2020)
    }
    
    if(input$YType == "Counts"){
      currentPlot <- currentPlot + ylab("Counts")
    } else if(input$YType == "Percentage"){
      currentPlot <- currentPlot + ylab("Percentage of Stops")
    } else if(input$YType == "ArrestPercent"){
      currentPlot <- currentPlot + ylab("Percentage of Arrests")
    }
    
    if(input$YType == "Percentage" | input$YType == "ArrestPercent"){
      currentPlot <- currentPlot +
        scale_y_continuous(labels = percent_format())
    }
    
    # Facets:
    if(input$Facet != "None"){
      currentPlot <- currentPlot + facet_wrap(~Facet, ncol=3) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    currentPlot
  })
  
  plotCode <- reactive({
    validate(
      need(input$ModifyForce, "Please select a type of force"),
      need(input$ModifyRace, "Please select at least one race"),
      need(input$ModifyGender, "Please select at least one gender"),
      need(input$ModifyCrime, "Please select at least one crime type")
    )
    
    # Define yvar based on user input
    yvar <- if (input$Yaxis == "Force") "value" else if (input$YType == "Counts") input$Yaxis else "Percentage"
    
    # Define xvar, colorvar, and facetvar based on user input
    xvar <- input$Xaxis
    colorvar <- if (input$Color != "None") input$Color else NULL
    facetvar <- if (input$Facet != "None") input$Facet else NULL
    
    # Generate ggplot code
    code <- sprintf("ggplot(data = currentData, aes(x = %s, y = %s", xvar, yvar)
    
    if (!is.null(colorvar)) {
      code <- paste0(code, sprintf(", fill = %s", colorvar))
    }
    
    code <- paste0(code, ")) +\n")
    code <- paste0(code, "  geom_bar(stat = 'identity', position = 'stack') +\n")
    
    if (!is.null(colorvar)) {
      code <- paste0(code, "  scale_colour_manual(values = customColors) +\n")
    }
    
    code <- paste0(code, "  theme_minimal() +\n")
    code <- paste0(code, "  theme(text = element_text(size = 20)) +\n")
    code <- paste0(code, "  theme(axis.title = element_text(size = 18)) +\n")
    
    if (xvar == "Year") {
      code <- paste0(code, "  scale_x_continuous(breaks = 2005:2020) +\n")
    }
    
    # Define y-axis label based on user input
    ylabel <- if (input$YType == "Counts") "Counts" else if (input$YType == "Percentage") "Percentage of Stops" else "Percentage of Arrests"
    code <- paste0(code, sprintf("  ylab('%s') +\n", ylabel))
    
    if (input$YType == "Percentage" || input$YType == "ArrestPercent") {
      code <- paste0(code, "  scale_y_continuous(labels = scales::percent_format()) +\n")
    }
    
    if (!is.null(facetvar)) {
      code <- paste0(code, sprintf("  facet_wrap(~ %s, ncol = 3) +\n", facetvar))
      code <- paste0(code, "  theme(axis.text.x = element_text(angle = 45, hjust = 1))\n")
    }
    
    code <- gsub("\\+\n$", "", code) # Remove the trailing '+' and newline
    code
  })
  
  # Display the ggplot code
  output$plotCode <- renderText({
    req(plotCode())
    plotCode()  # Display the dynamically generated R code for plotting
  })
  
  
  
  
  
  #Reactivate Dataset
  downloadFile <- reactive({
    if(input$YType == "Counts"){
      currentData <- prepareCountsData()
    } else{
      currentData <- preparePercentData()
    }
  })
  
  # Downloadable csv of selected dataset ----
  output$DownloadingData <- downloadHandler(
    
    filename = function() {
      paste("currentData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(downloadFile(), file, row.names = FALSE)
    })
}

shinyApp(ui = ui, server = server)
