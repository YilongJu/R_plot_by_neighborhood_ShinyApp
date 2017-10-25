# [Load packages] ----
library(rgdal)
library(tidyverse)
library(shiny)
library(lazyeval)
library(reshape2)
library(scales)
library(ggmap)
library(Cairo)
library(maptools)
library(rgeos)
library(scales)
library(RColorBrewer)
library(rsconnect)
library(plotly)
library(RCurl)

# author: "Brooke", "Yilong"
# date: [21 Sep, 2017]

# ====================================================
# In this version, all plots are already drawn


# [Read data] ----
# setwd("/Users/yilongju/Dropbox/Study/GitHub/R_plot_by_neighborhood_ShinyApp")
# data <- read.csv("ABM_censustract_file.csv")
# names(data)[2] <- "BoroCT2000"
# data <- data[order(data$BoroCT2000),]
# data <- data[-1973, ]
# varDef <- read.csv("Variable_Definitions.csv")
# ct2000shp <- readOGR("nyct2000_12c/nyct2000_12c/nyct2000.shp")
# boros <- readOGR("nybb_16a/nybb.shp")

data <- read.csv("data/ABM_censustract_file.csv")
names(data)[2] <- "BoroCT2000"
data <- data[order(data$BoroCT2000),]
data <- data[-1973, ]
varDef <- read.csv("data/Variable_Definitions.csv")
ct2000shp <- readOGR("data/nyct2000_12c/nyct2000_12c/nyct2000.shp")
boros <- readOGR("data/nybb_16a/nybb.shp")

# [Desciption of attributes] ----
beginRow <- 6
varNames <- varDef$varName[beginRow:nrow(varDef) - 1]
varShortNames <- varDef$varShortName[beginRow:nrow(varDef) - 1]
showPercentage <- varDef$showPercentage
varDefinitions <- varDef$varFullDefinition[beginRow:nrow(varDef) - 1]
checkboxGroupListIndex <- setNames(as.list(c(1:length(varNames))), varNames)
checkboxGroupList <- setNames(as.list(varNames), varShortNames)

getwd()
setwd("/Users/yilongju/Dropbox/Study/GitHub/R_plot_by_neighborhood_ShinyApp/data/TestScp")

fileName_in_server <- paste0("~/public_html/SingleVarPlotlyPlots/", varNames[1], ".html")
scp(host = "copenhagen.acasa.upenn.edu",
    path = fileName_in_server,
    keypasswd = "961216",
    user = "yilongju",
    binary = FALSE)




# [Covert real number vector to percentage vector] ----
NumToPercentage <- function(numVec) {
  PercVec <- paste0(round(100 * numVec, 2), "%")
  return(PercVec)
}

OpenLocalHTML <- function(x) {
  readLines(paste0('file://',
                   file.path("data",
                             "SingleVarPlotlyPlots",
                             paste0(x, ".html"))))
}
# [Define server ui] ----
ui <- fluidPage(
  # App title
  titlePanel("VNSNY/UPENN ABMS Study"),
  navlistPanel(
    "Contents",
    tabPanel("ABM Census", h3("Select the attributes and click Draw Plot to exhibit."),
             # Sidebar layout with input and output definitions
             sidebarLayout(
               # Sidebar panel for inputs
               sidebarPanel(
                 # Input: Choose which variables to display
                 checkboxGroupInput(inputId = "displayVariables",
                                    label = h4("Select Variables"),
                                    choices = checkboxGroupList,
                                    selected = 2
                 ),
                 actionButton("Plot", "Draw plot"),
                 verbatimTextOutput("value"),
                 verbatimTextOutput("varLength"),
                 fluidRow("Wait for about 10s for plotting when you select more than 2 attributes."),
                 width = 2
               ),
               # Main panel for displaying outputs
               mainPanel(
                 fluidRow(
                   plotlyOutput(outputId = "GIS", height = 800),
                   width = 8
                 ),
                 fluidRow(
                   htmlOutput("displayVarDef")
                 )
               )
             )
    ),
    tabPanel("Still Working", h3("This is the second panel"),
             # Sidebar layout with input and output definitions
             sidebarLayout(
               
               # Sidebar panel for inputs
               sidebarPanel(
                 
                 # Input: Slider for the number of bins
                 sliderInput(inputId = "bins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               # Main panel for displaying outputs
               mainPanel(
                 
                 # Output: Histogram
                 plotOutput(outputId = "distPlot")
               )
             )
    ),
    widths = c(2,10)
  )
)

# [Define server logic] ----
server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
  # Set up a variable to control the drawing
  v <- reactiveValues(doPlot = FALSE)
  # Detect whether the button is clicked
  observeEvent(input$Plot, {
    v$doPlot <- input$Plot
  })
  
  output$value <- renderPrint({ input$displayVariables })
  # output$selectedVars <- renderPrint({ colNames[input$displayVariables] })
  output$varLength <- renderPrint({ length(input$displayVariables) })
  output$displayVarDef <- renderUI({
    HTML(paste("<b>", varDef[which(varDef$varName %in% input$displayVariables), "varFullDefinition"], "</b>", collapse = "</br>"))
  })
  
  plotGIS <- reactive({
    # If the button is not clicked, do not draw
    if (v$doPlot == FALSE) {return()}
    
    # Isolate the plot code, maintain the old plot until the button is clicked again
    isolate({
      input_VarLen <- length(input$displayVariables)
      input_varNames <- input$displayVariables
      # input_VarLen <- 1
      # input_varNames <- c("povrate")

      if (input_VarLen == 1) {
        varName <- input_varNames[1]
        varIdx <- checkboxGroupListIndex[[varName]]
        result_plotly <- GetPlotlyPlot(varIdx)
        return(result_plotly)
      } else {
        return(plotly_empty())
      }
      
    })
    # Make sure it closes when we exit this reactive, even if there's an error
  })
  
  output$GIS <- renderPlotly({plotGIS()})
  
  
  
}

shinyApp(ui = ui, server = server)