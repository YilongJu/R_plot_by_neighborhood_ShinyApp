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
library(crosstalk)
library(doParallel)
library(leaflet)
library(OneR)
library(Hmisc)
library(moments)
library(colorspace)

# author: "Yilong", "Brooke"
# date: [21 Sep, 2017]

# [Function Declarations] ----
GetBinforVar <- function(data, varName, nbins = 6) {
  # data <- uCT
  # varName <- "subacc"
  # varName <- "propoa"
  numericValues <- unlist(c(data[, varName]), use.names = FALSE)
  nbins_t = nbins
  l <- 0
  iter = 0
  while (l < nbins && iter < 20) {
    bins <- cut2(x = numericValues, g = nbins_t,
                 levels.mean = T, onlycuts = T, digits = 2)
    l <- length(bins)
    nbins_t <- nbins_t + 1
    iter <- iter + 1
  }
  bins <- signif(bins, 3)
  return(bins)
}
GetRadius <- function(varValue, l = 2, u = 32) {
  range <- range(varValue, na.rm = TRUE)
  varValue <- (varValue - range[1])/(range[2] - range[1]) * (u - l) + l
  return(varValue)
}
ShowColors <- function(col, border = "light gray", ...) {
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "")
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}
# Covert real number vector to percentage vector
NumToPercentage <- function(numVec) {
  PercVec <- paste0(round(100 * numVec, 2), "%")
  return(PercVec)
}

# [Read data] ----
# setwd("/Users/yilongju/Dropbox/Study/GitHub/R_plot_by_neighborhood_ShinyApp")

data <- read.csv("data/ABM_censustract_precinct_111617.csv")
names(data)[2] <- "BoroCT2000"
data <- data[order(data$BoroCT2000),]
data <- data[-1973, ]
data_precinct <- data %>% select(precpop:offpcap)
varDef <- read.csv("data/Variable_Definitions.csv")
ct2000shp <- readOGR("data/nyct2000_12c/nyct2000_12c/nyct2000.shp")
boros <- readOGR("data/nybb_16a/nybb.shp")
ny.map <- readOGR("data/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp", layer="ZillowNeighborhoods-NY")
nypp <- readOGR("data/nypp_17c_police_precinct_shapefile/nypp.shp")

# Reproject ct data, to be consistent with ny neighbor data
ct2000shp <- spTransform(ct2000shp, ny.map@proj4string)
boros <- spTransform(boros, ny.map@proj4string)
nypp <- spTransform(nypp, ny.map@proj4string)

# [Desciption of attributes] ----
beginRow <- 6
endCol <- 4
varNames <- as.character(varDef$varName[beginRow:nrow(varDef) - 1])
varShortNames <- as.character(varDef$varShortName[beginRow:nrow(varDef) - 1])
showPercentage <- varDef$showPercentage[beginRow:nrow(varDef) - 1]
varDefinitions <- varDef$varFullDefinition[beginRow:nrow(varDef) - 1]
checkboxGroupListIndex <- setNames(as.list(c(1:length(varNames))), varNames)
checkboxGroupList <- setNames(as.list(as.character(varNames)), as.character(varShortNames))
shapeDataList <- setNames(as.list(c("CT", "NB")), c("Census Tract Map", "Neighborhood Map"))
varColors <- rainbow_hcl(length(varNames), c = 190, l = 60, start = 12, end = 300)
# ShowColors(varColors)
# varColors <- terrain.colors(length(varNames))
# varColors <- cm.colors(length(varNames))
# varColors <- rainbow_hcl(length(varNames), start = 60, end = 240)
# varColors <- diverge_hcl(length(varNames),
#                          h = c(800, 300), c = 100, l = c(20, 130), power = 0.4)
# ShowColors(varColors)

# [Prepare precinct shape data] ----
nypp@data$id <- rownames(nypp@data)
f_nypp <- fortify(nypp, polyname = "Precinct")
nypp_DF <- merge(f_nypp, nypp@data, by = "id")

# [Prepare Boros shape data] ----
#   add to data a new column termed "id" composed of the rownames of data
#   create a data.frame from our spatial object
#   merge the "fortified" data with the data from our spatial object
#   aggregate to an upper level
#   offset the label
boros@data$id <- rownames(boros@data)
f_boros <- fortify(boros, polyname = "BoroCode")
boros_DF <- merge(f_boros, boros@data, by = "id")
bnames <- aggregate(data = boros_DF, cbind(long,lat) ~ BoroName, FUN=function(x) mean(range(x)))
# bnames[4,3] <- 200741.5
bnames$BoroName <- as.character(bnames$BoroName)
bnames$BoroName <- as.factor(bnames$BoroName)

# [Prepare ct shape data] ----
#   add to data a new column termed "id" composed of the rownames of data'
#   create a data.frame from our spatial object
#   merge the "fortified" data with the data from our spatial object
ct2000shp@data$id <- rownames(ct2000shp@data)
f_ct2000shp <- fortify(ct2000shp, polyname = "BoroCT2000")
ct2000shp_DF <- merge(f_ct2000shp, ct2000shp@data, by = "id")

# [Prepare ny neighborhood shape data, <by Brooke>] ----
#   project the dataframe onto the shape file
#   add to data a new column termed "id" composed of the rownames of data
#   create a data.frame from our spatial object
#   merge the "fortified" data with the data from our spatial object
sodo <- ny.map[ny.map$City == "New York", ]
dat <- data.frame(Longitude = data$ctrdlong, Latitude = data$ctrdlat)
coordinates(dat) <- ~ Longitude + Latitude
proj4string(dat) <- proj4string(sodo)
location = over(dat, sodo)
data = cbind(data, location)
dataProjected <- sodo
dataProjected@data$id <- rownames(dataProjected@data)
watershedPoints <- fortify(dataProjected, region = "id")
watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")

# [Prepare useful data] ----
data_ids <- data %>% select(BoroCT2000, Name)
data_vars <- data %>% select(popdens:propnonw)
data_coords <- data[, c("ctrdlong", "ctrdlat")]
data_necessary <- cbind(data_ids, data_vars, data_coords)
# --- For CT
uCT <- data_necessary %>%
  group_by(BoroCT2000) %>%
  summarise_all(funs(mean))
ct2000shp_DF$BoroCT2000 <- as.character(ct2000shp_DF$BoroCT2000)
uCT$BoroCT2000 <- as.character(uCT$BoroCT2000)
dfCT <- dplyr::left_join(ct2000shp_DF, uCT, by = "BoroCT2000")
#   --- Find center of view
center_ct.map <- ct2000shp_DF %>%
  select(long, lat) %>%
  summarise(ctrlong = mean(long), ctrlat = mean(lat))
center_ct.map # -73.91271 40.69984
#   --- Combine shapefile with data
ct2000shp_attr <- ct2000shp
ct2000shp_attr@data <- dplyr::left_join(ct2000shp_attr@data, uCT, by = "BoroCT2000")

# --- For NB
uNB <- data_necessary %>%
  group_by(Name) %>%
  summarise_all(funs(mean))
dfNB <- dplyr::left_join(watershedDF, uNB, by = "Name")
#   --- Find center of view
center_ny.map <- watershedDF %>%
  select(long, lat) %>%
  summarise(ctrlong = mean(long), ctrlat = mean(lat))
center_ny.map # -73.92194 40.68922
#   --- Combine shapefile with data
ny.map_attr <- ny.map
ny.map_attr@data <- dplyr::left_join(ny.map_attr@data, uNB, by = "Name")

data_necessary$BoroCT2000 <- as.character(data_necessary$BoroCT2000)
NBname_CTntaname <- full_join(data_necessary, ct2000shp_DF, by = "BoroCT2000")
NBname_CTntaname <- NBname_CTntaname %>%
  distinct(Name, NTANAme) %>%
  arrange(Name)
NBname_CTntaname <- aggregate(NTANAme ~ Name, data = NBname_CTntaname,
                              FUN = paste0, collapse = "<br/>")

ny.map_attr@data <- left_join(ny.map_attr@data, NBname_CTntaname, by = "Name")

# --- For Boro
data_necessary_Boro <- cbind(data_necessary, borocodenum = data$borocodenum)
tbl_df(data_necessary_Boro)
data_necessary_Boro[is.na(data_necessary_Boro)] <- 0
uBR <- data_necessary_Boro %>%
  group_by(borocodenum) %>%
  summarise_all(funs(mean))
head(uBR)

dfBR <- dplyr::left_join(boros_DF, uBR, by = c("BoroCode" = "borocodenum"))
#   --- Combine shapefile with data
boros_attr <- boros
boros_attr@data <- dplyr::left_join(boros_attr@data, uBR, by = c("BoroCode" = "borocodenum"))

# --- For Precinct
data_precinct[is.na(data_precinct)]

uPP <- data_precinct %>%
  group_by(precinct) %>%
  summarise_all(funs(mean))
head(uPP)

dfPP <- dplyr::left_join(nypp_DF, uPP, by = c("Precinct" = "precinct"))
#   --- Combine shapefile with data
nypp_attr <- nypp
nypp_attr@data <- dplyr::left_join(nypp_attr@data, uPP, by = c("Precinct" = "precinct"))



# [Test leaflet] ----
# ny.map_attr
# ct2000shp_attr
# colfunc <- colorRampPalette(c("white", "green"))
varValues <- nypp_attr$offpcap
labels <- sprintf("<strong>%s</strong><br/><b><u>offpcap</u></b> %g<br/>",
                  nypp_attr@data$Precinct, signif(varValues, 4))
labels <- lapply(labels, HTML)
intervals <- c(0.01, 0.02, 0.03, 0.04, 0.06, 0.08, 0.10, 0.1, 1)

GetColorPalByBins <- function(varValues, intervals, color1, color2 = "white") {

  pal <- function(varValues) {
    intervals <- sort(intervals)
    colfunc <- colorRampPalette(c(color2, color1))
    colors <- colfunc(length(intervals) + 1)
    
    colorDeter <- data.frame(sapply(intervals, function(x) {
      x < varValues
    }))
    
    varColorIdx <- apply(colorDeter, 1, sum) + 1
    varColors <- colors[varColorIdx]
    varColors[is.na(varColors)] <- "#000000"
    return(varColors)
  }
  return(pal)
}

pal <- GetColorPalByBins(varValues, intervals, "blue")
pal(varValues)
pal <- colorQuantile(colfunc(6), domain = varValues, n = 6)
pal(varValues)

map <- leaflet(ny.map_attr) %>% 
  setView(-73.91271, 40.69984, 11) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey map") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard map") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") %>%
  addLayersControl(
    baseGroups = c("Grey map", "Standard map", "Dark map"),
    position = "topleft",
    options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE)
  ) %>%
  # addPolygons(weight = 4, color = "red") %>%
  # addPolygons(data = ct2000shp_attr, weight = 4, color = "blue") %>%
  # addPolygons(data = boros_attr, weight = 4, color = "black") %>%
  addPolygons(
    data = nypp_attr,
    fillColor = pal(varValues),
    weight = 1,
    opacity = 1,
    color = "green",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = F),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    # popup = tileVar,
    group = "offpcap"
    )
# %>%
#   addLegend(
#     pal = pal,
#     values = varValues,
#     opacity = 0.7,
#     title = "offpcap",
#     position = "bottomright",
#     group = "offpcap"
#   )

map



# [Define server ui] ----
ui <- fluidPage(
  # App title
  titlePanel("VNSNY/UPENN ABMS Study"),
  navlistPanel(
    "Contents",
    tabPanel(
     title = "ABM Census",
     h3("Select a variable to show on the tile and check others on the map. (Wait for about 10s for plotting.)"),
      # Sidebar layout with input and output definitions
      sidebarLayout(
        fluidRow(
          column(
            leafletOutput(outputId = "outputMap", height = 800),
            width = 10.5, offset = 0.5
          )
        ),
        fluidRow(
          column(
            radioButtons(inputId = "ChooseShapefileID",
                        label = h4("Select which map to show"),
                        choices = shapeDataList),
            selectInput("ChooseTileVar",
                        "Variable to show on the tile:",
                        unlist(checkboxGroupList)),
            verbatimTextOutput("displaySomething"),
            # radioButtons(inputId = "ChooseTileVarID2",
            #              label = h4("Select which variable to show on tile"),
            #              choices = checkboxGroupList),
            # actionButton("Plot", "Draw plot"),
            
            # fluidRow("Click the button to plot."),
            width = 3
          ),
          column(
            checkboxGroupInput(inputId = "ChooseLabelVars",
                         label = h4("Check variable definitions"),
                         choices = checkboxGroupList,
                         inline = FALSE),
            width = 3
          ),
          column(
            verbatimTextOutput("displaySomething2"),
            uiOutput("varDefOutput"),
            width = 5
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
        sliderInput(
          inputId = "bins",
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
  
  cat("000000")
  # A test plot for other usage
  output$distPlot <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
  # Create reactive variables that will reactive to user input (Select CT / NB)
  #   Variable shown on tile
  tileVar_r <- reactive({input$ChooseTileVar})
  map_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(ct2000shp_attr)
    } else if (input$ChooseShapefileID == "NB") {
      return(ny.map_attr)
    }
  })
  #   Label of shapedata
  mapDataDisplayLabel_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(ct2000shp_attr$NTANAme)
    } else if (input$ChooseShapefileID == "NB") {
      return(ny.map_attr$Name)
    }
  })
  #   Useful data for tile
  utilData_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(uCT)
    } else if (input$ChooseShapefileID == "NB") {
      return(uNB)
    }
  })
  #   Set range of radius of circles
  radiusRange_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(c(1,12))
    } else if (input$ChooseShapefileID == "NB") {
      return(c(2,32))
    }
  })
  #   Whether show CT names in hoverinfo
  CTNames_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(FALSE)
    } else if (input$ChooseShapefileID == "NB") {
      return(TRUE)
    }
  })
  # Generate variable definitions
  varDefOutput_r <- reactive({
    label <- "<h4>Variable Definitions</h4>"
    for (var in input$ChooseLabelVars) {
      varId <- checkboxGroupListIndex[[var]]
      varSN <- varShortNames[varId]
      varD <- varDefinitions[varId]
      label <- paste0(label, "<h5>", varSN, ":</h5>", varD, "<br/>")
    }
    HTML(label)
  })
  
  # Show variable definitions
  output$varDefOutput <- renderUI({
    varDefOutput_r()
  })
  
  # Show leaflet base map
  output$outputMap <- renderLeaflet({
      map <- leaflet(map_r()) %>% 
      setView(-73.91271, 40.69984, 11) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey map") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard map") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") %>%
      addLayersControl(
        baseGroups = c("Grey map", "Standard map", "Dark map"),
        position = "topleft",
        options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE)
      )
    })
  
  # Observe command from map control, hide / show layers
  observe({
    # Initialize map components
    map <- map_r()
    # map <- ct2000shp_attr
    mapData <- map@data
    mapDataDisplayLabel <- mapDataDisplayLabel_r()
    utilData <- utilData_r()
    radiusRange <- radiusRange_r()
    CTNames <- CTNames_r()
    
    cat("111111")
    
    tileVar <- input$ChooseTileVar
    # tileVar <- "subdens"
    # tileVar <- "subacc"
    # tileVar <- "popdens"
    restVars <- varNames[varNames != tileVar]
    labelVars <- restVars
    tileVarIdx <- checkboxGroupListIndex[[tileVar]]
    
    cat("aaaaaaaa")
    if(showPercentage[tileVarIdx] == 1) {
      cat("a11")
      varValues <- mapData[, tileVar]*100
      cat("a12")
      labels <- sprintf("<h3><strong>%s</strong></h3><br/><b><u>%s:</u></b> %g%%<br/>",
                        mapDataDisplayLabel, varShortNames[tileVarIdx], signif(varValues, 4))
      cat("a13")
    } else {
      cat("a21")
      varValues <- mapData[, tileVar]
      cat("a22")
      labels <- sprintf("<strong>%s</strong><br/><b><u>%s:</u></b> %g<br/>",
                        mapDataDisplayLabel, varShortNames[tileVarIdx], signif(varValues, 4))
      cat("a23")
    }
    cat("bbbbbb")
    for (labelVar in labelVars) {
      # labelVar <- labelVars[1]
      cat("b1")
      labelVarIdx <- checkboxGroupListIndex[[labelVar]]
      cat("b2")
      if(showPercentage[labelVarIdx] == 1) {
        cat("b31")
        labels <- paste0(labels, "<b>", varShortNames[labelVarIdx], ":</b> ",
                         signif(100*mapData[, labelVar], 4), "%<br/>")
      } else {
        cat("b32")
        labels <- paste0(labels, "<b>", varShortNames[labelVarIdx], ":</b> ",
                         signif(mapData[, labelVar], 4), "<br/>")
      }
    }
    cat("cccccc")
    if (CTNames) {
      labels <- paste0(labels, "<strong>CTs:</strong><br/>", mapData$NTANAme)
    }
    
    cat("22222")
    labels <- lapply(labels, HTML)
    colorPal <- c("Reds", "Greens", "Blues")
    tileVarIdxR <- tileVarIdx %% 3
    colfunc <- colorRampPalette(c("white", varColors[tileVarIdx]))
    colfunc(6)
    # if (tileVar == "subacc") {
    if (0) {
      pal <- colorFactor(colfunc(6), domain = varValues)
    } else if (abs(skewness(as.numeric(as.character(varValues)), na.rm = T)) > 1 | tileVar == "subacc") {
      pal <- colorBin(colfunc(6), domain = varValues, n = 6)
    } else {
      pal <- colorQuantile(colfunc(6), domain = varValues, n = 6)
    }
    
    # Create a proxy for Leaflet map, saving render time
    proxy <- leafletProxy("outputMap", data = map)

    # Generate Leaflet map layers
    proxy <- proxy %>%
      clearShapes() %>%
      addPolygons(weight = 4, color = "while") %>%
      # Add tile layer
      addPolygons(
        fillColor = pal(varValues),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = F),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal",
                       padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        # popup = tileVar,
        group = paste0("T_", varShortNames[tileVarIdx])
      ) 
    proxy <- proxy %>%
      clearControls() %>%
      # Add corresponding legend
      addLegend(
        pal = pal,
        values = varValues,
        opacity = 0.7,
        title = varShortNames[tileVarIdx],
        position = "bottomright",
        group = paste0("T_", varShortNames[tileVarIdx])
      )
    
    # Add all other variables to be shown as circles
    labelVarShortNames <- c()
    
    if (length(labelVars) > 0) {
      groupNameList <- c(NULL)
      for (labelVar in labelVars) {
        labelVarIdx <- checkboxGroupListIndex[[labelVar]]
        pal2 <- colorBin(varColors[labelVarIdx], domain = mapData[, labelVar], bins = 6)
        groupName <- varShortNames[labelVarIdx]
        labelVarShortNames <- c(labelVarShortNames, groupName)

        proxy <- proxy %>%
          addLegend(
            colors = varColors[labelVarIdx],
            labels = paste0("<b>", varShortNames[labelVarIdx], "</b>"),
            opacity = 0.7,
            title = NULL,
            position = "bottomright",
            group = groupName
          ) %>%
          hideGroup(groupName) %>%
          addCircles(
            lng = ~ctrdlong, lat = ~ctrdlat,
            weight = GetRadius(mapData[, labelVar],
                               radiusRange[1], radiusRange[2]),
            fill = FALSE,
            color = varColors[labelVarIdx],
            stroke = T, fillOpacity = 0.6, opacity = 0.6,
            group = groupName
          )
          
        groupNameList <- c(groupNameList, groupName)
      }
      
      # Add map control
      proxy <- proxy %>%
        addLayersControl(
          baseGroups = c("Grey map", "Standard map", "Dark map"),
          overlayGroups = labelVarShortNames,
          position = "topleft",
          options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE)
        )
    }
    
    proxy
  })
  
  # # Set up a variable to control the drawing [For buttons]
  # v <- reactiveValues(doPlot = FALSE)
  # # Detect whether the button is clicked
  # observeEvent(input$Plot, {
  #   v$doPlot <- input$Plot
  # })

  # Use for buttons and progress bars
  # plotGIS_r <- reactive({
  #   # # If the button is not clicked, do not draw
  #   # if (v$doPlot == FALSE) {
  #   #   
  #   # }
  #   # Create a Progress object (for progess bar)
  #   # progress <- shiny::Progress$new()
  #   # Initialize the progressbar
  #   # progress$set(message = "Plotting...", value = 0)
  #     # progress$inc(1/5, detail = "Initializing...")
  #     # progress$inc(1/5, detail = "Adding tiles...")
  #     # progress$inc(1/5, detail = "Showing variables...")
  #     # progress$inc(1/5, detail = "Showing more variables...")
  #     # progress$inc(1/5, detail = "Adding legends...")
  #   # on.exit(progress$close())
  #   # Isolate the plot code, maintain the old plot until the button is clicked again
  #   # Make sure it closes when we exit this reactive, even if there's an error
  # })
  
  # # For debug
  # output$displaySomething <- renderPrint({
  #   paste(input$ChooseTileVar,
  #         checkboxGroupListIndex[[input$ChooseTileVar]],
  #         showPercentage[checkboxGroupListIndex[[input$ChooseTileVar]]],
  #         sep = ", "
  #   )
  # })
  # output$displaySomething2 <- renderPrint({
  #   input$ChooseLabelVars
  # })
}
# [Run server] ----
shinyApp(ui = ui, server = server)
