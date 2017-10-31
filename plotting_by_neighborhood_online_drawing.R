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
# Covert real number vector to percentage vector
NumToPercentage <- function(numVec) {
  PercVec <- paste0(round(100 * numVec, 2), "%")
  return(PercVec)
}

# [Read data] ----
# setwd("/Users/yilongju/Dropbox/Study/GitHub/R_plot_by_neighborhood_ShinyApp")

data <- read.csv("data/ABM_censustract_file.csv")
names(data)[2] <- "BoroCT2000"
data <- data[order(data$BoroCT2000),]
data <- data[-1973, ]
varDef <- read.csv("data/Variable_Definitions.csv")
ct2000shp <- readOGR("data/nyct2000_12c/nyct2000_12c/nyct2000.shp")
boros <- readOGR("data/nybb_16a/nybb.shp")
ny.map <- readOGR("data/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp", layer="ZillowNeighborhoods-NY")

# Reproject ct data, to be consistent with ny neighbor data
ct2000shp <- spTransform(ct2000shp, ny.map@proj4string)

# [Desciption of attributes] ----
beginRow <- 6
varNames <- varDef$varName[beginRow:nrow(varDef) - 1]
varShortNames <- as.character(varDef$varShortName[beginRow:nrow(varDef) - 1])
showPercentage <- varDef$showPercentage[beginRow:nrow(varDef) - 1]
varDefinitions <- varDef$varFullDefinition[beginRow:nrow(varDef) - 1]
checkboxGroupListIndex <- setNames(as.list(c(1:length(varNames))), varNames)
checkboxGroupList <- setNames(as.list(as.character(varNames)), as.character(varShortNames))
shapeDataList <- setNames(as.list(c("CT", "NB")), c("Census Tract Map", "Neighborhood Map"))

ShowColors <- function(col, border = "light gray", ...) {
      n <- length(col)
      plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
           axes = FALSE, xlab = "", ylab = "")
      rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
   }

varColors <- rainbow_hcl(length(varNames), c = 150, l = 60)
ShowColors(varColors)
# 
# varColors <- terrain.colors(length(varNames))
# varColors <- cm.colors(length(varNames))
# varColors <- rainbow_hcl(length(varNames), start = 60, end = 240)
# varColors <- diverge_hcl(length(varNames),
#                          h = c(800, 300), c = 100, l = c(20, 130), power = 0.4)
# ShowColors(varColors)

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
bnames[4,3] <- 200741.5
bnames$BoroName <- as.character(bnames$BoroName)
bnames$BoroName <- as.factor(bnames$BoroName)

# [Prepare ct shape data] ----
#   add to data a new column termed "id" composed of the rownames of data'
#   create a data.frame from our spatial object
#   merge the "fortified" data with the data from our spatial object
ct2000shp@data$id <- rownames(ct2000shp@data)
f_ct2000shp <- fortify(ct2000shp , polyname = "BoroCT2000")
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
data = cbind(data,location)
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
  
  output$distPlot <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
  # # Set up a variable to control the drawing
  # v <- reactiveValues(doPlot = FALSE)
  # # Detect whether the button is clicked
  # observeEvent(input$Plot, {
  #   v$doPlot <- input$Plot
  # })
  
  
  tileVar_r <- reactive({input$ChooseTileVar})
  map_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(ct2000shp_attr)
    } else if (input$ChooseShapefileID == "NB") {
      return(ny.map_attr)
    }
  })
  mapDataDisplayLabel_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(ct2000shp_attr$NTANAme)
    } else if (input$ChooseShapefileID == "NB") {
      return(ny.map_attr$Name)
    }
  })
  utilData_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(uCT)
    } else if (input$ChooseShapefileID == "NB") {
      return(uNB)
    }
  })
  radiusRange_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(c(1,12))
    } else if (input$ChooseShapefileID == "NB") {
      return(c(2,32))
    }
  })
  CTNames_r <- reactive({
    if (input$ChooseShapefileID == "CT") {
      return(FALSE)
    } else if (input$ChooseShapefileID == "NB") {
      return(TRUE)
    }
  })

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
  
  output$varDefOutput <- renderUI({
    varDefOutput_r()
  })
  
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
  
  observe({
    map <- map_r()
    mapData <- map@data
    mapDataDisplayLabel <- mapDataDisplayLabel_r()
    utilData <- utilData_r()
    radiusRange <- radiusRange_r()
    CTNames <- CTNames_r()
    
    tileVar <- input$ChooseTileVar
    # labelVars <- input$ChooseLabelVars
    restVars <- varNames[varNames != tileVar]
    labelVars <- restVars
    tileVarIdx <- checkboxGroupListIndex[[tileVar]]
    
    if(showPercentage[tileVarIdx] == 1) {
      varValues <- mapData[, tileVar]*100
      labels <- sprintf("<h3><strong>%s</strong></h3><br/><b><u>%s:</u></b> %g%%<br/>",
                        mapDataDisplayLabel, varShortNames[tileVarIdx], signif(varValues, 4))
    } else {
      varValues <- mapData[, tileVar]
      labels <- sprintf("<strong>%s</strong><br/><b><u>%s:</u></b> %g<br/>",
                        mapDataDisplayLabel, varShortNames[tileVarIdx], signif(varValues, 4))
    }
    for (labelVar in labelVars) {
      labelVarIdx <- checkboxGroupListIndex[[labelVar]]
      if(showPercentage[labelVarIdx] == 1) {
        labels <- paste0(labels, "<b>", varShortNames[labelVarIdx], ":</b> ",
                         signif(100*mapData[, labelVar], 4), "%<br/>")
      } else {
        labels <- paste0(labels, "<b>", varShortNames[labelVarIdx], ":</b> ",
                         signif(mapData[, labelVar], 4), "<br/>")
      }
    }
    if (CTNames) {
      labels <- paste0(labels, "<strong>CTs:</strong><br/>", mapData$NTANAme)
    }
    
    labels <- lapply(labels, HTML)

    proxy <- leafletProxy("outputMap", data = map)
      
    # tileVar <- "subdens"
    # tileVar <- "subacc"
    # tileVar <- "popdens"
    colorPal <- c("Reds", "Greens", "Blues")
    tileVarIdxR <- tileVarIdx %% 3
    
    numericValues <- unlist(c(data[, tileVar]), use.names = FALSE)
    if (tileVar == "subacc") {
      pal <- colorFactor(colorPal[tileVarIdxR + 1], domain = varValues)
    } else if (abs(skewness(as.numeric(as.character(varValues)), na.rm = T)) > 1) {
      pal <- colorBin(colorPal[tileVarIdxR + 1], domain = varValues, n = 6)
    } else {
      pal <- colorQuantile(colorPal[tileVarIdxR + 1], domain = varValues, n = 6)
    }
    

    proxy <- proxy %>%
      clearShapes() %>%
      addPolygons(weight = 4, color = "while") %>%
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
        popup = tileVar,
        group = paste0("T_", varShortNames[tileVarIdx])
      ) 
    proxy <- proxy %>%
      clearControls() %>%
      addLegend(
        pal = pal,
        values = varValues,
        opacity = 0.7,
        title = varShortNames[tileVarIdx],
        position = "bottomright",
        group = paste0("T_", varShortNames[tileVarIdx])
      )
    # 
    labelVarShortNames <- c()
    
    if (length(labelVars) > 0) {
      for (labelVar in labelVars) {
        labelVarIdx <- checkboxGroupListIndex[[labelVar]]
        pal2 <- colorBin(varColors[labelVarIdx], domain = mapData[, labelVar], bins = 6)
        groupName <- varShortNames[labelVarIdx]
        labelVarShortNames <- c(labelVarShortNames, groupName)

        proxy <- proxy %>%
          addCircles(
            lng = ~ctrdlong, lat = ~ctrdlat,
            weight = GetRadius(mapData[, labelVar],
                               radiusRange[1], radiusRange[2]),
            # fill = pal2(mapData[, labelVar]),
            fill = FALSE,
            color = varColors[labelVarIdx],
            stroke = T, fillOpacity = 0.6, opacity = 0.6,
            group = groupName
          ) %>%
          addLegend(
            colors = varColors[labelVarIdx],
            labels = paste0("<b>", varShortNames[labelVarIdx], "</b>"),
            opacity = 0.7,
            title = NULL,
            position = "bottomright",
            group = groupName
          ) %>%
          hideGroup(groupName)
      }
      
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
  
  # plotGIS_r <- reactive({
  #   # # If the button is not clicked, do not draw
  #   # if (v$doPlot == FALSE) {
  #   #   
  #   # }
  #   # Create a Progress object (for progess bar)
  #   # progress <- shiny::Progress$new()
  #   # Initialize the progressbar
  #   # progress$set(message = "Plotting...", value = 0)
  #   shapefile <- input$ChooseShapefileID
  #   tileVar <- tileVar_r()
  #   
  #   if (shapefile == "CT") {
  #     # progress$inc(1/5, detail = "Initializing...")
  #     labels <- sprintf(
  #       "<strong>%s</strong><br/><b>popdens:</b> %g people / mi<sup>2</sup><br/><b>povrate:</b> %g%%",
  #       ct2000shp_attr$NTANAme,
  #       round(ct2000shp_attr[, tileVar]),
  #       round(ct2000shp_attr$povrate*100, 2)
  #     ) %>% lapply(htmltools::HTML)
  #     
  #     bins <- GetBinforVar(uCT, tileVar)
  #     pal <- colorBin("YlOrRd", domain = uCT[ ,tileVar], bins = bins)
  #     
  #     bins2 <- GetBinforVar(uCT, "povrate")
  #     pal2 <- colorBin("blue", domain = uCT$povrate, bins = bins)
  #     
  #     # progress$inc(1/5, detail = "Adding tiles...")
  #     map2 <- leaflet(ct2000shp_attr) %>% 
  #       setView(-73.91271, 40.69984, 11) %>%
  #       addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey map") %>%
  #       addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard map") %>%
  #       addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map")
  #     
  #     # progress$inc(1/5, detail = "Showing variables...")
  #     map2 <- map2 %>%
  #       addPolygons(weight = 4, color = "while") %>%
  #       addPolygons(
  #         fillColor = ~pal(popdens),
  #         weight = 1,
  #         opacity = 1,
  #         color = "white",
  #         dashArray = "3",
  #         fillOpacity = 0.7,
  #         highlight = highlightOptions(
  #           weight = 3,
  #           color = "#666",
  #           dashArray = "",
  #           fillOpacity = 0.7,
  #           bringToFront = F),
  #         label = labels,
  #         labelOptions = labelOptions(
  #           style = list("font-weight" = "normal",
  #                        padding = "3px 8px"),
  #           textsize = "15px",
  #           direction = "auto"),
  #         popup = "Hi",
  #         group = "POPDENS"
  #       )
  #     
  #     # progress$inc(1/5, detail = "Showing more variables...")
  #     map2 <- map2 %>%
  #       addCircles(
  #         lng = ~ctrdlong, lat = ~ctrdlat,
  #         weight = GetRadius(ct2000shp_attr@data, "povrate", 1, 8),
  #         fill = ~pal2(povrate),
  #         stroke = T, fillOpacity = 0.6, opacity = 0.6,
  #         group = "PORVRATE"
  #       )
  #     
  #     # progress$inc(1/5, detail = "Adding legends...")
  #     map2 <- map2 %>%
  #       addLegend(
  #         pal = pal,
  #         values = ~popdens,
  #         opacity = 0.7,
  #         title = "Popdens",
  #         position = "bottomright",
  #         group = "POPDENS"
  #       ) %>%
  #       addLegend(
  #         colors = "blue",
  #         labels = "<b>Povrate</b>",
  #         values = ~povrate,
  #         opacity = 0.7,
  #         title = NULL,
  #         position = "bottomright",
  #         group = "PORVRATE"
  #       ) %>%
  #       addLayersControl(
  #         baseGroups = c("Grey map", "Standard map", "Dark map"),
  #         overlayGroups = c("POPDENS", "PORVRATE"),
  #         options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE)
  #       )
  #     map2
  #   }
  #   else if (shapefile == "NB") {
  #     bins <- GetBinforVar(dfNB, "popdens")
  #     pal <- colorBin("YlOrRd", domain = dfNB$popdens, bins = bins)
  #     
  #     bins2 <- GetBinforVar(dfNB, "povrate")
  #     pal2 <- colorBin("blue", domain = dfNB$povrate, bins = bins)
  #     
  #     labels <- sprintf(
  #       "<strong>%s</strong><br/><b>popdens:</b> %g people / mi<sup>2</sup><br/><b>povrate:</b> %g%%",
  #       ny.map_attr$Name,
  #       round(ny.map_attr$popdens),
  #       round(ny.map_attr$povrate*100, 2)
  #     ) %>% lapply(htmltools::HTML)
  #     
  #     map <- leaflet(ny.map_attr) %>% 
  #       setView(-73.92194, 40.68922, 11) %>%
  #       addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey map") %>%
  #       addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard map") %>%
  #       addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") %>%
  #       addPolygons(weight = 4, color = "while") %>%
  #       addPolygons(
  #         fillColor = ~pal(popdens),
  #         weight = 2,
  #         opacity = 1,
  #         color = "white",
  #         dashArray = "3",
  #         fillOpacity = 0.7,
  #         highlight = highlightOptions(
  #           weight = 5,
  #           color = "#666",
  #           dashArray = "",
  #           fillOpacity = 0.7,
  #           bringToFront = F),
  #         label = labels,
  #         labelOptions = labelOptions(
  #           style = list("font-weight" = "normal",
  #                        padding = "3px 8px"),
  #           textsize = "15px",
  #           direction = "auto"),
  #         popup = "Hi",
  #         group = "POPDENS"
  #       ) %>%
  #       addLegend(
  #         pal = pal,
  #         values = ~popdens,
  #         opacity = 0.7,
  #         title = "Popdens",
  #         position = "bottomright",
  #         group = "POPDENS"
  #       ) %>%
  #       addLegend(
  #         colors = "blue",
  #         labels = "<b>Povrate</b>",
  #         values = ~povrate,
  #         opacity = 0.7,
  #         title = NULL,
  #         position = "bottomright",
  #         group = "PORVRATE"
  #       ) %>%
  #       addCircles(
  #         lng = ~ctrdlong, lat = ~ctrdlat,
  #         weight = GetRadius(ny.map_attr@data, "povrate"),
  #         fill = ~pal2(povrate),
  #         stroke = T, fillOpacity = 0.8, opacity = 0.7,
  #         group = "PORVRATE"
  #       ) %>%
  #       addLayersControl(
  #         baseGroups = c("Grey map", "Standard map", "Dark map"),
  #         overlayGroups = c("POPDENS", "PORVRATE"),
  #         options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE)
  #       )
  #     map
  #   }
  #   # on.exit(progress$close())
  #   # Isolate the plot code, maintain the old plot until the button is clicked again
  #   # Make sure it closes when we exit this reactive, even if there's an error
  # })
}

shinyApp(ui = ui, server = server)