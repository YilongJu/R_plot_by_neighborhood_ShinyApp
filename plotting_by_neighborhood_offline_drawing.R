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

# author: "Brooke", "Yilong"
# date: [21 Sep, 2017]

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



# [Covert real number vector to percentage vector] ----
NumToPercentage <- function(numVec) {
  PercVec <- paste0(round(100 * numVec, 2), "%")
  return(PercVec)
}

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

# [Function for generating Plotly plot] ----
#   Need to be declared after processing the data
GetPlotlyPlot <- function(varIdx) {
  # [Initialize variables] ----
  # varIdx <- 2
  varName <- varNames[varIdx]
  varShortName <- varShortNames[varIdx]
  
  # [Get Plotly] ----
  t2 <- data %>%
    group_by(BoroCT2000) %>%
    summarise_(gmean = interp(~mean(var), var = as.name(as.character(varName))))
  t2$BoroCT2000 <- as.character(t2$BoroCT2000)
  merge.shp.vars <- dplyr::left_join(ct2000shp_DF, t2, by = "BoroCT2000")
  merge.shp.vars <- merge.shp.vars %>% 
    select(-(CTLabel:CT2000), -(CDEligibil:NTACode), -(PUMA:Shape_Area))
  
  center_DF <- merge.shp.vars %>%
    group_by(BoroCT2000) %>%
    summarise(ctrdlong = mean(long), ctrdlat = mean(lat))
  merge.shp.vars <- merge(merge.shp.vars, center_DF, by = "BoroCT2000")
  merge.shp.vars_forPlotly <- merge.shp.vars %>%
    select(BoroCT2000, ctrdlong, ctrdlat, gmean, NTANAme) %>%
    distinct(BoroCT2000, .keep_all = TRUE)
  merge.shp.vars_forPlotly$NTANAme <- as.character(merge.shp.vars_forPlotly$NTANAme)
  merge.shp.vars$NTANAme <- as.character(merge.shp.vars$NTANAme)
  merge.shp.vars$piece <- as.character(merge.shp.vars$piece)
  merge.shp.vars$group <- as.character(merge.shp.vars$group)
  
  varIdxR <- varIdx %% 3
  # varIdxR <- 1
  Palette_list <- c("Reds", "Greens", "Blues")
  
  if (!is.na(showPercentage[varIdx])) {
    result_plot <- ggplot() +
      geom_polygon(data = merge.shp.vars,
                   aes(long, lat, group = group,fill = gmean)) +
      geom_polygon(data = boros_DF, aes(long, lat, group = group),
                   fill = NA, color="black") +
      geom_point(data = merge.shp.vars_forPlotly,
                 aes(ctrdlong, ctrdlat), size = 0.2, alpha = 0.15) +
      coord_equal(ratio = 1) +
      scale_fill_distiller(labels = percent,
                           name = paste0(varShortName, "(", varName, ")"),
                           palette = Palette_list[varIdxR + 1],
                           breaks = pretty_breaks(n = 4),
                           direction = 1) +
      # guides(fill = guide_legend(reverse = TRUE)) +
      geom_text(data = bnames, aes(long, lat, label = BoroName),
                size = 3, fontface = "bold") +
      theme_nothing(legend=TRUE)
  } else {
    result_plot <- ggplot() +
      geom_polygon(data = merge.shp.vars,
                   aes(long, lat, group = group, fill = gmean)) +
      geom_polygon(data = boros_DF, aes(long, lat, group = group),
                   fill = NA, color="black") +
      geom_point(data = merge.shp.vars_forPlotly,
                 aes(ctrdlong, ctrdlat), size = 0.2, alpha = 0.15) +
      coord_equal(ratio = 1) +
      scale_fill_distiller(name = paste0(varShortName, " \n(", varName, ")"),
                           palette = Palette_list[varIdxR + 1],
                           breaks = pretty_breaks(n = 4),
                           direction = 1) +
      # guides(fill = guide_legend(reverse = TRUE)) +
      geom_text(data = bnames, aes(long, lat, label = BoroName),
                size = 3, fontface = "bold") +
      theme_nothing(legend=TRUE)
  }
  result_plotly <- ggplotly(result_plot)
  # [Modify Plotly] ----
  result_plotly_build <- plotly_build(result_plotly)
  centerPointDataLoc <- length(result_plotly_build$x$data) - 2
  
  if (!is.na(showPercentage[varIdx])) {
    result_plotly_build$x$data[[centerPointDataLoc]]$hoverinfo <-
      paste0("CT: ", merge.shp.vars_forPlotly$NTANAme,
             "<br />", varShortNames[varIdx], ": ",
             NumToPercentage(merge.shp.vars_forPlotly$gmean))
    result_plotly_build$x$data[[centerPointDataLoc]]$text <-
      paste0("CT: ", merge.shp.vars_forPlotly$NTANAme,
             "<br />", varShortNames[varIdx], ": ",
             NumToPercentage(merge.shp.vars_forPlotly$gmean))
  } else {
    result_plotly_build$x$data[[centerPointDataLoc]]$hoverinfo <-
      paste0("CT: ", merge.shp.vars_forPlotly$NTANAme,
             "<br />", varShortNames[varIdx], ": ",
             merge.shp.vars_forPlotly$gmean)
    result_plotly_build$x$data[[centerPointDataLoc]]$text <-
      paste0("CT: ", merge.shp.vars_forPlotly$NTANAme,
             "<br />", varShortNames[varIdx], ": ",
             merge.shp.vars_forPlotly$gmean)
  }
  
  # [Return Plotly plot] ----
  return(result_plotly_build)
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
      
      # Used for multiple variables ----
      # if (input_VarLen > 1) {
      #   # Create a Progress object (for progess bar)
      #   progress <- shiny::Progress$new()
      #   # Initialize the progressbar
      #   progress$set(message = "Plotting...", value = 0)
      #   data_ids <- data[, "BoroCT2000"]
      #   data_ids <- as.character(data_ids)
      #   # var_len <- 2
      #   data_vars <- data[, input$displayVariables]
      #   # data_vars <- data[, c("popdens", "povrate")]
      #   data_coords <- data[, c("ctrdlong", "ctrdlat")]
      #   data_necessary <- cbind(BoroCT2000 = data_ids, data_vars, data_coords)
      #   data_necessary_nonmissing <- data_necessary[complete.cases(data_necessary), ]
      #   t2 <- data_necessary %>%
      #     group_by(BoroCT2000) %>%
      #     summarise_all(funs(mean))
      #   
      #   DF <- dplyr::left_join(ct2000shp_DF, t2, by = "BoroCT2000")
      #   center_DF <- DF %>% 
      #     group_by(BoroCT2000)%>%
      #     summarise(clong = mean(long), clat = mean(lat))
      #   DF <- left_join(DF, center_DF, by = "BoroCT2000")
      #   DF$ctrdlong <- DF$clong
      #   DF$ctrdlat <- DF$clat
      #   DF <- DF[, -ncol(DF)]
      #   DF <- DF[, -ncol(DF)]
      #   
      #   DF_nonmissing <- DF[complete.cases(DF[,(ncol(DF)-1):ncol(DF)]), ]
      #   
      #   # Draw a blank map
      #   map_blankFrame <- ggplot(DF_nonmissing) +
      #     geom_polygon(aes(x=long, y=lat, group = group), fill = "white") +
      #     geom_path(aes(x=long, y=lat, group = group), color = "black", size = 0.2) +
      #     geom_point(aes(x=ctrdlong, y=ctrdlat), size = 0.2) +
      #     # geom_bar(data = DF, aes(x=long, y=lat, group = group, fill = Povrate.mean)) +
      #     coord_equal() +
      #     theme_nothing()
      #   # map_blankFrame
      #   
      #   DF_forBarPlot <- DF_nonmissing %>% distinct(ctrdlong, ctrdlat, .keep_all = TRUE)
      #   region_num <- nrow(DF_forBarPlot)
      #   attr_num <- var_len
      #   
      #   # Get centers for each region to display barplots
      #   # Get attributes
      #   # [ID, Attrbutes, ctrdlong, ctrdlat]
      #   BoroCT2000_Idx <- which(colnames(DF_forBarPlot) == "BoroCT2000")
      #   DF_forBarPlot_attrs <- DF_forBarPlot[, c(BoroCT2000_Idx, (ncol(DF_forBarPlot) - (attr_num + 2) + 1):ncol(DF_forBarPlot))]
      #   # [ID, ctrdlong, ctrdlat]
      #   DF_forBarPlot_centers <- DF_forBarPlot_attrs[,-c(2:(ncol(DF_forBarPlot_attrs)-2))]
      #   # [ID, Attrbutes]
      #   DF_forBarPlot_attrs <- DF_forBarPlot_attrs[, 1:(ncol(DF_forBarPlot_attrs)-2)]
      #   
      #   # Minmax Standardize
      #   DF_forBarPlot_attrs_min <- apply(as.matrix(DF_forBarPlot_attrs[, -1]), 2, min, na.rm = TRUE)
      #   DF_forBarPlot_attrs_max <- apply(as.matrix(DF_forBarPlot_attrs[, -1]), 2, max, na.rm = TRUE)
      #   DF_forBarPlot_attrs_std <- apply(as.matrix(DF_forBarPlot_attrs[, -1]), 2,
      #                                    FUN = function(i){i <- (i - min(i, na.rm = TRUE))/
      #                                      (max(i, na.rm = TRUE) - min(i, na.rm = TRUE))})
      #   # (DF_forBarPlot_attrs[, 2:ncol(DF_forBarPlot_attrs)]
      #   #                           - DF_forBarPlot_attrs_min) /(DF_forBarPlot_attrs_max
      #   #                                                        - DF_forBarPlot_attrs_min)
      #   
      #   # Melt data from plotting
      #   DF_forBarPlot_attrs_std <- cbind(DF_forBarPlot_attrs[1], DF_forBarPlot_attrs_std)
      #   melten_DF_attrs <- melt(DF_forBarPlot_attrs_std, id = "BoroCT2000")
      #   
      #   # Get a mini barplot list
      #   region_num <- 100
      #   bar.testplot_list <- foreach(i = 1:region_num) %do% {
      #     gt_plot <- ggplotGrob(
      #       ggplot(melten_DF_attrs[which(melten_DF_attrs$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]),]) +
      #         geom_bar(aes(factor(BoroCT2000), value, group = variable), fill = rainbow(attr_num),
      #                  position='dodge',stat='identity', color = "black") +
      #         scale_fill_brewer(palette = "Spectral") +
      #         labs(x = "", y = "") +
      #         theme(legend.position = "none", rect = element_blank(), line = element_blank(), text = element_blank())
      #     )
      #     panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
      #     # progress$inc(1/(2*region_num + floor(region_num/10)), detail = paste("Draw for region ", i, " / ", region_num))
      #     return(gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r])
      #   }
      #   
      #   
      #   barplot_size <- 500
      #   # print(bar.testplot_list)
      #   
      #   bar_annotation_list <- foreach(i = 1:region_num) %do% {
      #     custom_annot <- annotation_custom(bar.testplot_list[[i]],
      #                                       xmin = DF_forBarPlot_centers$ctrdlong[DF_forBarPlot_centers$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]] - barplot_size,
      #                                       xmax = DF_forBarPlot_centers$ctrdlong[DF_forBarPlot_centers$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]] + barplot_size,
      #                                       ymin = DF_forBarPlot_centers$ctrdlat[DF_forBarPlot_centers$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]] - barplot_size,
      #                                       ymax = DF_forBarPlot_centers$ctrdlat[DF_forBarPlot_centers$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]] + barplot_size)
      #     progress$inc(1/(2*region_num + floor(region_num/10)), detail = paste("Allocating plots ", i, " / ", region_num))
      #     return(custom_annot)
      #   }
      #   result_plot <- Reduce(`+`, bar_annotation_list, map_blankFrame)
      #   progress$inc(floor(region_num/10)/(2*region_num + floor(region_num/10)), detail = "Showing plot.")
      #   # Remember to close the processbar object.
      #   on.exit(progress$close())
      #   return(result_plot)
      # }
    })
    # Make sure it closes when we exit this reactive, even if there's an error
  })
  
  output$GIS <- renderPlotly({plotGIS()})
  
  
  
}

shinyApp(ui = ui, server = server)