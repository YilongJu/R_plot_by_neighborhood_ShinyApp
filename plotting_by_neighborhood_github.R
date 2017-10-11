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
library(foreach)
library(rsconnect)
library(plotly)
# packageVersion("plotly")


# author: "Brooke", "Yilong"
# date: [21 Sep, 2017]
# Timezone: "US/Eastern" 

# setwd("/Users/yilongju/Dropbox/Study/GitHub/R_plot_by_neighborhood_ShinyApp/data")
# data <- read.csv("ABM_censustract_file.csv")
# names(data)[2] <- "BoroCT2000"
# data <- data[order(data$BoroCT2000),]
# varDef <- read.csv("Variable_Definitions.csv")
# ct2000shp <- readOGR("nyct2000_12c/nyct2000_12c/nyct2000.shp")
# boros <- readOGR("nybb_16a/nybb.shp")
# ny.map <- readOGR("ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp", layer="ZillowNeighborhoods-NY")

data <- read.csv("data/ABM_censustract_file.csv")
names(data)[2] <- "BoroCT2000"
data <- data[order(data$BoroCT2000),]
varDef <- read.csv("data/Variable_Definitions.csv")
ct2000shp <- readOGR("data/nyct2000_12c/nyct2000_12c/nyct2000.shp")
boros <- readOGR("data/nybb_16a/nybb.shp")

# Covert real number vector to percentage vector
NumToPercentage <- function(numVec) {
  PercVec <- paste0(round(100 * numVec, 2), "%")
  return(PercVec)
}


# Prepare ct shape data
#   add to data a new column termed "id" composed of the rownames of data
ct2000shp@data$id <- rownames(ct2000shp@data)
#   create a data.frame from our spatial object
f_ct2000shp <- fortify(ct2000shp , polyname = "BoroCT2000")
#   merge the "fortified" data with the data from our spatial object
ct2000shp_DF <- merge(f_ct2000shp, ct2000shp@data, by = "id")

t2 <- data %>%
  group_by(BoroCT2000) %>%
  summarise(
    Pop.Dens = mean(popdens),
    Povrate.mean = mean(povrate),
  )

t3 <- data %>%
  group_by(BoroCT2000) %>%
  summarise_all(funs(mean(., na.rm = TRUE)))

ct2000shp_DF$BoroCT2000 <- as.character(ct2000shp_DF$BoroCT2000)
t2$BoroCT2000 <- as.character(t2$BoroCT2000)
merge.shp.vars <- dplyr::left_join(ct2000shp_DF, t2, by = "BoroCT2000")

# Prepare Boros shape data
#   add to data a new column termed "id" composed of the rownames of data
boros@data$id <- rownames(boros@data)
#   create a data.frame from our spatial object
f_boros <- fortify(boros, polyname = "BoroCode")
#   merge the "fortified" data with the data from our spatial object
boros_DF <- merge(f_boros, boros@data, by = "id")
#   aggregate to an upper level
bnames <- aggregate(data = boros_DF, cbind(long,lat) ~ BoroName, FUN=function(x) mean(range(x)))
#   offset the label
bnames[4,3] <- 200741.5
bnames$BoroName <- as.character(bnames$BoroName)
bnames$BoroName <- as.factor(bnames$BoroName)

# Test a plot
varName <- "Pop.Dens"
varIdxR <- 1
lowColors <- c("#132B43", "#2B4313", "#43132B")
highColors <- c("#56B1F7", "#B1F756", "#F756B1")
fill_area <- scale_fill_continuous(low = lowColors[varIdxR + 1], high = highColors[varIdxR+1], name = varName)

center_DF <- merge.shp.vars %>% group_by(BoroCT2000) %>% summarise(ctrdlong = mean(long), ctrdlat = mean(lat))
# colnames(center_DF)[2:3] <- c("ctrdlong", ctrdlat)
merge.shp.vars <- merge(merge.shp.vars, center_DF, by = "BoroCT2000")
# merge.shp.vars$ctrdlong <- merge.shp.vars$clong
# merge.shp.vars$ctrdlat <- merge.shp.vars$clat
# merge.shp.vars <- merge.shp.vars[, -(ncol(merge.shp.vars)-2)]
# merge.shp.vars <- merge.shp.vars[, -(ncol(merge.shp.vars)-2)]
merge.shp.vars_forPlotly <- merge.shp.vars %>%
  select(BoroCT2000, ctrdlong, ctrdlat, Povrate.mean, NTANAme) %>%
  distinct(BoroCT2000, .keep_all = TRUE)
# >>>>> New
test_plot <- ggplot() +
  geom_polygon(data = merge.shp.vars, aes(long, lat, group = group, fill = Povrate.mean)) +
  geom_polygon(data = boros_DF, aes(long, lat, group = group), fill = NA, color="black") +
  geom_point(data = merge.shp.vars_forPlotly, aes(ctrdlong, ctrdlat), size = 0.2, alpha = 0.15) +
  coord_equal(ratio = 1) +
  scale_fill_distiller(labels = percent, name="Percent",
                       palette = "Reds", breaks = pretty_breaks(n = 4), direction = 1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Mean Povrate") +
  geom_text(data = bnames, aes(long, lat, label = BoroName), size = 3, fontface = "bold") +
  theme_nothing(legend=TRUE)
# test_plot
test_plotly <- ggplotly(test_plot)
test_plotly_build <- plotly_build(test_plotly)
test_plotly_build$x$data[[2140]]$hovertext <- 
  paste0("CT: ", merge.shp.vars_forPlotly$NTANAme,
         "<br />Mean Povrate: ",
         NumToPercentage(merge.shp.vars_forPlotly$Povrate.mean))
test_plotly_build
names(test_plotly_build$x$data[[1]])
test_plotly_build$x$data[[1]]$type
test_plotly_build$x$data[[3]]$text
str(test_plotly_build$x$data[[4]])
# CTname

x <- c(0.1, 0.2, 0.042323)

NumToPercentage(x)
test_plotly_build$x$data[[2140]]
head(test_plotly_build$x$data[[2140]]$hovertext)
toy_hovertext <- head(test_plotly_build$x$data[[2140]]$hovertext)
toy_msvfp <- head(merge.shp.vars_forPlotly)
toy_hovertext <- paste0("CT: ", toy_msvfp$NTANAme, "<br />Mean Povrate: ", NumToPercentage(toy_msvfp$Povrate.mean))

# Boroname 
test_plotly_build$x$data[[2141]]
length(test_plotly_build$x$data)


# Sys.setenv("plotly_username"="yilongju")
# Sys.setenv("plotly_api_key"="1g49fyKMWDSJcwC7afZ2")
# chart_link = plotly_POST(test_plotly, filename="geom_polygon/test")
chart_link = api_create(test_plotly, filename="geom_polygon/test2")
chart_link
# ggsave(plot = test_plot, filename = "Propoa10order.png",
#        width=20, height=15, type='cairo-png', dpi = 1200) 
# New <<<<<

# Define UI for app that draws barpltos ----
# All attributes
varNames <- varDef$varName[5:nrow(varDef) - 1]
varShortNames <- varDef$varShortName[5:nrow(varDef) - 1]
varDefinitions <- varDef$varFullDefinition[5:nrow(varDef) - 1]
checkboxGroupListIndex <- setNames(as.list(c(1:length(varNames))), varNames)
checkboxGroupList <- setNames(as.list(varNames), varShortNames)

ui <- fluidPage(
  # App title ----
  titlePanel("VNSNY/UPENN ABMS Study"),
  navlistPanel(
    "Contents",
    tabPanel("ABM Census", h3("Select the attributes and click Draw Plot to exhibit."),
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(
                 # Input: Choose which variables to display ----
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
               # Main panel for displaying outputs ----
               mainPanel(
                 fluidRow(
                   plotOutput(outputId = "GIS", height = 800),
                   width = 8
                 ),
                 fluidRow(
                   htmlOutput("displayVarDef")
                 )
               )
             )
    ),
    tabPanel("Still Working", h3("This is the second panel"),
             # Sidebar layout with input and output definitions ----
             sidebarLayout(

               # Sidebar panel for inputs ----
               sidebarPanel(

                 # Input: Slider for the number of bins ----
                 sliderInput(inputId = "bins",
                             label = "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30)
               ),
               # Main panel for displaying outputs ----
               mainPanel(

                 # Output: Histogram ----
                 plotOutput(outputId = "distPlot")
               )
             )
    ),
    widths = c(2,10)
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
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
    # vars <- c("popdens", "povrate")
    # HTML(paste(varDef[which(varDef$varName %in% vars), 3], collapse = "</br>"))
    
    HTML(paste("<b>", varDef[which(varDef$varName %in% input$displayVariables), 3], "</b>", collapse = "</br>"))
  })
  

  diplayVarDefinition <- reactive({
    var_len <- length(input$displayVariables)
    varNames <- input$displayVariables
  })
    
  plotGIS <- reactive({
    # If the button is not clicked, do not draw
    if (v$doPlot == FALSE) {return()}

    # Isolate the plot code, maintain the old plot until the button is clicked again
    isolate({
      var_len <- length(input$displayVariables)
      varNames <- input$displayVariables
      # var_len <- 1
      # varNames <- c("povrate")
      # varNames <- c("landind")
      
      if (var_len == 1) {
        varName <- varNames[1]
        t2 <- data %>%
          group_by(BoroCT2000) %>%
          summarise_(gmean = interp(~mean(var), var = as.name(varName)))
        t2$BoroCT2000 <- as.character(t2$BoroCT2000)
        merge.shp.vars <- dplyr::left_join(ct2000shp_DF, t2, by = "BoroCT2000")

        varIdx <- checkboxGroupListIndex[[varName]]
        varIdxR <- varIdx %% 3
        # varIdxR <- 1
        Palette_list <- c("Reds", "Greens", "Blues")
        
        result_plot <- ggplot() +
          geom_polygon(data = merge.shp.vars, aes(long, lat, group = group,fill = gmean)) +
          geom_polygon(data = boros_DF, aes(long, lat, group = group),
                       fill = NA, color="black") +
          coord_equal(ratio = 1) +
          scale_fill_distiller(labels = percent, name = varName,
                               palette = Palette_list[varIdxR + 1], breaks = pretty_breaks(n = 4),
                               direction = 1) +
          guides(fill = guide_legend(reverse = TRUE)) +
          geom_text(data = bnames, aes(long, lat, label = BoroName),
                    size = 3, fontface = "bold") +
          theme_nothing(legend=TRUE)
        
        return(result_plot)
      }

      if (var_len > 1) {
        # Create a Progress object (for progess bar)
        progress <- shiny::Progress$new()
        # Initialize the progressbar
        progress$set(message = "Plotting...", value = 0)
        data_ids <- data[, "BoroCT2000"]
        data_ids <- as.character(data_ids)
        # var_len <- 2
        data_vars <- data[, input$displayVariables]
        # data_vars <- data[, c("popdens", "povrate")]
        data_coords <- data[, c("ctrdlong", "ctrdlat")]
        data_necessary <- cbind(BoroCT2000 = data_ids, data_vars, data_coords)
        data_necessary_nonmissing <- data_necessary[complete.cases(data_necessary), ]
        t2 <- data_necessary %>%
          group_by(BoroCT2000) %>%
          summarise_all(funs(mean))

        DF <- dplyr::left_join(ct2000shp_DF, t2, by = "BoroCT2000")
        center_DF <- DF %>% 
          group_by(BoroCT2000)%>%
          summarise(clong = mean(long), clat = mean(lat))
        DF <- left_join(DF, center_DF, by = "BoroCT2000")
        DF$ctrdlong <- DF$clong
        DF$ctrdlat <- DF$clat
        DF <- DF[, -(ncol(DF)-2)]
        DF <- DF[, -(ncol(DF)-2)]
        
        DF_nonmissing <- DF[complete.cases(DF[,(ncol(DF)-1):ncol(DF)]), ]

        # Draw a blank map
        map_blankFrame <- ggplot(DF_nonmissing) +
          geom_polygon(aes(x=long, y=lat, group = group), fill = "white") +
          geom_path(aes(x=long, y=lat, group = group), color = "black", size = 0.2) +
          geom_point(aes(x=ctrdlong, y=ctrdlat), size = 0.2) +
          # geom_bar(data = DF, aes(x=long, y=lat, group = group, fill = Povrate.mean)) +
          coord_equal() +
          theme_nothing()
        # map_blankFrame

        DF_forBarPlot <- DF_nonmissing %>% distinct(ctrdlong, ctrdlat, .keep_all = TRUE)
        region_num <- nrow(DF_forBarPlot)
        attr_num <- var_len

        # Get centers for each region to display barplots
        # Get attributes
        # [ID, Attrbutes, ctrdlong, ctrdlat]
        BoroCT2000_Idx <- which(colnames(DF_forBarPlot) == "BoroCT2000")
        DF_forBarPlot_attrs <- DF_forBarPlot[, c(BoroCT2000_Idx, (ncol(DF_forBarPlot) - (attr_num + 2) + 1):ncol(DF_forBarPlot))]
        # [ID, ctrdlong, ctrdlat]
        DF_forBarPlot_centers <- DF_forBarPlot_attrs[,-c(2:(ncol(DF_forBarPlot_attrs)-2))]
        # [ID, Attrbutes]
        DF_forBarPlot_attrs <- DF_forBarPlot_attrs[, 1:(ncol(DF_forBarPlot_attrs)-2)]

        # Minmax Standardize
        DF_forBarPlot_attrs_min <- apply(as.matrix(DF_forBarPlot_attrs[, -1]), 2, min, na.rm = TRUE)
        DF_forBarPlot_attrs_max <- apply(as.matrix(DF_forBarPlot_attrs[, -1]), 2, max, na.rm = TRUE)
        DF_forBarPlot_attrs_std <- apply(as.matrix(DF_forBarPlot_attrs[, -1]), 2,
                                         FUN = function(i){i <- (i - min(i, na.rm = TRUE))/
                                           (max(i, na.rm = TRUE) - min(i, na.rm = TRUE))})
          # (DF_forBarPlot_attrs[, 2:ncol(DF_forBarPlot_attrs)]
          #                           - DF_forBarPlot_attrs_min) /(DF_forBarPlot_attrs_max
          #                                                        - DF_forBarPlot_attrs_min)

        # Melt data from plotting
        DF_forBarPlot_attrs_std <- cbind(DF_forBarPlot_attrs[1], DF_forBarPlot_attrs_std)
        melten_DF_attrs <- melt(DF_forBarPlot_attrs_std, id = "BoroCT2000")

        # Get a mini barplot list
        region_num <- 100
        bar.testplot_list <- foreach(i = 1:region_num) %do% {
          gt_plot <- ggplotGrob(
            ggplot(melten_DF_attrs[which(melten_DF_attrs$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]),]) +
              geom_bar(aes(factor(BoroCT2000), value, group = variable), fill = rainbow(attr_num),
                       position='dodge',stat='identity', color = "black") +
              scale_fill_brewer(palette = "Spectral") +
              labs(x = "", y = "") +
              theme(legend.position = "none", rect = element_blank(), line = element_blank(), text = element_blank())
          )
          panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
          # progress$inc(1/(2*region_num + floor(region_num/10)), detail = paste("Draw for region ", i, " / ", region_num))
          return(gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r])
        }
        

        barplot_size <- 500
        # print(bar.testplot_list)

        bar_annotation_list <- foreach(i = 1:region_num) %do% {
          custom_annot <- annotation_custom(bar.testplot_list[[i]],
                            xmin = DF_forBarPlot_centers$ctrdlong[DF_forBarPlot_centers$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]] - barplot_size,
                            xmax = DF_forBarPlot_centers$ctrdlong[DF_forBarPlot_centers$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]] + barplot_size,
                            ymin = DF_forBarPlot_centers$ctrdlat[DF_forBarPlot_centers$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]] - barplot_size,
                            ymax = DF_forBarPlot_centers$ctrdlat[DF_forBarPlot_centers$BoroCT2000 == melten_DF_attrs$BoroCT2000[i]] + barplot_size)
          progress$inc(1/(2*region_num + floor(region_num/10)), detail = paste("Allocating plots ", i, " / ", region_num))
          return(custom_annot)
        }
        result_plot <- Reduce(`+`, bar_annotation_list, map_blankFrame)
        progress$inc(floor(region_num/10)/(2*region_num + floor(region_num/10)), detail = "Showing plot.")
        # Remember to close the processbar object.
        on.exit(progress$close())
        return(result_plot)
      }
    })

    # Make sure it closes when we exit this reactive, even if there's an error


  })

  output$GIS <- renderPlot({plotGIS()})
  


}

shinyApp(ui = ui, server = server)
