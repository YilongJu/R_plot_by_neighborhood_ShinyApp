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



# author: "Brooke", "Yilong"
# date: [21 Sep, 2017] 


setwd("/Users/yilongju/Dropbox/Study/RA/Rdrawing/VNSNY_UPENN_ABMS_Study")
data <- read.csv("ABM_censustract_file.csv")
names(data)[2]<-"BoroCT2000"

ct2000shp <- readOGR("nyct2000_12c/nyct2000_12c/nyct2000.shp")
boros <- readOGR("nybb_16a/nybb.shp")
ny.map <- readOGR("ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp", layer="ZillowNeighborhoods-NY")
# ny.map <- boros
sodo <- ny.map[ny.map$City == "New York", ]
dat <- data.frame(Longitude = data$ctrdlong, Latitude = data$ctrdlat)
coordinates(dat) <- ~ Longitude + Latitude
#project the dataframe onto the shape file
proj4string(dat) <- proj4string(sodo)
location = over(dat, sodo)
data = cbind(data,location)

summary(data) 

dataProjected <- sodo
# add to data a new column termed "id" composed of the rownames of data
dataProjected@data$id <- rownames(dataProjected@data)

# create a data.frame from our spatial object
watershedPoints <- fortify(dataProjected, region = "id")

# merge the "fortified" data with the data from our spatial object
watershedDF <- merge(watershedPoints, dataProjected@data, by = "id")

# Prepare ct shape data
ct2000shp@data$id <- rownames(ct2000shp@data)
f_ct2000shp <- fortify(ct2000shp , polyname = "BoroCT2000")
ct2000shp_DF <- merge(f_ct2000shp, ct2000shp@data, by = "id")
merge.shp.vars <- merge(ct2000shp_DF, data, by = "BoroCT2000")

ct_names <- merge.shp.vars %>% select(long, lat, order) %>% distinct(long, lat, .keep_all = TRUE)
nrow(merge.shp.vars[!complete.cases(merge.shp.vars$povrate), ])
nrow(data[!complete.cases(data$povrate), ])

# Prepare Boros shape data
boros@data$id <- rownames(boros@data)
f_boros <- fortify(boros, polyname = "BoroCode")
boros_DF <- merge(f_boros, boros@data, by = "id")
bnames <- aggregate(data = boros_DF, cbind(long,lat) ~ BoroName, FUN=function(x) mean(range(x)))
bnames[4,3] <- 200741.5
bnames$BoroName <- as.character(bnames$BoroName)
bnames[6,1] <- "P"
bnames[6,2] <- 1050000
bnames[6,3] <- 180000
bnames$BoroName <- as.factor(bnames$BoroName)


t1 <- data %>%
  group_by(Name) %>%
  summarise(
    Pop.Dens = mean(popdens),
    Povrate.mean = mean(povrate),
    centerLong = mean(ctrdlong),
    centerLat = mean(ctrdlat)
  )

DF <- dplyr::left_join(watershedDF, t1, by = "Name")
#   id      long      lat order  hole piece group State   County     City
# 1 10 -73.96143 40.74851     1 FALSE     1  10.1    NY New York New York
# 2 10 -73.96150 40.74851     2 FALSE     1  10.1    NY New York New York
# 3 10 -73.96157 40.74851     3 FALSE     1  10.1    NY New York New York
# 4 10 -73.96164 40.74852     4 FALSE     1  10.1    NY New York New York
# 5 10 -73.96170 40.74853     5 FALSE     1  10.1    NY New York New York
# 6 10 -73.96180 40.74856     6 FALSE     1  10.1    NY New York New York
#               Name RegionID Pop.Dens Povrate.mean centerLong centerLat
# 1 Roosevelt Island    20239 13188.59     0.169268   -73.9487  40.76297
# 2 Roosevelt Island    20239 13188.59     0.169268   -73.9487  40.76297
# 3 Roosevelt Island    20239 13188.59     0.169268   -73.9487  40.76297
# 4 Roosevelt Island    20239 13188.59     0.169268   -73.9487  40.76297
# 5 Roosevelt Island    20239 13188.59     0.169268   -73.9487  40.76297
# 6 Roosevelt Island    20239 13188.59     0.169268   -73.9487  40.76297

# Need "myfortify", cannot be replaced by "fortify"



DF <- dplyr::left_join(watershedDF, t1, by = "Name")


# Test a plot
varName <- "Pop.Dens"
varIdxR <- 1
lowColors <- c("#132B43", "#2B4313", "#43132B")
highColors <- c("#56B1F7", "#B1F756", "#F756B1")
fill_area <- scale_fill_continuous(low = lowColors[varIdxR + 1], high = highColors[varIdxR+1], name = varName)

center_DF <- merge.shp.vars %>% group_by(BoroCT2000) %>% summarise(clong = mean(long), clat = mean(lat))
head(center_DF)
head(merge.shp.vars)
dim(center_DF)
merge.shp.vars <- merge(merge.shp.vars, center_DF, by = "BoroCT2000")
merge.shp.vars$ctrdlong <- merge.shp.vars$clong
merge.shp.vars$ctrdlat <- merge.shp.vars$clat
merge.shp.vars <- merge.shp.vars[, -ncol(merge.shp.vars)]
merge.shp.vars <- merge.shp.vars[, -ncol(merge.shp.vars)]

sorted_merge.shp.vars <- merge.shp.vars %>% arrange(long, lat)
# bnames[6,2] <- 1050000
# bnames[6,3] <- 180000
p <- 143500
sorted_merge.shp.vars[p:(p+200), c("long", "lat", "povrate", "BoroCT2000", "Name")]
selected_area <- sorted_merge.shp.vars[p:(p+19), c("long", "lat", "povrate", "BoroCT2000", "Name")]

ggplot(selected_area) +
  geom_point(aes(long, lat)) +
  geom_text(aes(long, lat, label = Name), size = 3)

selected_merge.shp.vars <- merge.shp.vars %>% filter(Name == "Springfield Gardens")
head(selected_merge.shp.vars)
dim(selected_merge.shp.vars)
class(selected_merge.shp.vars$order)

class(selected_merge.shp.vars$group)
head(sorted_merge.shp.vars)
dim(merge.shp.vars)
min(merge.shp.vars$ctrdlat)
max(merge.shp.vars$ctrdlat)

# # >>>>> New
# test_plot <- ggplot() + 
#   geom_polygon(data = merge.shp.vars, aes(long, lat, group = group, fill = propoa)) + 
#   geom_polygon(data = boros_DF, aes(long, lat, group = group), fill = NA, color="black") +
#   coord_equal(ratio = 1) + 
#   scale_fill_distiller(labels = percent, name="Percent",
#                        palette = "Reds", breaks = pretty_breaks(n = 4), direction = 1) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   labs(title = "Poverty Rate") +
#   theme_nothing(legend=TRUE) + 
#   geom_text(data = bnames, aes(long, lat, label = BoroName), size = 3, fontface = "bold") +
#   geom_polygon(data = selected_merge.shp.vars, aes(long, lat, group = group), fill = NA, color = "blue") +
#   geom_text(data = selected_merge.shp.vars, aes(ctrdlong, ctrdlat, label = BoroCT2000), size = 0.5) +
#   # geom_point(data = selected_merge.shp.vars, aes(long, lat, color = order), size = 0.2) +
#   # geom_text(data = selected_merge.shp.vars, aes(long, lat, label = order), size = 0.5) +
#   geom_point(data = selected_merge.shp.vars, aes(long, lat, color = as.numeric(group)), size = 0.2) +
#   scale_color_gradient(low = "#132B43", high = "#56B1F7") +
#   geom_text(data = selected_merge.shp.vars, aes(long, lat, label = as.character(group)), size = 0.3)
# # test_plot
# ggsave(plot = test_plot, filename = "Propoa9.png",
#        width=20, height=15, type='cairo-png', dpi = 1200)
# 
#   
# # New <<<<<
  
  
# ggplot(data = DF) +
#   geom_polygon(aes(x=long, y=lat, group = group, fill = Pop.Dens)) +
#   # geom_bar(data = DF, aes(x=long, y=lat, group = group, fill = Povrate.mean)) +
#   geom_path(aes(x=long, y=lat, group = group), color = "black", size = 0.2) +
#   # scale_fill_continuous(low = "#132B43", high = "#56B1F7") +
#   scale_fill_distiller(name = varName,
#                        palette = "Reds", breaks = pretty_breaks(n = 4)) +
#   # scale_fill_distiller(labels=percent,name="Percent",
#   #                      palette="Reds",breaks=pretty_breaks(n=4)) +
#   guides(fill = guide_legend(reverse=TRUE)) +
#   coord_equal() + 
#   theme_nothing(legend = TRUE)

# 
# result_plot <- ggplot(DF) +
#   geom_polygon(aes(x=long, y=lat, group = group, fill = varName)) +
#   # geom_bar(data = DF, aes(x=long, y=lat, group = group, fill = Povrate.mean)) +
#   geom_path(aes(x=long, y=lat, group = group), color = "black", size = 0.2) +
#   # scale_fill_distiller(labels = percent, name="Percent",
#   # palette = "Reds", breaks = pretty_breaks(n=4)) +
#   # scale_fill_distiller(name = varName,
#   # palette = "Spectral", breaks = pretty_breaks(n=4)) +
#   # guides(fill = guide_legend(reverse=TRUE)) +
#   labs(title = varName) +
#   fill_area +
#   coord_equal() +
#   theme_nothing(legend = TRUE) # +
# # theme_minimal()
# result_plot
# # theme_minimal()

# Warning message: `panel.margin` is deprecated. Please use `panel.spacing` property instead




# Define UI for app that draws barpltos ----
colNames <- colnames(data)
idxNames <- c(1:length(colNames))
checkboxGroupListIndex <- setNames(as.list(idxNames[5:length(colNames)]), colNames[5:length(colNames)])
checkboxGroupList <- setNames(as.list(colNames[5:length(colNames)]), colNames[5:length(colNames)])

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
                 #         > colnames(data_raw)
                 #  [1] "ctuniq"      "boroCT2000"  "ctrdlong"    "ctrdlat"    
                 #  [5] "popdens"     "povrate"     "unemp"       "busdens"    
                 #  [9] "subdens"     "subacc"      "intdens"     "landind"    
                 # [13] "nonres"      "resid"       "ptrans"      "pwalk"      
                 # [17] "propoa"      "propnonw"    "borocodenum"
                 
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
                 
                 # Output: Histogram ----
                 
                 # fluidRow(column(3, verbatimTextOutput("selectedVars")))
                 plotOutput(outputId = "GIS", height = 800),
                 width = 8
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
  
  # Create a Progress object (for progess bar)
  progress <- shiny::Progress$new()
  
  progress$set(message = "Plotting...", value = 0)
  
  # Set up a variable to control the drawing
  v <- reactiveValues(doPlot = FALSE)
  # Detect whether the button is clicked
  observeEvent(input$Plot, {
    v$doPlot <- input$Plot
  })
  # Close the plot
  # observeEvent(input$displayVariables, {
  #   v$doPlot <- FALSE
  # })
  
  output$value <- renderPrint({ input$displayVariables })
  # output$selectedVars <- renderPrint({ colNames[input$displayVariables] })
  output$varLength <- renderPrint({ length(input$displayVariables) })
  
  plotGIS <- reactive({
    # If the button is not clicked, do not draw
    if (v$doPlot == FALSE) {return()}
    
    # Isolate the plot code, maintain the old plot until the button is clicked again
    isolate({
      var_len <- length(input$displayVariables)
      varNames <- input$displayVariables
      if (var_len == 1) {
        varName <- varNames[1]
        t1 <- data %>% 
          group_by(Name) %>%
          summarise_(gmean = interp(~mean(var), var = as.name(varName)))
        DF <- dplyr::left_join(watershedDF, t1, by = "Name")
        
        varIdx <- checkboxGroupListIndex[[varName]]
        varIdxR <- varIdx %% 3
        lowColors <- c("#132B43", "#2B4313", "#43132B")
        highColors <- c("#56B1F7", "#B1F756", "#F756B1")
        fill_area <- scale_fill_continuous(low = lowColors[varIdxR+1], high = highColors[varIdxR+1], name = varName)
        result_plot <- ggplot(DF) +
          geom_polygon(aes(x=long, y=lat, group = group, fill = gmean)) +
          # geom_bar(data = DF, aes(x=long, y=lat, group = group, fill = Povrate.mean)) +
          geom_path(aes(x=long, y=lat, group = group), color = "black", size = 0.2) +
          # scale_fill_distiller(labels = percent, name="Percent",
          # palette = "Reds", breaks = pretty_breaks(n=4)) +
          scale_fill_distiller(name = varName,
                               palette = "Spectral", breaks = pretty_breaks(n=4)) +
          # guides(fill = guide_legend(reverse=TRUE)) +
          labs(title = varName) +
          # fill_area +
          coord_equal() +
          theme_nothing(legend = TRUE) # +
        # theme_minimal()
        return(result_plot)
      }
      
      if (var_len > 1) {
        #var_len <- 2
        data_ids <- data[, "Name"]
        #input$displayVariables <- c("popdens", "povrate")
        # data_vars <- data[, c("popdens", "povrate")]
        data_vars <- data[, input$displayVariables]
        #data_vars <- data[, c("popdens", "povrate")]
        data_coords <- data[, c("ctrdlong", "ctrdlat")]
        data_necessary <- cbind(Name = data_ids, data_vars, data_coords)
        dim(data_necessary)
        data_necessary_nonmissing <- data_necessary[complete.cases(data_necessary), ]
        t2 <- data_necessary %>%
          group_by(Name) %>%
          summarise_all(funs(mean))
        
        DF <- dplyr::left_join(watershedDF, t2, by = "Name")
        DF_nonmissing <- DF[complete.cases(DF[,(ncol(DF)-1):ncol(DF)]), ]
        
        # Draw a blank map
        map_blankFrame <- ggplot(DF_nonmissing) +
          geom_polygon(aes(x=long, y=lat, group = group), fill = "white") +
          geom_path(aes(x=long, y=lat, group = group), color = "black", size = 0.2) +
          geom_point(aes(x=ctrdlong, y=ctrdlat), size = 0.2) +
          # geom_bar(data = DF, aes(x=long, y=lat, group = group, fill = Povrate.mean)) +
          coord_equal() +
          theme_minimal()
        map_blankFrame
        
        # [12:30 PM, 20 Sep, 2017] [To be done] Produce atable with instinct location of centers.
        DF_forBarPlot <- DF_nonmissing %>% distinct(ctrdlong, ctrdlat, .keep_all = TRUE) 
        region_num <- nrow(DF_forBarPlot)
        attr_num <- var_len
        
        # Get centers for each region to display barplots
        # Get attributes
        DF_forBarPlot_attrs <- DF_forBarPlot[, (ncol(DF_forBarPlot) - (attr_num + 2)):ncol(DF_forBarPlot)]
        DF_forBarPlot_centers <- DF_forBarPlot_attrs[,-c(2:(ncol(DF_forBarPlot_attrs)-2))]
        DF_forBarPlot_attrs <- DF_forBarPlot_attrs[, 1:(ncol(DF_forBarPlot_attrs)-2)]
        
        # Standardize data
        # DF_forBarPlot_attrs_mean <- colMeans(DF_forBarPlot_attrs[, -1], na.rm = TRUE)
        # DF_forBarPlot_attrs_sd <- apply(as.matrix(DF_forBarPlot_attrs[, -1]), 2, sd, na.rm = TRUE)
        # DF_forBarPlot_attrs_std <- (DF_forBarPlot_attrs[, 2:ncol(DF_forBarPlot_attrs)] - DF_forBarPlot_attrs_mean)/DF_forBarPlot_attrs_sd
        
        # Minmax Standardize
        DF_forBarPlot_attrs_min <- apply(as.matrix(DF_forBarPlot_attrs[, -1]), 2, min, na.rm = TRUE)
        DF_forBarPlot_attrs_max <- apply(as.matrix(DF_forBarPlot_attrs[, -1]), 2, max, na.rm = TRUE)
        DF_forBarPlot_attrs_std <- (DF_forBarPlot_attrs[, 2:ncol(DF_forBarPlot_attrs)] - DF_forBarPlot_attrs_min)/(DF_forBarPlot_attrs_max - DF_forBarPlot_attrs_min)
        
        # Melt data from plotting
        DF_forBarPlot_attrs_std <- cbind(DF_forBarPlot_attrs[1], DF_forBarPlot_attrs_std)
        melten_DF_attrs <- melt(DF_forBarPlot_attrs_std, id = "RegionID")
        
        # Get a mini barplot list
        bar.testplot_list <- 
          lapply(1:region_num, function(i) { 
            gt_plot <- ggplotGrob(
              ggplot(melten_DF_attrs[which(melten_DF_attrs$RegionID == melten_DF_attrs$RegionID[i]),]) +
                geom_bar(aes(factor(RegionID), value, group = variable), fill = rainbow(attr_num),      
                         position='dodge',stat='identity', color = "black") +
                labs(x = "", y = "") +
                theme(legend.position = "none", rect = element_blank(), line = element_blank(), text = element_blank())
            )
            panel_coords <- gt_plot$layout[gt_plot$layout$name == "panel",]
            gt_plot[panel_coords$t:panel_coords$b, panel_coords$l:panel_coords$r]
          })
        
        barplot_size <- 5e-3
        
        bar_annotation_list <- lapply(1:region_num, function(i) 
          annotation_custom(bar.testplot_list[[i]], 
                            xmin = DF_forBarPlot_centers$ctrdlong[DF_forBarPlot_centers$RegionID == melten_DF_attrs$RegionID[i]] - barplot_size,
                            xmax = DF_forBarPlot_centers$ctrdlong[DF_forBarPlot_centers$RegionID == melten_DF_attrs$RegionID[i]] + barplot_size,
                            ymin = DF_forBarPlot_centers$ctrdlat[DF_forBarPlot_centers$RegionID == melten_DF_attrs$RegionID[i]] - barplot_size,
                            ymax = DF_forBarPlot_centers$ctrdlat[DF_forBarPlot_centers$RegionID == melten_DF_attrs$RegionID[i]] + barplot_size))
        
        result_plot <- Reduce(`+`, bar_annotation_list, map_blankFrame)
        return(result_plot)
      }
    })
    
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
  })
  
  output$GIS <- renderPlot({plotGIS()})
  
  
}

shinyApp(ui = ui, server = server)