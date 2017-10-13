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
library(doParallel)

packages <- c("rgdal", "tidyverse", "shiny", "lazyeval",
              "reshape2", "scales", "ggmap", "Cairo",
              "maptools", "rgeos", "scales", "RColorBrewer",
              "rsconnect", "plotly", "doParallel")

# author: "Brooke", "Yilong"

# [Read data] ----
setwd("/Users/yilongju/Dropbox/Study/GitHub/R_plot_by_neighborhood_ShinyApp/data")
data <- read.csv("ABM_censustract_file.csv")
names(data)[2] <- "BoroCT2000"
data <- data[order(data$BoroCT2000),]
data <- data[-1973, ]
varDef <- read.csv("Variable_Definitions.csv")
ct2000shp <- readOGR("nyct2000_12c/nyct2000_12c/nyct2000.shp")
boros <- readOGR("nybb_16a/nybb.shp")
ny.map <- readOGR("ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp",
                  layer="ZillowNeighborhoods-NY")
# data <- read.csv("data/ABM_censustract_file.csv")
# names(data)[2] <- "BoroCT2000"
# data <- data[order(data$BoroCT2000),]
# varDef <- read.csv("data/Variable_Definitions.csv")
# ct2000shp <- readOGR("data/nyct2000_12c/nyct2000_12c/nyct2000.shp")
# boros <- readOGR("data/nybb_16a/nybb.shp")

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
ExportPlotlyPlot <- function(varIdx) {
  # [Initialize variables] ----
  # varIdx <- 8
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
  
  # [Save Plotly to local file] ----
  htmlwidgets::saveWidget(result_plotly_build, paste0(varName, ".html"))
}

# [Plot for each variable] ----
setwd("/Users/yilongju/Dropbox/Study/GitHub/R_plot_by_neighborhood_ShinyApp/PlotlyPlots2")
# i <- 3
cl <- makeCluster(8)
registerDoParallel(cl)
foreach(i = 1:length(varNames),
        .packages = packages) %dopar% ExportPlotlyPlot(i)


# [Test a plot] ----
# # >>>>> New
# test_plot <- ggplot() +
#   geom_polygon(data = merge.shp.vars, aes(long, lat, group = group, fill = Povrate.mean)) +
#   geom_polygon(data = boros_DF, aes(long, lat, group = group),
#                fill = NA, color="black") +
#   coord_equal(ratio = 1) +
#   scale_fill_distiller(labels = percent, name="Percent",
#                        palette = "Reds",
#                        breaks = pretty_breaks(n = 4), direction = 1) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   labs(title = "Propoa") +
#   geom_text(data = bnames, aes(long, lat, label = BoroName), size = 3, fontface = "bold") +
#   theme_nothing(legend=TRUE)
# test_plot
# # ggsave(plot = test_plot, filename = "Propoa10order.png",
# #        width=20, height=15, type='cairo-png', dpi = 1200) 
# # New <<<<<
# 
# 
# library(doParallel)
# registerDoParallel(cores=4)
# foreach(i=1:3) %dopar% sqrt(i)