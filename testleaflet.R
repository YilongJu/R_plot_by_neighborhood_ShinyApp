library(leaflet)
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
library(geojsonio)

setwd("/Users/yilongju/Dropbox/Study/GitHub/R_plot_by_neighborhood_ShinyApp")
data <- read.csv("data/ABM_censustract_file.csv")
names(data)[2] <- "BoroCT2000"
data <- data[order(data$BoroCT2000),]
data <- data[-1973, ]
varDef <- read.csv("data/Variable_Definitions.csv")
ct2000shp <- readOGR("data/nyct2000_12c/nyct2000_12c/nyct2000.shp")
boros <- readOGR("data/nybb_16a/nybb.shp")
ny.map <- readOGR("data/ZillowNeighborhoods-NY/ZillowNeighborhoods-NY.shp",
                  layer="ZillowNeighborhoods-NY")

# Reproject ct data, to be consistent with ny neighbor data
ct2000shp <- spTransform(ct2000shp, ny.map@proj4string)

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

data_ids <- data %>% select(BoroCT2000, Name)
data_vars <- data %>% select(popdens:propnonw)
data_coords <- data[, c("ctrdlong", "ctrdlat")]
data_necessary <- cbind(data_ids, data_vars, data_coords)

t1 <- data_necessary %>%
  group_by(Name) %>%
  summarise_all(funs(mean))

t2 <- data_necessary %>%
  group_by(BoroCT2000) %>%
  summarise_all(funs(mean))

ct2000shp_DF$BoroCT2000 <- as.character(ct2000shp_DF$BoroCT2000)
t2$BoroCT2000 <- as.character(t2$BoroCT2000)

DF <- dplyr::left_join(watershedDF, t1, by = "Name")
DFCT <- dplyr::left_join(ct2000shp_DF, t2, by = "BoroCT2000")
# DF_nonmissing <- DF[complete.cases(DF[,(ncol(DF)-1):ncol(DF)]), ]

center_ct.map <- ct2000shp_DF %>%
  select(long, lat) %>%
  summarise(ctrlong = mean(long), ctrlat = mean(lat))
center_ct.map # -73.91271 40.69984

center_ny.map <- watershedDF %>%
  select(long, lat) %>%
  summarise(ctrlong = mean(long), ctrlat = mean(lat))
center_ny.map # -73.92194 40.68922

GetBinforVar <- function(data, varName) {
  range <- range(data[, varName], na.rm = TRUE)
  maxPwd <- floor(log(range, 10))[2]
  binNum <- floor(range[2]/10^maxPwd)
  bin <- c(0:binNum, Inf) * 10^maxPwd
  return(bin)
}
GetRadius <- function(data, varName, l = 2, u = 32) {
  range <- range(data[, varName], na.rm = TRUE)
  var <- data[, varName]
  var <- (var - range[1])/(range[2] - range[1]) * (u - l) + l
  return(var)
}

# [NY map] ----
ny.map_attr <- ny.map
ny.map_attr@data <- dplyr::left_join(ny.map_attr@data, t1, by = "Name")

bins <- c(0, 5000, 10000, 20000, 30000, 40000, 50000, Inf)

bins <- GetBinforVar(DF, "popdens")
pal <- colorBin("YlOrRd", domain = DF$popdens, bins = bins)

bins2 <- GetBinforVar(DF, "povrate")
pal2 <- colorBin("blue", domain = DF$povrate, bins = bins)


labels <- sprintf(
  "<strong>%s</strong><br/><b>popdens:</b> %g people / mi<sup>2</sup><br/><b>povrate:</b> %g%%",
  ny.map_attr$Name,
  round(ny.map_attr$popdens),
  round(ny.map_attr$povrate*100, 2)
) %>% lapply(htmltools::HTML)

map <- leaflet(ny.map_attr) %>% 
  setView(-73.92194, 40.68922, 10) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey map") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard map") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") %>%
  addPolygons(weight = 4, color = "while") %>%
  addPolygons(
    fillColor = ~pal(popdens),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
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
    popup = "Hi",
    group = "POPDENS"
    ) %>%
  addLegend(
    pal = pal,
    values = ~popdens,
    opacity = 0.7,
    title = "Popdens",
    position = "bottomright",
    group = "POPDENS"
    ) %>%
  addLegend(
    colors = "blue",
    labels = "<b>Povrate</b>",
    values = ~povrate,
    opacity = 0.7,
    title = NULL,
    position = "bottomright",
    group = "PORVRATE"
  ) %>%
  addCircles(
    lng = ~ctrdlong, lat = ~ctrdlat,
    weight = GetRadius(ny.map_attr@data, "povrate"),
    fill = ~pal2(povrate),
    stroke = T, fillOpacity = 0.8, opacity = 0.7,
    group = "PORVRATE"
  ) %>%
  addLayersControl(
    baseGroups = c("Grey map", "Standard map", "Dark map"),
    overlayGroups = c("POPDENS", "PORVRATE"),
    options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE)
  )
map

map %>% addPopups(-73.92194, 40.68922, "Haha",
                  options = popupOptions(closeButton = TRUE)
)
# [CT map] ----

ct2000shp_attr <- ct2000shp
ct2000shp_attr@data <- dplyr::left_join(ct2000shp_attr@data, t2, by = "BoroCT2000")
labels <- sprintf(
  "<strong>%s</strong><br/><b>popdens:</b> %g people / mi<sup>2</sup><br/><b>povrate:</b> %g%%",
  ct2000shp_attr$Name,
  round(ct2000shp_attr$popdens),
  round(ct2000shp_attr$povrate*100, 2)
) %>% lapply(htmltools::HTML)

bins <- GetBinforVar(DFCT, "popdens")
pal <- colorBin("YlOrRd", domain = DFCT$popdens, bins = bins)

bins2 <- GetBinforVar(DFCT, "povrate")
pal2 <- colorBin("blue", domain = DFCT$povrate, bins = bins)


map2 <- leaflet(ct2000shp_attr) %>% 
  setView(-73.91271, 40.69984, 10) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Grey map") %>%
  addProviderTiles(providers$OpenStreetMap.Mapnik, group = "Standard map") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark map") %>%
  addPolygons(weight = 4, color = "while") %>%
  addPolygons(
    fillColor = ~pal(popdens),
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
    popup = "Hi",
    group = "POPDENS"
  ) %>%
  addLegend(
    pal = pal,
    values = ~popdens,
    opacity = 0.7,
    title = "Popdens",
    position = "bottomright",
    group = "POPDENS"
  ) %>%
  addLegend(
    colors = "blue",
    labels = "<b>Povrate</b>",
    values = ~povrate,
    opacity = 0.7,
    title = NULL,
    position = "bottomright",
    group = "PORVRATE"
  ) %>%
  addCircles(
    lng = ~ctrdlong, lat = ~ctrdlat,
    weight = GetRadius(ct2000shp_attr@data, "povrate", 1, 8),
    fill = ~pal2(povrate),
    stroke = T, fillOpacity = 0.6, opacity = 0.6,
    group = "PORVRATE"
  ) %>%
  addLayersControl(
    baseGroups = c("Grey map", "Standard map", "Dark map"),
    overlayGroups = c("POPDENS", "PORVRATE"),
    options = layersControlOptions(autoZIndex = TRUE, collapsed = FALSE)
  )
map2

# DF[which(DF$Name == "Upper East Side"),] %>%
#   select(popdens:propnonw) %>%
#   summarise_all(funs(mean))
