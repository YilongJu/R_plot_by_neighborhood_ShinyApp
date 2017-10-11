library(plotly)
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
df$hover <- with(df, paste(state, '<br>', "Beef", beef, "Dairy", dairy, "<br>",
                           "Fruits", total.fruits, "Veggies", total.veggies,
                           "<br>", "Wheat", wheat, "Corn", corn))
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total.exports, text = ~hover, locations = ~code,
    color = ~total.exports, colors = 'Purples'
  ) %>%
  colorbar(title = "Millions USD") %>%
  layout(
    title = '2011 US Agriculture Exports by State<br>(Hover for breakdown)',
    geo = g
  )
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
# Sys.setenv("plotly_username"="yilongju")
# Sys.setenv("plotly_api_key"="X4hZvII7QOQWX4axRI9S")
chart_link = api_create(p, filename="choropleth/ag")
chart_link


library(plotly)
library(maps)

county_df <- map_data("county")
state_df <- map_data("state")
head(merge.shp.vars)
head(county_df)

# create state boundaries
p <- ggplot(county_df, aes(long, lat, group = group)) +
  geom_polygon(colour = alpha("black", 1/2), fill = NA) +
  geom_polygon(data = state_df, colour = "black", fill = NA) + 
  theme_void()
p
q <- ggplotly(p)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(q, filename="geom_polygon/county-level-boundaries2")
chart_link




library(plotly)

dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
gg <- qplot(carat, price, data=dsamp, colour=clarity)


df <- data.frame(x=c(1, 2, 3, 4), y=c(1, 5, 3, 5), group=c('A', 'A', 'B', 'B'))
g <- ggplot(data=df, aes(x=x, y=y, colour=group)) + geom_point()
g <- ggplotly(g)
p <- plotly_build(g)
str(p)




