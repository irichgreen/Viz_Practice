library(plotly)
plot_ly(z = ~volcano)

library(leaflet)
pal <- colorQuantile("YlOrRd", NULL, n = 8)
leaflet(orstationc) %>% 
    addTiles() %>%
    addCircleMarkers(color = ~pal(tann))

library(leaflet)

m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

m <- leaflet()
m <- addTiles(m)
m <- addMarkers(m, lng=174.768, lat=-36.852, popup="The birthplace of R")
m

# add some circles to a map
df = data.frame(Lat = 1:10, Long = rnorm(10))
leaflet(df) %>% addCircles()

leaflet(df) %>% addCircles(lng = ~Long, lat = ~Lat)

leaflet() %>% addCircles(data = df)
# or use df in addCircles() only
leaflet() %>% addCircles(data = df, lat = ~ Lat, lng = ~ Long)

library(sp)
Sr1 = Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
Sr2 = Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
Sr3 = Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
Sr4 = Polygon(cbind(c(5, 6, 6, 5, 5), c(4, 4, 3, 3, 4)), hole = TRUE)
Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr4, Sr3), "s3/4")
SpP = SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
leaflet(height = "300px") %>% addPolygons(data = SpP)

library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
    addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

m = leaflet() %>% addTiles()
df = data.frame(
    lat = rnorm(100),
    lng = rnorm(100),
    size = runif(100, 5, 20),
    color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))

library(plotly)
p <- ggplot(data = diamonds, aes(x = cut, fill = clarity)) +
    geom_bar(position = "dodge")
ggplotly(p)

d <- diamonds[sample(nrow(diamonds), 500), ]
plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
        mode = "markers", color = carat, size = carat)

library(DiagrammeR)
grViz("
      digraph {
      layout = twopi
      node [shape = circle]
      A -> {B C D} 
      }")

install.packages("metricsgraphics")
library(metricsgraphics)
mjs_plot(mtcars, x=wt, y=mpg) %>%
    mjs_point(color_accessor=carb, size_accessor=carb) %>%
    mjs_labs(x="Weight of Car", y="Miles per Gallon")

#install.packages("rglwidget")
library(rgl)
library(rglwidget)
library(htmltools)

theta <- seq(0, 6*pi, len=100)
xyz <- cbind(sin(theta), cos(theta), theta)
lineid <- plot3d(xyz, type="l", alpha = 1:0, 
                 lwd = 5, col = "blue")["data"]

browsable(tagList(
    rglwidget(elementId = "example", width = 500, height = 400,
              controllers = "player"),
    playwidget("example", 
               ageControl(births = theta, ages = c(0, 0, 1),
                          objids = lineid, alpha = c(0, 1, 0)),
               start = 1, stop = 6*pi, step = 0.1, 
               rate = 6,elementId = "player")
))

install.packages("threejs")
library(threejs)
z <- seq(-10, 10, 0.01)
x <- cos(z)
y <- sin(z)
scatterplot3js(x,y,z, color=rainbow(length(z)))


library(DT)
datatable(iris, options = list(pageLength = 5))

library(d3heatmap)
d3heatmap(mtcars, scale="column", colors="Blues")


library(networkD3)
data(MisLinks, MisNodes)
forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             Group = "group", opacity = 0.4)

library(visNetwork)
nodes <- data.frame(id = 1:6, title = paste("node", 1:6), 
                    shape = c("dot", "square"),
                    size = 10:15, color = c("blue", "red"))
edges <- data.frame(from = 1:5, to = c(5, 4, 6, 3, 3))
visNetwork(nodes, edges) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

