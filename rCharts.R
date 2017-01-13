library(rCharts)

## Example 1 Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')

## Example 2 Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
rPlot(Freq ~ Hair | Eye, color = 'Eye', data = hair_eye, type = 'bar')

r1 <- rPlot(mpg ~ wt | am + vs, data = mtcars, type = 'point', color = 'gear')
r1

graph_chart1.addHandler(function(type, e){
    var data = e.evtData;
    if (type === 'click'){
        return alert("You clicked on car with mpg: " + data.mpg.in[0]);
    }
})

data(economics, package = 'ggplot2')
econ <- transform(economics, date = as.character(date))
m1 <- mPlot(x = 'date', y = c('psavert', 'uempmed'), type = 'Line',
            data = econ)
m1$set(pointSize = 0, lineWidth = 1)
m1

hair_eye_male <- subset(as.data.frame(HairEyeColor), Sex == "Male")
n1 <- nPlot(Freq ~ Hair, group = "Eye", data = hair_eye_male, 
            type = 'multiBarChart')
n1

require(reshape2)
uspexp <- melt(USPersonalExpenditure)
names(uspexp)[1:2] = c('category', 'year')
x1 <- xPlot(value ~ year, group = 'category', data = uspexp, 
            type = 'line-dotted')
x1

h1 <- Highcharts$new()
h1$chart(type = "spline")
h1$series(data = c(1, 3, 2, 4, 5, 4, 6, 2, 3, 5, NA), dashStyle = "longdash")
h1$series(data = c(NA, 4, 1, 3, 4, 2, 9, 1, 2, 3, 4), dashStyle = "shortdot")
h1$legend(symbolWidth = 80)
h1


map3 <- Leaflet$new()
map3$setView(c(51.505, -0.09), zoom = 13)
map3$marker(c(51.5, -0.09), bindPopup = "<p> Hi. I am a popup </p>")
map3$marker(c(51.495, -0.083), bindPopup = "<p> Hi. I am another popup </p>")
map3


usp = reshape2::melt(USPersonalExpenditure)
p4 <- Rickshaw$new()
p4$layer(value ~ Var2, group = 'Var1', data = usp, type = 'area')
p4


## 
names(iris) = gsub("\\.", "", names(iris))
r1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, 
            color = 'Species', type = 'point')
r1$publish('Scatterplot', host = 'gist')
r1$publish('Scatterplot', host = 'rpubs')
