library(highcharter)

data(citytemp)

hc <- highchart() %>% 
    hc_xAxis(categories = citytemp$month) %>% 
    hc_add_series(name = "Tokyo", data = citytemp$tokyo) %>% 
    hc_add_series(name = "London", data = citytemp$london) %>% 
    hc_add_series(name = "Other city",
                  data = (citytemp$tokyo + citytemp$london)/2)

hc

hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2,
             backgroundColor = list(
                 linearGradient = c(0, 0, 500, 500),
                 stops = list(
                     list(0, 'rgb(255, 255, 255)'),
                     list(1, 'rgb(200, 200, 255)')
                 )))

hc <- hc %>% 
    hc_chart(type = "column",
             options3d = list(enabled = TRUE, beta = 15, alpha = 15))

hc

hc <- hc_chart(hc, type = "line", options3d = list(enabled = FALSE))
hc


hc %>% 
    hc_xAxis(title = list(text = "Month in x Axis"),
             opposite = TRUE,
             plotLines = list(
                 list(label = list(text = "This is a plotLine"),
                      color = "#FF0000",
                      width = 2,
                      value = 5.5))) %>% 
    hc_yAxis(title = list(text = "Temperature in y Axis"),
             opposite = TRUE,
             minorTickInterval = "auto",
             minorGridLineDashStyle = "LongDashDotDot",
             showFirstLabel = FALSE,
             showLastLabel = FALSE,
             plotBands = list(
                 list(from = 25, to = JS("Infinity"), color = "rgba(100, 0, 0, 0.1)",
                      label = list(text = "This is a plotBand")))) 

hc <- highchart() %>% 
    hc_xAxis(categories = citytemp$month) %>% 
    hc_add_series(name = "Tokyo", data = citytemp$tokyo) %>% 
    hc_add_series(name = "New York", data = citytemp$new_york) 

hc 


hc %>% 
    hc_add_series(name = "London", data = citytemp$london, type = "area") %>% 
    hc_rm_series(name = "New York")



data(stars)

thm <- hc_theme(
    chart = list(
        backgroundColor = "black"
    ),
    yAxis = list(
        gridLineWidth = 0
    )
)

colors <- c("#FB1108","#FD150B","#FA7806","#FBE426","#FCFB8F",
            "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")

stars$color <- colorize(log(stars$temp), colors)

x <- c("Luminosity", "Temperature", "Distance")
y <- sprintf("{point.%s}",
             c("lum", "temp", "distance"))
tltip <- tooltip_table(x, y)


hchart(stars, "point", x = temp, y = lum, size = radiussun) %>% 
    hc_xAxis(type = "logarithmic", reversed = TRUE) %>% 
    hc_yAxis(type = "logarithmic") %>% 
    hc_title(text = "Our nearest Stars") %>% 
    hc_subtitle(text = "In a Hertzsprung-Russell diagram") %>% 
    hc_add_theme(thm) %>% 
    hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = tltip)




data("weather")
x <- c("Min", "Mean", "Max")
y <- sprintf("{point.%s}",
             c("min_temperaturec", "mean_temperaturec", "max_temperaturec"))
tltip <- tooltip_table(x, y)

hchart(weather, type = "columnrange",
       x = date,
       low = min_temperaturec,
       high = max_temperaturec,
       color = mean_temperaturec) %>% 
    hc_chart(polar = TRUE) %>%
    hc_yAxis(
        max = 30,
        min = -10,
        labels = list(format = "{value} C"),
        showFirstLabel = FALSE
    ) %>% 
    hc_xAxis(
        title = list(text = ""),
        gridLineWidth = 0.5,
        labels = list(format = "{value: %b}")
    ) %>% 
    hc_tooltip(
        useHTML = TRUE,
        headerFormat = as.character(tags$small("{point.x:%d %B, %Y}")),
        pointFormat = tltip
    )



data(diamonds, economics_long, mpg, package = "ggplot2")
hchart(mpg, "scatter", x = displ, y = hwy, group = class)

library("dplyr")

economics_long2 <- filter(economics_long, variable %in% c("pop", "uempmed", "unemploy"))
hchart(economics_long2, "line", x = date, y = value01, group = variable)


hchart(diamonds$price) 
hchart(density(diamonds$price), area = TRUE, color = "#B71C1C", name = "Price")
hchart(diamonds$cut, colorByPoint = TRUE)
hchart(LakeHuron)
x <- stl(log(AirPassengers), "per")

hchart(x)

library("forecast")

x <- forecast(ets(USAccDeaths), h = 48, level = 95)

hchart(x)

library("igraph")
N <- 40

net <- sample_gnp(N, p = 2/N)
wc <- cluster_walktrap(net)

V(net)$label <- seq(N)
V(net)$name <- paste("I'm #", seq(N))
V(net)$page_rank <- round(page.rank(net)$vector, 2)
V(net)$betweenness <- round(betweenness(net), 2)
V(net)$degree <- degree(net)
V(net)$size <- V(net)$degree
V(net)$comm <- membership(wc)
V(net)$color <- colorize(membership(wc))

hchart(net, layout = layout_with_fr)


library("quantmod")

x <- getSymbols("USD/JPY", src = "oanda", auto.assign = FALSE)

hchart(x)


x <- getSymbols("AAPL", auto.assign = FALSE)

hchart(x)


library("survival")
library("dplyr")

data(lung)

lung <- mutate(lung, sex = ifelse(sex == 1, "Male", "Female"))

fit <- survfit(Surv(time, status) ~ sex, data = lung) 

hchart(fit, ranges = TRUE)

hchart(princomp(USArrests, cor = TRUE))


library("dplyr")
library("broom")

data(mpg, package = "ggplot2")

fit <- loess(displ ~ hwy, data = mpg) %>% 
    augment() %>% 
    arrange(hwy)

highchart() %>% 
    hc_title(text = "Package Broom rocks!") %>% 
    hc_add_series_df(mpg, type = "scatter", x = hwy, y = displ, group = class) %>%
    hc_add_series_df(fit, type = "arearange",
                     x = hwy,
                     low = .fitted - .se.fit,
                     high = .fitted + .se.fit)


ds <- lapply(seq(10), function(x){
    list(data = cumsum(rnorm(50, 1, 5)), name = x)
})

highchart() %>%
    hc_plotOptions(series = list(showInLegend = FALSE,marker = list(enabled = FALSE))) %>%
    hc_add_series_list(ds)


highchart(type = "stock") %>% 
    hc_title(text = "Monthly Deaths from Lung Diseases in the UK") %>% 
    hc_subtitle(text = "Deaths from bronchitis, emphysema and asthma") %>% 
    hc_add_series_ts(fdeaths, name = "Female") %>%
    hc_add_series_ts(mdeaths, name = "Male")


data("favorite_bars")
data("favorite_pies")

highchart() %>% 
    hc_title(text = "This is a bar graph describing my favorite pies
             including a pie chart describing my favorite bars") %>%
    hc_subtitle(text = "In percentage of tastiness and awesomeness") %>% 
    hc_add_series_labels_values(favorite_pies$pie, favorite_pies$percent, name = "Pie",
                                colorByPoint = TRUE, type = "column") %>% 
    hc_add_series_labels_values(favorite_bars$bar, favorite_bars$percent,
                                colors = substr(terrain.colors(5), 0 , 7), type = "pie",
                                name = "Bar", colorByPoint = TRUE, center = c('35%', '10%'),
                                size = 100, dataLabels = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(text = "percentage of tastiness"),
             labels = list(format = "{value}%"), max = 100) %>% 
    hc_xAxis(categories = favorite_pies$pie) %>% 
    hc_legend(enabled = FALSE) %>% 
    hc_tooltip(pointFormat = "{point.y}%") %>% 
    hc_credits(enabled = TRUE, text = "Source: HIMYM",
               href = "https://www.youtube.com/watch?v=f_J8QU1m0Ng",
               style = list(fontSize = "12px"))


hc <- highcharts_demo()

hc


hc %>% hc_add_theme(hc_theme_538())
hc %>% hc_add_theme(hc_theme_economist())
hc %>% hc_add_theme(hc_theme_ft())

hc %>% hc_add_theme(hc_theme_db())


# You can test:
# hc %>% hc_add_theme(hc_theme_sparkline())

data(economics_long, package = "ggplot2")
library(dplyr)

economics_long %>%
    group_by(variable) %>%
    do(spark = hcts(.$value, type = "area",
                    name = first(.$variable), showInLegend = FALSE) %>%
           hc_add_theme(hc_theme_sparkline())) %>% 
    .[["spark"]] %>% 
    as.list() %>% 
    hw_grid(rowheight = 100)
