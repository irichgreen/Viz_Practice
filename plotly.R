library(plotly)
plot_ly(z = ~volcano)

library(plotly)
txhousing

p <- ggplot(txhousing, aes(date, median)) +
    geom_line(aes(group = city), alpha = 0.2)

subplot(
    p, ggplotly(p, tooltip = "city"), 
    ggplot(txhousing, aes(date, median)) + geom_bin2d(),
    ggplot(txhousing, aes(date, median)) + geom_hex(),
    nrows = 2, shareX = TRUE, shareY = TRUE,
    titleY = FALSE, titleX = FALSE
)

library(dplyr)
tx <- group_by(txhousing, city)
# initiate a plotly object with date on x and median on y
p <- plot_ly(tx, x = ~date, y = ~median)
# plotly_data() returns data associated with a plotly object, note the group attribute!
plotly_data(p)

# add a line highlighting houston
add_lines(
    # plots one line per city since p knows city is a grouping variable
    add_lines(p, alpha = 0.2, name = "Texan Cities", hoverinfo = "none"),
    name = "Houston", data = filter(txhousing, city == "Houston")
)

allCities <- txhousing %>%
    group_by(city) %>%
    plot_ly(x = ~date, y = ~median) %>%
    add_lines(alpha = 0.2, name = "Texan Cities", hoverinfo = "none")

allCities %>%
    filter(city == "Houston") %>%
    add_lines(name = "Houston")


allCities %>%
    add_fun(function(plot) {
        plot %>% filter(city == "Houston") %>% add_lines(name = "Houston")
    }) %>%
    add_fun(function(plot) {
        plot %>% filter(city == "San Antonio") %>% add_lines(name = "San Antonio")
    })

pm <- GGally::ggpairs(iris)
ggplotly(pm)


data(gapminder, package = "gapminder")
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, 
                           color = continent, frame = year)) +
    geom_point() +
    scale_x_log10()

ggplotly(p)

