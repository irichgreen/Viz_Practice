library(plotly)

data("diamonds")
# The diamonds dataset
str(diamonds)
head(diamonds)

# A firs scatterplot has been made for you
plot_ly(diamonds, x = ~carat, y = ~price)

# general plotly example
data("diamonds")
plot_ly(diamonds, x = ~carat, y = ~price, color = ~clarity)

# Replace ___ with the correct vector
plot_ly(diamonds, x = ~carat, y = ~price, color = ~clarity, size = ~carat)


# Calculate the numbers of diamonds for each cut<->clarity combination
library(dplyr)
diamonds_bucket <- diamonds %>% count(cut, clarity)

# Replace ___ with the correct vector
plot_ly(diamonds_bucket, x = ~cut, y = ~n, type = "bar", color = ~clarity) 



# The Non Fancy Box Plot
plot_ly(y = ~rnorm(50), type = "box")

# The Fancy Box Plot
plot_ly(diamonds, y = ~price, color = ~cut, type = "box")

# The Super Fancy Box Plot
plot_ly(diamonds, x = ~clarity, y = ~price, color = ~clarity, type = "box") %>%
    layout(boxmode = "group")



# 3D Plotly
# Your volcano data
str(volcano)

# The heatmap
plot_ly(z = ~volcano, type = "heatmap")

# The 3d surface map
plot_ly(z = ~volcano, type = "surface")



qplot(carat, price, data = diamonds, 
      colour = clarity)
ggplotly()


# Create the ggplot2 graph
ggplot(mtcars, aes(x = wt, y = mpg, col =cyl)) +
    geom_point()

# Make your plot interactive
ggplotly()


