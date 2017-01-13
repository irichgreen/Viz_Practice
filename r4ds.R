library(tidyverse)
library(plotly)
mpg


g <- ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy))

ggplotly(g)

g <- ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplotly(g)

g <- ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplotly(g)


# Left
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplotly()

# Right
ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy, shape = class))
ggplotly()


ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_wrap(~ class, nrow = 2)

ggplotly()

ggplot(data = mpg) + 
    geom_point(mapping = aes(x = displ, y = hwy)) + 
    facet_grid(drv ~ cyl)

ggplotly()


