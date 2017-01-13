# Example from https://github.com/Ather-Energy/ggTimeSeries
library(ggplot2)
library(ggthemes)
library(data.table)
library(ggTimeSeries)

# creating some data
set.seed(10)
dfData = data.frame(
    Time = 1:1000,
    Signal = abs(
        c(
            cumsum(rnorm(1000, 0, 3)), 
            cumsum(rnorm(1000, 0, 4)), 
            cumsum(rnorm(1000, 0, 1)),
            cumsum(rnorm(1000, 0, 2))
        )
    ),
    VariableLabel = c(rep('Class A', 1000), rep('Class B', 1000), rep('Class C', 1000), rep('Class D', 1000))
)

# base plot
p1 = ggplot(dfData, aes(x = Time, y = Signal, group = VariableLabel, fill = VariableLabel)) +
    stat_steamgraph()


# adding some formatting
p1 + 
    xlab('') + 
    ylab('') + 
    coord_fixed( 0.2 * diff(range(dfData$Time)) / diff(range(dfData$Signal)))

library(plotly)
ggplotly()





library("ggplot2")
library("ggExtra")

# Basic usage
set.seed(30)
df <- data.frame(x = rnorm(500, 50, 10), y = runif(500, 0, 50))
p <- ggplot(df, aes(x, y)) + geom_point()

ggMarginal(p)
ggMarginal(p, colour = "red")
ggMarginal(p, type = "histogram")
