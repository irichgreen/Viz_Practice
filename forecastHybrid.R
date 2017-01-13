# Install from CRAN the usual way:
install.packages("forecastHybrid")
library(forecastHybrid)
library(ggplot2)
library(scales)
library(tidyverse)

citation("forecastHybrid")

# To cite package ‘forecastHybrid’ in publications use:
#    
#    David Shaub and Peter Ellis (2016). forecastHybrid: Convenient Functions for
# Ensemble Time Series Forecasts. R package version 0.3.0.
# https://CRAN.R-project.org/package=forecastHybrid
# 
# A BibTeX entry for LaTeX users is
# 
# @Manual{,
#    title = {forecastHybrid: Convenient Functions for Ensemble Time Series Forecasts},
#    author = {David Shaub and Peter Ellis},
#    year = {2016},
#    note = {R package version 0.3.0},
#    url = {https://CRAN.R-project.org/package=forecastHybrid},
# }

# median duration of unemployment, in weeks
uempmed <- ts(economics$uempmed, start = c(1967, 7), frequency = 12)

BoxCox.lambda(uempmed) # very close to zero, so use logarithm tansform

m1 <- hybridModel(uempmed, lambda = 0, weights = "equal")

m2 <- hybridModel(uempmed, lambda = 0, weights = "cv.errors")

f1 <- forecast(m1, 24)
f2 <- forecast(m2, 24)

par(mfrow = c(2, 1), font.main = 1, bty = "l", mar = c(4,3,6,2) + 0.1, cex.main = 0.6)
plot(f1)
plot(f2)



par(mfrow = c(3, 2), bty = "l", cex = 0.8)
plot(f2$thetam)
plot(f2$ets)
plot(f2$stlm)
plot(f2$auto.arima)
plot(f2$nnetar)
plot(f2$tbats)



autoplot(f2) +
    ggtitle("Forecast median length of unemployment",
            subtitle = "Six component models, weights chosen by cross-validation") +
    labs(y = "Weeks", x = "",
         caption = "Source: 'economics' in ggplot2 R package,\nultimately from http://research.stlouisfed.org/fred2")

f3 <- thetaf(uempmed, h = 24)
f4 <- forecast(thetam(uempmed), h = 24)

data.frame(thetaf = f3$mean, thetam = f3$mean) %>%
    mutate_each("as.numeric") %>%
    ggplot(aes(x = thetaf, y = thetam)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point() +
    labs(x = "thetaf()", y = "forecast(thetam())",
         caption = "Forecasts are of median length of unemployment in the USA") +
    ggtitle("Separating thetaf into a modelling function with a forecast method",
            subtitle = "The two methods give identical results")

