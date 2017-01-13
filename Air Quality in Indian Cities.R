# Might need to uncomment out and run these next two lines if not already installed:
# devtools::install_github("Ather-Energy/ggTimeSeries")
# devtools::install_github("masalmon/usaqmindia")

#install.packages("ropenaq")
#devtools::install_github("Ather-Energy/ggTimeSeries", force = TRUE)
#devtools::install_github("masalmon/usaqmindia")
#install.packages("forecastHybrid")
# load up functionality
library(ropenaq) # sourced from OpenAQ, see https://github.com/ropensci/ropenaq
library(ggplot2)
library(scales)
library(tidyverse)
library(usaqmindia) # sourced from US Air Quality Monitoring, see https://github.com/masalmon/usaqmindia
library(forecastHybrid)
library(seasonal)   # for Diwali dates
library(ggseas)
library(ggmap)
library(lubridate)
library(RColorBrewer)

data(holiday) # will be using this for "diwali"

#============openaq experiment================
# based on the example on the package page on GitHub
how_many <- attr(aq_measurements(city = "Ahmedabad"), "meta")
n <- how_many$found # unfortunately only goes back to mid 2015
results_ls <- list()
# Limit is 1000 observations at once, so we bring them all in 1000 at a time:
for(i in 1:ceiling(n / 1000)){
    results_ls[[i]] <- aq_measurements(country = "IN", city = "Ahmedabad", 
                                       date_from = "2010-01-01", 
                                       limit = 1000, page = i)   
    cat(i)
}

# convert into a data frame:
results_df <- do.call("rbind", results_ls) %>%
    arrange(dateLocal)

# draw exploratory graph
results_df %>%
    ggplot(aes(x = dateLocal, y = value)) + 
    facet_wrap(~parameter, ncol = 1, scales = "free_y") +
    geom_line() +
    ggtitle("Air pollutants in Ahmedabad, 2016") +
    scale_y_continuous("Value", label = comma) +
    labs(x = "", caption = "Source: OpenAQ")

#ggplotly()

#=========map of india========
cities <- data_frame(
    city = c("Chennai", "Delhi", "Hyderabad", "Kolkata", "Mumbai"),
    lat = c(13.067439, 28.644800, 17.387140, 22.572645, 19.0759837),
    long = c(80.2784700, 77.216721, 78.491684	, 88.363892,	72.8776559)
)

india <- get_map("India", zoom = 5, maptype = "satellite")

ggmap(india) +
    geom_text(data = cities,  aes(x = long, y = lat, label = city), colour = "white") +
    theme_nothing()


#=========US Air Quality Monitoring===========
data("pm25_india")

pm25_india %>%
    ggplot(aes(x = datetime, y = conc)) +
    facet_wrap(~city, ncol = 1) +
    geom_line()


# daily aggregation
pm25_india %>%
    mutate(day = as.Date(datetime)) %>%
    group_by(day, city) %>%
    summarise(avvalue = mean(conc, tr = 0.1, na.rm = TRUE)) %>%
    ggplot(aes(x = day, y = avvalue)) +
    facet_wrap(~city, ncol = 1, scales = "free_y") +
    geom_line()

# monthly aggregation graphic:
pm25_india %>%
    mutate(mon = substring(datetime, 1, 7)) %>%
    group_by(mon, city) %>%
    summarise(avvalue = mean(conc, tr = 0.1, na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(mon = as.Date(paste0(mon, "-15"))) %>%
    ggplot(aes(x = mon, y = avvalue)) +
    facet_wrap(~city, ncol = 1, scales = "free_y") +
    geom_line(colour = "grey50", linetype = 2) +
    stat_stl(s.window = 7, frequency = 12, colour = "steelblue", size = 1.2) +
    ggtitle("Airborne fine particulate matter in Indian cities (PM2.5)",
            subtitle = "Showing original and seasonally adjusted") +
    labs(x = "Seasonal adjustment does not take Diwali into account", 
         y = "Trimmed mean monthly PM2.5 concentration",
         caption = "Data: U.S. Embassy and Consulates in India")

# Create time series objects
pm25_monthly_df <- pm25_india %>%
    mutate(mon = substring(datetime, 1, 7)) %>%
    group_by(mon, city) %>%
    summarise(avvalue = mean(conc, tr = 0.1, na.rm = TRUE)) %>% 
    ungroup() %>%
    spread(city, avvalue) 

pm25_monthly_ts <- pm25_monthly_df %>%
    select(-mon) %>%
    map(function(x){ts(x, start = c(2013, 1), frequency = 12)}) %>%
    do.call(cbind, .)

# Month-plots
par(mfrow = c(2, 3), font.main = 1)
for(i in 1:5){
    monthplot(pm25_monthly_ts[ , i], bty = "l",
              ylab = "",
              main = colnames(pm25_monthly_ts)[i])   
    grid()
}

#-------------Decomposition graphics----------------------   
# named palette assigning a colour to each city
palette <- brewer.pal(5, "Set1")
names(palette) <- c("Delhi", "Kolkata", "Mumbai", "Hyderabad", "Chennai")

# Note that Diwali was in November 2013, October 2014, November 2015 in the relevant years
diwali[114:116]


# Decomposition graphic:
pm25_monthly_ts %>%
    as.data.frame() %>%
    mutate(time = time(pm25_monthly_ts),
           month = time - floor(time)) %>%
    gather(city, value, -time, -month) %>% 
    # imputation: if value is missing, give it mean for that month and city:
    group_by(city, month) %>%
    mutate(value = ifelse(is.na(value), mean(value, na.rm = TRUE), value)) %>%
    ungroup() %>%
    # order city levels for drawing legend (there are better ways for doing this programmatically):
    mutate(city = factor(city, levels = c("Delhi", "Kolkata", "Mumbai", "Hyderabad", "Chennai"))) %>%
    ggsdc(aes(x = time, y = value, colour = city), s.window = 7) +
    geom_line(size = 1) +
    theme(legend.position = "right")+
    labs(colour = "", x = "Vertical lines show the month of Diwali, and any impact is seen in the 'irregular' series.", 
         y = "Trimmed mean monthly PM2.5 concentration",
         caption = "Data: U.S. Embassy and Consulates in India") +
    scale_colour_manual(values = palette) +
    ggtitle("Airborne fine particulate matter in Indian cities (PM2.5)",
            subtitle = "Diwali makes an impact but is part of a broader seasonality")  +
    geom_vline(xintercept = ((month(diwali) - 1) / 12 + year(diwali))[114:116], colour = "grey50")


delhi <- pm25_monthly_ts[ , "Delhi"]

par(mfrow = c(1, 2), bty = "l", font.main = 1)
acf(delhi)
pacf(delhi)

BoxCox.lambda(delhi) # about 0.21.  Note that if we don't use this in forecasting there would be a tendency to forecast < 0

diwalix <- window(genhol(diwali), start = start(delhi), end = end(delhi))

mod0 <- auto.arima(delhi, xreg = diwalix, lambda = 0.21)
mod0



