#devtools::install_github("hafen/trelliscopejs")
#install.packages("gapminder")

library(trelliscopejs)
library(ggplot2)
library(plotly)
library(gapminder)

qplot(year, lifeExp, data = gapminder) +
    xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
    facet_trelliscope(~ country + continent,
                      nrow = 2, ncol = 6, width = 300, as_plotly = TRUE)


qplot(cty, hwy, data = mpg) +
    facet_trelliscope(~ class + manufacturer)

library(dplyr)
library(rbokeh)

ggplot2::mpg %>%
    group_by(class, manufacturer) %>%
    summarise(
        panel = panel(
            figure(xlab = "City mpg", ylab = "Highway mpg") %>%
                ly_points(cty, hwy))) %>%
    trelliscope(name = "city_vs_highway_mpg_bk")


library(trelliscopejs)
library(ggplot2)
library(dplyr)

glimpse(mpg)
# without facet_trelliscope:
qplot(cty, hwy, data = mpg) +
    geom_abline(alpha = 0.5) +
    xlim(7, 37) + ylim(9, 47) + theme_bw() +
    facet_wrap(~ manufacturer + class, nrow = 4, ncol = 8)

# with facet_trelliscope:
qplot(cty, hwy, data = mpg) +
    geom_abline(alpha = 0.5) +
    xlim(7, 37) + ylim(9, 47) + theme_bw() +
    facet_trelliscope(~ manufacturer + class, nrow = 2, ncol = 4)


qplot(class, cty, data = mpg, geom = c("boxplot", "jitter")) +
    ylim(7, 37) + theme_bw()

qplot(class, cty, data = mpg, geom = c("boxplot", "jitter")) +
    facet_trelliscope(~ class, ncol = 7, height = 800, width = 200,
                      state = list(sort = list(sort_spec("cty_mean")))) +
    ylim(7, 37) + theme_bw()


mpg %>%
    group_by(manufacturer, class) %>%
    summarise(
        mean_city_mpg = mean(cty),
        mean_hwy_mpg = mean(hwy))


library(rbokeh)

d <- mpg %>%
    group_by(manufacturer, class) %>%
    summarise(
        mean_city_mpg = mean(cty),
        mean_hwy_mpg = mean(hwy),
        panel = panel(
            figure(xlab = "City mpg", ylab = "Highway mpg",
                   xlim = c(7, 37), ylim = c(9, 47)) %>%
                ly_points(cty, hwy,
                          hover = data_frame(model = paste(year, model),
                                             cty = cty, hwy = hwy))))

d

d %>%
    trelliscope(name = "city_vs_highway_mpg", nrow = 2, ncol = 4)


mpg %>%
    group_by(manufacturer, class) %>%
    summarise(
        mean_city_mpg = cog(mean(cty), desc = "Mean city mpg"),
        mean_hwy_mpg = cog(mean(hwy), desc = "Mean highway mpg"),
        panel = panel(
            figure(xlab = "City mpg", ylab = "Highway mpg",
                   xlim = c(7, 37), ylim = c(9, 47)) %>%
                ly_points(cty, hwy,
                          hover = data_frame(model = paste(year, model),
                                             cty = cty, hwy = hwy)))) %>%
    trelliscope(name = "city_vs_highway_mpg", nrow = 1, ncol = 2)


library(tidyr)

d <- mpg %>%
    group_by(manufacturer, class) %>%
    nest() %>%
    mutate(panel = panels(data,
                          ~ figure(xlab = "City mpg", ylab = "Highway mpg") %>%
                              ly_points(cty, hwy, data = .x)))
d


d %>%
    trelliscope(name = "city_vs_highway_mpg")

# Gapminder data

library(gapminder)

gapminder %>%
    group_by(country, continent) %>%
    nest() %>%
    mutate(panel = panels(data,
                          ~ figure(ylim = c(10, 95), toolbar = NULL, width = 300) %>%
                              ly_points(year, lifeExp, hover = .x, data = .x) %>%
                              theme_axis("x", major_label_orientation = 45))) %>%
    trelliscope(name = "gapminder_lifeexp", nrow = 2, ncol = 6)


qplot(year, lifeExp, data = gapminder) +
    xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
    facet_trelliscope(~ country + continent, nrow = 2, ncol = 7, width = 300)


# Housing data

#install.packages("housingData")
library(housingData)

lm_coefs <- function(x, y)
    coef(lm(y ~ x))

d <- housing %>%
    group_by(county, state) %>%
    summarise(
        slope = cog(lm_coefs(time, medListPriceSqft)[2], desc = "list price slope"),
        mean_list = cog(mean(medListPriceSqft, na.rm = TRUE), desc = "mean list price / sq ft"),
        mean_sold = cog(mean(medSoldPriceSqft, na.rm = TRUE), desc = "mean sold price / sq ft"),
        n_obs = cog(length(which(!is.na(medListPriceSqft))),
                    desc = "number of non-NA list prices"),
        zillow_link = cog_href(
            sprintf("http://www.zillow.com/homes/%s_rb/",
                    gsub(" ", "-", paste(county, state)))[1],
            desc = "zillow link"),
        panel = panel(
            figure(xlab = "time", ylab = "median list price / sq ft", toolbar = NULL) %>%
                ly_points(time, medListPriceSqft,
                          hover = data_frame(time = time, mean_list = medListPriceSqft)))
    ) %>%
    filter(n_obs > 1) %>%
    trelliscope(
        name = "list_vs_time",
        desc = "monthly mean list price vs. time for 2984 US counties from 2008–2016",
        state = list(labels = c("county", "state")))
