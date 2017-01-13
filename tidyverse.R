library(tidyverse)

d <- mtcars %>% 
    # Convert `am` to factor and select relevant variables
    mutate(am = factor(am, labels = c("Automatic", "Manual"))) %>% 
    select(am, mpg, hp)

ggplot(d, aes(mpg, hp, color = am)) +
    geom_point()

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Set minsplit = 2 to fit every data point
full_fit <- rpart(am ~ mpg + hp, data = d, minsplit = 2)
prp(full_fit)

set.seed(245)
n <- nrow(d)
train_rows <- sample(seq(n), size = .8 * n)
train <- d[ train_rows, ]
test  <- d[-train_rows, ]

# Define a named list of parameter values
gs <- list(minsplit = c(2, 5, 10),
           maxdepth = c(1, 3, 8)) %>% 
    cross_d() # Convert to data frame grid

gs

mod <- function(...) {
    rpart(am ~ hp + mpg, data = train, control = rpart.control(...))
}

gs <- gs %>% mutate(fit = pmap(gs, mod))
gs

compute_accuracy <- function(fit, test_features, test_labels) {
    predicted <- predict(fit, test_features, type = "class")
    mean(predicted == test_labels)
}

test_features <- test %>% select(-am)
test_labels   <- test$am

gs <- gs %>%
    mutate(test_accuracy = map_dbl(fit, compute_accuracy,
                                   test_features, test_labels))
gs

gs <- gs %>% arrange(desc(test_accuracy), desc(minsplit), maxdepth)
gs

prp(gs$fit[[1]])
