library("data.table")
library("replyr")

data("iris", package= "datasets")
iris.dt <- data.table(iris)

# non-standard evaluation, column names hard-coded
iris.dt[, mean(Sepal.Length), by=Species]

# standard evaluation, column names parameterized
let(
    list(GROUPCOL='Species', DATACOL='Sepal.Length'),
    iris.dt[, mean(DATACOL), by=GROUPCOL]
)
