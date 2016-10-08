#
# Bee Swarm Tutorial
# Packages used: beeswarm by Aron Charles Eklund
#

install.packages("beeswarm")
library("beeswarm")
options(scipen=999)

# Load data
workers <- read.csv("data/income-sample-2014.tsv", sep="\t", stringsAsFactors=FALSE)

# Traditional
hist(workers$INCTOT, breaks=30)
stripchart(workers$INCTOT)

# Beeswarm
beeswarm(workers$INCTOT)

# Beeswarm for categories
beeswarm(INCTOT ~ main_occ, data=workers, method="swarm")
beeswarm(INCTOT ~ main_occ, data=workers, method="center")
beeswarm(INCTOT ~ main_occ, data=workers, method="hex")
beeswarm(INCTOT ~ main_occ, data=workers, method="square")


# Beeswarm options
beeswarm(INCTOT ~ main_occ, data=workers, col=sample(colors(), 27), pch=19, method="swarm", cex=0.5)

# More parameters
par(las=1)
beeswarm(INCTOT ~ main_occ, data=workers, col=sample(colors(), 27), pch=19, method="swarm", cex=0.5, horizontal=TRUE, xlab="Annual Income, Dollars", ylab="Occupation Category", main="Distribution of Income, by Occupation Caetgory", labels=c(LETTERS, "AA"), bty="n")

