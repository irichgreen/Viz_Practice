#install.packages("samplesize4surveys")
devtools::install_github("psirusteam/samplesize4surveys")
library(samplesize4surveys)

ss4S2(N = 10000, K = 1, cve = 0.05, me = 0.03, DEFF = 2, plot = TRUE)
