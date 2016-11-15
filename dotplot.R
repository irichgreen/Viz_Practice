## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("clusterProfiler")

library(clusterProfiler)
data(gcSample)
x=compareCluster(gcSample, fun='enrichDO')
dotplot(x, showCategory=5, includeAll=FALSE)
dotplot(x, showCategory=5)


source("http://bioconductor.org/biocLite.R")
pkgs <- rownames(installed.packages())
biocLite(pkgs, type="source")


install.packages("sp")
install.packages("BelgiumMaps.StatBel", repos = "http://www.datatailor.be/rcube", type = "source")
vignette("BelgiumMaps_AdministrativeAreas_StatBel", package = "BelgiumMaps.StatBel")
