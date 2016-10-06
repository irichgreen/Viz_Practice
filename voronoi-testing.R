install.packages("deldir", dependencies=TRUE)

library(deldir)
source("lib/latlong2state.R")

#
# Play data.
#

# Generate points
x <- rnorm(500, 0, 1.5)
y <- rnorm(500, 0, 1)

# Calculate tesslation
vtess <- deldir(x, y)

# Voronoi
plot(x, y, type="n", asp=1)
points(x, y, pch=20, col="red", cex=0.5)
plot(vtess, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)

# Delaunay triangulation
plot(x, y, type="n", asp=1)
plot(vtess, wlines="triang", wpoints="none", number=FALSE, add=TRUE, lty=1)
points(x, y, pch=20, col="black", cex=0.5)


#
# Some real data now. Same idea.
#

# Load data
airports <- read.csv("data/airport-locations.tsv", sep="\t", stringsAsFactors=FALSE)
airports$state <- latlong2state(airports[,c(2,1)])
airports_contig <- na.omit(airports)

# Projection
library(mapproj)
airports_projected <- mapproject(airports_contig$longitude, airports_contig$latitude, "albers", param=c(39,45))

# Plot
par(mar=c(0,0,0,0))
plot(airports_projected, asp=1, type="n", bty="n", xlab="", ylab="", axes=FALSE)
points(airports_projected, pch=20, cex=0.1, col="red")

# Voronoi
vtess <- deldir(airports_projected$x, airports_projected$y)
plot(vtess, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)

# Delaunay triangulation
vtess <- deldir(airports_projected$x, airports_projected$y)
plot(vtess, wlines="triang", wpoints="none", number=FALSE, add=TRUE, lty=1)
