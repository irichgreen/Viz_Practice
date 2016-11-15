library(mapmate)
library(dplyr)
library(RColorBrewer)
pal <- rev(brewer.pal(11, "RdYlBu"))

data(annualtemps)
data(borders)
data(bathymetry)

id <- "frameID"
temps <- mutate(annualtemps, frameID = Year - min(Year) + 1) %>% filter(frameID == 
                                                                            1)  # subset to first frame
brdrs <- mutate(borders, frameID = 1)
bath <- mutate(bathymetry, frameID = 1)

save_map(temps, id = id, ortho = FALSE, col = "dodgerblue", type = "points", 
         save.plot = FALSE, return.plot = TRUE)
save_map(temps, id = id, col = "#FF4500", type = "points", save.plot = FALSE, 
         return.plot = TRUE)
save_map(bath, id = id, type = "points", save.plot = FALSE, return.plot = TRUE)
save_map(brdrs, id = id, type = "maplines", save.plot = FALSE, return.plot = TRUE)
save_map(brdrs, id = id, lon = -70, lat = 40, rotation.axis = 0, type = "maplines", 
         save.plot = FALSE, return.plot = TRUE)
save_map(bath, z.name = "z", id = id, col = pal, type = "maptiles", save.plot = FALSE, 
         return.plot = TRUE)

save_map(bath, z.name = "z", id = id, col = pal, type = "density", save.plot = FALSE, 
         return.plot = TRUE)
save_map(bath, z.name = "z", id = id, col = pal, type = "density", contour = "overlay", 
         save.plot = FALSE, return.plot = TRUE)
save_map(bath, z.name = "z", id = id, col = pal, type = "density", contour = "only", 
         save.plot = FALSE, return.plot = TRUE)

save_map(temps, id = id, col = "red", type = "points", contour = "overlay", 
         save.plot = FALSE, return.plot = TRUE)
save_map(temps, id = id, col = "blue", type = "points", contour = "only", save.plot = FALSE, 
         return.plot = TRUE)

save_map(temps, z.name = "z", id = id, col = pal, type = "density", contour = "overlay", 
         save.plot = FALSE, return.plot = TRUE)
save_map(temps, id = id, col = pal, type = "density", contour = "overlay", save.plot = FALSE, 
         return.plot = TRUE)
save_map(bath, id = id, lon = -70, lat = 50, col = pal, type = "density", contour = "overlay", 
         save.plot = FALSE, return.plot = TRUE)

save_map(temps, z.name = "z", id = id, col = pal, type = "density", contour = "overlay", 
         density.geom = "tile", save.plot = FALSE, return.plot = TRUE)
save_map(temps, z.name = "z", id = id, col = pal, type = "density", contour = "overlay", 
         density.geom = "polygon", save.plot = FALSE, return.plot = TRUE)
save_map(bath, z.name = "z", id = id, col = pal, type = "density", contour = "overlay", 
         density.geom = "tile", save.plot = FALSE, return.plot = TRUE)
save_map(bath, z.name = "z", id = id, col = pal, type = "density", contour = "overlay", 
         density.geom = "polygon", save.plot = FALSE, return.plot = TRUE)

save_map(temps, id = id, col = pal, type = "density", contour = "overlay", density.geom = "tile", 
         save.plot = FALSE, return.plot = TRUE)
save_map(temps, id = id, col = pal, type = "density", contour = "overlay", density.geom = "polygon", 
         save.plot = FALSE, return.plot = TRUE)
save_map(temps, id = id, col = pal, type = "density", ortho = FALSE, contour = "overlay", 
         density.geom = "tile", save.plot = FALSE, return.plot = TRUE)
save_map(temps, id = id, col = pal, type = "density", ortho = FALSE, contour = "overlay", 
         density.geom = "polygon", save.plot = FALSE, return.plot = TRUE)


# polygons
#install.packages("rworldmap"); install.packages("rworldxtra"); install.packages("rworldmap")
library(rworldmap)
library(rworldxtra)  # required for 'high' resolution map
library(maptools)  # required for fortify to work
# also recommend installing rgeos

spdf <- joinCountryData2Map(countryExData, mapResolution = "high")
spdf@data$id <- rownames(spdf@data)
bio <- ggplot2::fortify(spdf, region = "id") %>% left_join(subset(spdf@data, 
                                                                  select = c(id, BIODIVERSITY)), by = "id") %>% mutate(frameID = 1) %>% rename(lon = long)

# raster layer
library(raster)
proj4 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +to wgs84=0,0,0"
z <- "BIODIVERSITY"
# 1-degree resolution, still somewhat coarse
r <- raster(extent(-180, 180, -90, 90), nrow = 180, ncol = 360, proj4)
bio2 <- rasterize(spdf, r, field = z) %>% rasterToPoints %>% tbl_df() %>% setNames(c("lon", 
                                                                                     "lat", z)) %>% mutate(frameID = 1)

clrs <- c("royalblue", "purple", "orange", "yellow")
save_map(bio, z.name = z, id = id, lon = -10, lat = 20, col = pal, type = "polygons", 
         save.plot = FALSE, return.plot = TRUE)
save_map(bio2, z.name = z, id = id, lon = -10, lat = 20, col = pal, type = "maptiles", 
         save.plot = FALSE, return.plot = TRUE)
