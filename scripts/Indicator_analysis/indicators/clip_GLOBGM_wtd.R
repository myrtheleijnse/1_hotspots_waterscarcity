########## GWdepth: wtd globgm ##########
# Author: Myrthe Leijnse

### Libraries ###
library(raster)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("C:/Users/5738091/Documents/2022_PhD/Data/")

### Functions ###

### Reading Data ###
ff <- list.files("./", pattern = ".tif$", full.names=T)
list_rasters <- lapply(ff, raster)

### Execution ###

# rasterize tifs
raster_list <- list(raster(ls_files[[1]]))
for i in 2:12{
  raster_list <- append(raster_list, raster(ls_files([[i]])))
}
raster_2010 <- stack(raster_list)

# stack rasters per year

# 
for i in ls_files{
  
}

### Plotting ###

### test ###
#read data
wtd <- raster("Indicators/GWDepth/globgm-wtd-ss.tif")

clip <- mask(crop(wtd, extent(shp)), shp)
plot(clip)
