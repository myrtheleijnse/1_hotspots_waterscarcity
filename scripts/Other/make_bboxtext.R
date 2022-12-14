### obtain boxes of shapefiles
library(sf)
library(raster)
library(rlist)

setwd("C:/Users/5738091/Documents/2022_PhD/Data/Water_provinces/Hotspot_Shapefiles/")

ff <- list.files(getwd(), pattern="\\.shp$", full.names=T)
x <- lapply(ff, shapefile)
bbox <- lapply(x, bbox)
bbox <- lapply(bbox, sort, decreasing=T)
file.create("bbox.txt")
lapply(bbox, write, "bbox.txt", append=T, ncolumns=19, sep = ",")
test <- append(sub(".*/", "", ff), bbox[])
