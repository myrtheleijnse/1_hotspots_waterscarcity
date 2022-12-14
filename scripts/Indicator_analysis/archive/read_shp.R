### open shapefile ###
library(sf)
library(ggplot2)

setwd("C:/Users/5738091/OneDrive - Universiteit Utrecht/Documents/Data/Water provences/Hotspot_Shapefiles")

Cali_shape <- st_read("California.shp")
ggplot()+
  geom_sf(data=Cali_shape, size=3, color = "black", fill = "steelblue")+
  coord_sf()
