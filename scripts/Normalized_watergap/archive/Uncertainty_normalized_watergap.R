library(raster)

setwd("C:/Users/5738091/Documents/2022_PhD/Data/WSI")

gapdischarge <- raster("TIF/output_hotspot_certainty/gapdischarge_above05_and2000.tif")
gapdemand <- raster("TIF/output_hotspot_certainty/gapdemand_above05_and2000.tif")
gapextreme <- raster("TIF/output_hotspot_certainty/gape_above01_and2000.tif")

hotspot_uncertainty <- gapdischarge + gapextreme
