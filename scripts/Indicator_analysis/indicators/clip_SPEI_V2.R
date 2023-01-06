########## SPEI ##########
# Author: Myrthe Leijnse

# Data: SPEIbase v2.7 based on CRU TS 4.05 dataset
# Spatialres: 0.5deg/50km
# Timeref: 1901-2019
# Definition: Standardized Precipitation Evaporation Index; 12month SPEI; January 

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("E:/1_hotspots_waterscarcity/Data")

### Functions ###
ncdf_to_df_mean <- function(ncinput, shp, variable, unit, shpcount){
  b <- brick(ncinput)
  stack <- b[[709]]
  for (i in seq(721, 1428, 12)){
    stack <- stack(stack, b[[i]])
  } 
  stack <- setZ(stack, as.Date('1959-1-1') + seq(366, 22280.5, 365.25))
  clip <- mask(crop(stack, extent(shp[[shpcount]])), shp[[shpcount]])
  df <- data.frame(value = cellStats(clip, "mean"), 
                   year = as.numeric(format(strptime(getZ(clip), "%Y-%m-%d"), format = "%Y")),
                   variable = variable,
                   unit = unit,
                   hotspot = names(shp[shpcount]))
  return (df)
}

### Execution ###
df_SPEI <- ncdf_to_df_mean("Indicators/SPEI/spei12month.nc", shp_list, "SPEI", "index", 1)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_mean("Indicators/SPEI/spei12month.nc", shp_list, "SPEI", "index", i)
  df_SPEI <- rbind(df_SPEI, df)
}
rownames(df_SPEI) <- seq(1,nrow(df_SPEI),1)

### Write csv ###
write.csv(df_SPEI, "Indicators/Indicator_tables/CRU_SPEI_1960_2019.csv", row.names=F)