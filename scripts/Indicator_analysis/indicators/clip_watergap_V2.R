########## Watergap PCRGLOB ##########
# Author: Myrthe Leijnse

# Data: PCRGLOB wb
# Spatialres: 5min
# Timeref: 1980-2019
# Definition: total gross water demand - total water abstraction (incl. fossil gw and desalination)

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
  clip <- mask(crop(b, extent(shp[[shpcount]])), shp[[shpcount]])
  df <- data.frame(value = cellStats(clip, "mean"), 
                   year = as.numeric(format(strptime(getZ(clip), "%Y-%m-%d"), format = "%Y")),
                   variable = variable,
                   unit = unit,
                   hotspot = names(shp[shpcount]),
                   value.ref = cellStats(clip, "mean")) #initialize
  for (i in 1:nrow(df)){
    df$value.ref[i] <- df$value.ref[i]/df$value[1]
  }
  return (df)
}

### Execution ###
df_watergap <- ncdf_to_df_mean("WSI/totalWaterGap_annuaTot.nc", shp_list, "Watergap", "m", 1)
df_watergap$year <- seq(1980, 2019, 1)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_mean("WSI/totalWaterGap_annuaTot.nc", shp_list, "Watergap", "m", i)
  df$year <- seq(1980, 2019, 1)
  df_watergap <- rbind(df_watergap, df)
}

### Write csv ###
write.csv(df_watergap, "Indicators/Indicator_tables/watergap_1980_2019.csv", row.names=F)

### Plotting ###
ggplot() + geom_line(data = df_watergap, aes(year, value.ref, group=hotspot, color = hotspot), lwd=2) +
  ggtitle("Mean annual watergap per hotspot 1980-2019") +
  labs(y="m") +
  theme_bw()
