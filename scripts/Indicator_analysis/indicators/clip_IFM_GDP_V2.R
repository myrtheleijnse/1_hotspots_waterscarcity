########## GDP IFM 1980-2016 ##########
# Author: Myrthe Leijnse

# Data: IFM (?) Murakami & Yamagata (2019), Hoch et al., 2022
# Spatialres: 5 arcmin/10km (also lower resolution available)
# Timeref: 1980-2016
# Definitions: Purchasing power parity (billion US$ 2005 reference year)
# GDP per capita should be around 70,000 US$ in 2019
# Total GDP should be around 3.36 trillion US$ in 2021

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
                   year = as.numeric(seq(1980,2015,1)),
                   variable = variable,
                   unit = unit,
                   hotspot = names(shp[shpcount]),
                   value.ref = cellStats(clip, "mean")) #initialize
  for (i in 1:nrow(df)){
    df$value.ref[i] <- df$value.ref[i]/df$value[1]
  }
  rownames(df) <- NULL
  return (df)
}

### Execution ###
df_GDP <- ncdf_to_df_mean("Indicators/GDP/GDP_Murakami&Yamagata/gdp_historic_5min.nc", shp_list, "GDP", "PPP $US in 2005 year rate", 1)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_mean("Indicators/GDP/GDP_Murakami&Yamagata/gdp_historic_5min.nc", shp_list, "GDP", "PPP $US in 2005 year rate", i)
  df_GDP <- rbind(df_GDP, df)
  print(i)
}

### Write CSV ###
write.csv(df_GDP, "Indicators/Indicator_tables/GDP_1980_2015.csv", row.names=F)

