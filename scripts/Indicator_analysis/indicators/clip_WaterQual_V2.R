########## Water quality ##########
# Author: Myrthe Leijnse

# # Data: SPEIbase v2.7 based on CRU TS 4.05 dataset
# Spatialres: 5 arcmin/10km
# Timeref: 1980-2019
# Definitions:
# Total dissolved solids (TDS)/salinity; threshold: 2100 mg/L
# Biological oxygen demand (BOD); threshold: 8 mg/L
# Pathogenes/fecal coliform concentration (FC); threshold: cfu/100ml
# Water temperature (degrees Celsius)

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)
require(gridExtra)

### Directory ###
setwd("E:/1_hotspots_waterscarcity/Data")

### Functions ###
ncdf_to_df_mean <- function(ncinput, shp, variable, unit, shpcount){
  b <- brick(ncinput)
  clip <- mask(crop(b, extent(shp[[shpcount]])), shp[[shpcount]])
  df <- data.frame(value = cellStats(clip, "mean"), 
                   year = seq(1980, 2019, 1),
                   variable = variable,
                   unit = unit,
                   hotspot = names(shp[shpcount]),
                   value.ref = cellStats(clip, "mean")) #initialize
  for (i in 1:nrow(df)){
    df$value.ref[i] <- df$value[i]/df$value[1]
  }
  rownames(df) <- NULL
  return (df)
}

### Execution ###
df_organic <- ncdf_to_df_mean("Indicators/waterqual/organic_yearlyAvg_1980_2019.nc", shp_list, "BOD", "mg/L", 1)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_mean("Indicators/waterqual/organic_yearlyAvg_1980_2019.nc", shp_list, "BOD", "mg/L", i)
  df_organic <- rbind(df_organic, df)
}
df_watTemp <- ncdf_to_df_mean("Indicators/waterqual/waterTemperature_yearlyAvg_1980_2019.nc", shp_list, "Water temperature", "°C", 1)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_mean("Indicators/waterqual/waterTemperature_yearlyAvg_1980_2019.nc", shp_list, "Water temperature", "°C", i)
  df_watTemp <- rbind(df_watTemp, df)
}
df_pathogen <- ncdf_to_df_mean("Indicators/waterqual/pathogen_yearlyAvg_1980_2019.nc", shp_list, "FC", "cfu/100ml", 1)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_mean("Indicators/waterqual/pathogen_yearlyAvg_1980_2019.nc", shp_list, "FC", "cfu/100ml", i)
  df_pathogen <- rbind(df_pathogen, df)
}
df_salinity <- ncdf_to_df_mean("Indicators/waterqual/salinity_yearlyAvg_1980_2019.nc", shp_list, "TDS", "mg/L", 1)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_mean("Indicators/waterqual/salinity_yearlyAvg_1980_2019.nc", shp_list, "TDS", "mg/L", i)
  df_salinity <- rbind(df_salinity, df)
}
df_waterQual <- rbind(df_organic, df_watTemp, df_pathogen, df_salinity)

### Write csv ###
write.csv(df_organic, "Indicators/Indicator_tables/DynQual_organic_1980_2019.csv", row.names=F)
write.csv(df_watTemp, "Indicators/Indicator_tables/DynQual_waterTemperature_1980_2019.csv", row.names=F)
write.csv(df_pathogen, "Indicators/Indicator_tables/DynQual_pathogen_1980_2019.csv", row.names=F)
write.csv(df_salinity, "Indicators/Indicator_tables/DynQual_salinity_1980_2019.csv", row.names=F)
write.csv(df_waterQual, "Indicators/Indicator_tables/DynQual_waterQuality_1980_2019.csv", row.names=F)

#or count of pixels with exceeding thresholds/area exceeding threshold