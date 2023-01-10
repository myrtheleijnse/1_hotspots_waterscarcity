########## WaterUse PCRGLOBWB to dataframe ##########
# Author: Myrthe Leijnse

# Data: PCRGLOBWB 2.0 
# Spatialres: 5 arcmin
# Timeref: 1960-2019 (domestic, industrial); 1961-2014 (livestock)
# Units: m/day (domestic, industrial); m (livestock)
# ! units m/day correct!??

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("E:/1_hotspots_waterscarcity/Data/")

### Functions ###
ncdf_to_df_sum <- function(ncinput, shp, varname, variable, unit, shpcount, startyear, endyear){
  b <- brick(ncinput, varname = varname)
  clip <- mask(crop(b, extent(shp[[shpcount]])), shp[[shpcount]])
  df <- data.frame(value = cellStats(clip, "sum"), 
                   year = seq(startyear, endyear, 1),
                   variable = variable,
                   unit = unit,
                   hotspot = names(shp[shpcount]),
                   value.ref = cellStats(clip, "sum")) #initialize
  for (i in 1:nrow(df)){
    df$value.ref[i] <- df$value.ref[i]/df$value[1]
  }
  rownames(df) <- NULL
  return (df)
}

### Execution ###
df_domesticWaterUse <- ncdf_to_df_sum("Indicators/SectoralWithdrawal/domestic_water_demand_historical_annual_1960-2019.nc", shp_list,
                                      "domesticGrossDemand", "Domestic gross demand", "m/d", 1, 1960, 2019)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_sum("Indicators/SectoralWithdrawal/domestic_water_demand_historical_annual_1960-2019.nc", shp_list,
                       "domesticGrossDemand", "Domestic gross demand", "m/d", i, 1960, 2019)
  df_domesticWaterUse <- rbind(df_domesticWaterUse, df)
}
df_industryWaterUse <- ncdf_to_df_sum("Indicators/SectoralWithdrawal/industry_water_demand_historical_annual_1960-2019.nc", shp_list,
                                      "industryGrossDemand", "Industry gross demand", "m/d", 1, 1960, 2019)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_sum("Indicators/SectoralWithdrawal/industry_water_demand_historical_annual_1960-2019.nc", shp_list,
                       "industryGrossDemand", "Industry gross demand", "m/d", i, 1960, 2019)
  df_industryWaterUse <- rbind(df_industryWaterUse, df)
}
df_livestockWaterUse <- ncdf_to_df_sum("Indicators/SectoralWithdrawal/livestock_water_demand_historical_annual_1960-2014.nc", shp_list,
                                       "livestockGrossDemand", "Livestock gross demand", "m", 1, 1961, 2014)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_sum("Indicators/SectoralWithdrawal/livestock_water_demand_historical_annual_1960-2014.nc", shp_list,
                       "livestockGrossDemand", "Livestock gross demand", "m", i, 1961, 2014)
  df_livestockWaterUse <- rbind(df_livestockWaterUse, df)
}

### Write CSV ###
write.csv(df_domesticWaterUse, "Indicators/Indicator_tables/domesticWaterUse_1960_2019.csv", row.names=F)
write.csv(df_industryWaterUse, "Indicators/Indicator_tables/industryWaterUse_1960_2019.csv", row.names=F)
write.csv(df_livestockWaterUse, "Indicators/Indicator_tables/livestock_1961_2014.csv", row.names=F)