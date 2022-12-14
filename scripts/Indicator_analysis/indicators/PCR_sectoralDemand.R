########## WaterUse PCRGLOBWB to dataframe ##########
# Author: Myrthe Leijnse
# Data PCRGLOBWB2.0 2010-2019 annual nc
# units m for livestock and m/day for industry/domestic

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("C:/Users/5738091/Documents/2022_PhD/Data/")

### Functions ###
nc_to_array <- function(ncfile, ncvariable){
  nc_data <- nc_open(ncfile)
  array <- ncvar_get(nc_data, ncvariable)
  nc_close(nc_data)
  return(array)
}
nc_t <- function(ncfile){
  nc_data <- nc_open(ncfile)
  t <- ncvar_get(nc_data, "time")
  nc_close(nc_data)
  return(t)
}
nc_lon <- function(ncfile){
  nc_data <- nc_open(ncfile)
  lon <- ncvar_get(nc_data, "lon")
  nc_close(nc_data)
  return(lon)
}
nc_lat <- function(ncfile){
  nc_data <- nc_open(ncfile)
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  nc_close(nc_data)
  return(lat)
}
slice_to_raster <- function(slice){
  r <- raster(t(slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  return(r)
}
waterUse_2010_2019 <- function(shp, array, t, lon, lat, variable) {
  slice2010 <- slice_to_raster(array[,,1])
  slice2011 <- slice_to_raster(array[,,2])
  slice2012 <- slice_to_raster(array[,,3])
  slice2013 <- slice_to_raster(array[,,4])
  slice2014 <- slice_to_raster(array[,,5])
  slice2015 <- slice_to_raster(array[,,6])
  slice2016 <- slice_to_raster(array[,,7])
  slice2017 <- slice_to_raster(array[,,8])
  slice2018 <- slice_to_raster(array[,,9])
  slice2019 <- slice_to_raster(array[,,10])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016, slice2017, slice2018, slice2019)
  names(raster) <- c(sprintf(paste0(variable," %d"),seq(2010, 2019, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #sum per year of waterdemand per region
  df <- data.frame(value = cellStats(clip, "sum"), year = c(seq(2010, 2019, 1)))
  #add columns
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  df$Variable <- variable
  df$unit <- "m/d"
  df$hotspot <- shp$hotspot[1]
  return(df)
} 
waterUse_2010_2014 <- function(shp, array, t, lon, lat, variable) {
  slice2010 <- slice_to_raster(array[,,1])
  slice2011 <- slice_to_raster(array[,,2])
  slice2012 <- slice_to_raster(array[,,3])
  slice2013 <- slice_to_raster(array[,,4])
  slice2014 <- slice_to_raster(array[,,5])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014)
  names(raster) <- c(sprintf(paste0(variable," %d"),seq(2010, 2014, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #mean per year over area for watergap in region
  df <- data.frame(value = cellStats(clip, "mean"), year = c(seq(2010, 2014, 1)))
  #add columns
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  df$Variable <- variable
  df$unit <- "m"
  df$hotspot <- shp$hotspot[1]
  return(df)
} 

### Reading Data ###
domestic_demand_2010_2019 <- nc_to_array("Indicators/SectoralWithdrawal/domestic_water_demand_2010-2019_5min.nc", "domesticGrossDemand")
industrial_demand_2010_2019 <- nc_to_array("Indicators/SectoralWithdrawal/industrial_water_demand_2010-2019_5min.nc", "industryGrossDemand")
livestock_demand_2010_2014 <- nc_to_array("Indicators/SectoralWithdrawal/livestock_water_demand_2010-2014_5min.nc", "livestockGrossDemand")
t <- nc_t("Indicators/SectoralWithdrawal/domestic_water_demand_2010-2019_5min.nc")
lon <- nc_lon("Indicators/SectoralWithdrawal/domestic_water_demand_2010-2019_5min.nc")
lat <- nc_lat("Indicators/SectoralWithdrawal/domestic_water_demand_2010-2019_5min.nc")
#shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

### Execution ###
# Obtain dataframes for all hotspot regions
# Domestic
df_domesticWaterUse <- waterUse_2010_2019(shp_list[[1]], domestic_demand_2010_2019, t, lon, lat, "Gross domestic demand")
for (i in 2:length(shp_list)){
  df <- waterUse_2010_2019(shp_list[[i]], domestic_demand_2010_2019, t, lon, lat, "Gross domestic demand")
  df_domesticWaterUse <- rbind(df_domesticWaterUse, df)
}
rownames(df_domesticWaterUse) <- seq(1,nrow(df_domesticWaterUse), 1)
# Industrial
df_industrialWaterUse <- waterUse_2010_2019(shp_list[[1]], industrial_demand_2010_2019, t, lon, lat, "Gross industrial demand")
for (i in 2:length(shp_list)){
  df <- waterUse_2010_2019(shp_list[[i]], domestic_demand_2010_2019, t, lon, lat, "Gross domestic demand")
  df_industrialWaterUse <- rbind(df_industrialWaterUse, df)
}
rownames(df_industrialWaterUse) <- seq(1,nrow(df_industrialWaterUse), 1)
# Livestock
df_livestockWaterUse <- waterUse_2010_2014(shp_list[[1]], livestock_demand_2010_2014, t, lon, lat, "Gross livestock demand")
for (i in 2:length(shp_list)){
  df <- waterUse_2010_2014(shp_list[[i]], livestock_demand_2010_2014, t, lon, lat, "Gross livestock demand")
  df_livestockWaterUse <- rbind(df_livestockWaterUse, df)
}
rownames(df_livestockWaterUse) <- seq(1,nrow(df_livestockWaterUse), 1)

### Plotting ###