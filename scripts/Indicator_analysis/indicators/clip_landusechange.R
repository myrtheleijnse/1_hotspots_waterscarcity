########## MODIS Land Use change ##########
# Author: Myrthe Leijnse

# Data: MCD12Q1.006 MODIS Land Cover Type Yearly Global 500m; LC_Type1 Classes
# Timeref: 2001-2010
# Classed map with 
# 12: Croplands: at least 60% of area is cultivated cropland - "Cropland area" 
# 13: Urban and Built-up Lands: at least 30% impervious surface area including building materials, asphalt and vehicles - "Urban area"
#TODO extend to 2019 (earlier not available)

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
landuse_2001_2010 <- function(shp, array, t, lon, lat, variable, class) {
  slice2001 <- slice_to_raster(array[,,1])
  slice2002 <- slice_to_raster(array[,,2])
  slice2003 <- slice_to_raster(array[,,3])
  slice2004 <- slice_to_raster(array[,,4])
  slice2005 <- slice_to_raster(array[,,5])
  slice2006 <- slice_to_raster(array[,,6])
  slice2007 <- slice_to_raster(array[,,7])
  slice2008 <- slice_to_raster(array[,,8])
  slice2009 <- slice_to_raster(array[,,9])
  slice2010 <- slice_to_raster(array[,,10])
  #stack
  raster <- stack(slice2001, slice2002, slice2003, slice2004, slice2005, slice2006, slice2007, slice2008, slice2009, slice2010)
  names(raster) <- c(sprintf(paste0(variable," %d"),seq(2001, 2010, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #calculating area of land use class
  area_list <- list()
  for (i in 1:nlayers(clip)) {
    area <- clip[[i]]
    area[area < class] <- NA 
    area[area > class] <- NA
    cellsize <- area(area, na.rm=T, weights=F)
    cellsize <- cellsize[!is.na(cellsize)]
    area_list[[i]] <- length(cellsize)*median(cellsize) # in km2
  }
  #make data frame area and add columns
  df <- do.call(rbind.data.frame, area_list)
  colnames(df) <- c("value")
  df$year = c(seq(2001, 2010, 1))
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  df$Variable <- variable
  df$unit <- "km2"
  df$hotspot <- shp$hotspot[1]
  df$value.ref <- df$value
  #compare to reference year (2001)
  for (i in 1:nrow(df)){
    df$value.ref[i] <- df$value[i]/df$value[1]
  }
  return(df)
}

### Reading Data ###
landuse_2001_2010.array <- nc_to_array("Indicators/LandUse/MODIS_2001_2010/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg_fv0.02.nc4", "landcover_igbp")
t <- nc_t("Indicators/LandUse/MODIS_2001_2010/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg_fv0.02.nc4")
lon <- nc_lon("Indicators/LandUse/MODIS_2001_2010/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg_fv0.02.nc4")
lat <- nc_lat("Indicators/LandUse/MODIS_2001_2010/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg_fv0.02.nc4")

### Execution ###
df_croplandarea <- landuse_2001_2010(shp_list[[1]], landuse_2001_2010.array, t, lon, lat, "Cropland area", 12)
for (i in 2:length(shp_list)){
  df <- landuse_2001_2010(shp_list[[i]], landuse_2001_2010.array, t, lon, lat, "Cropland area", 12)
  df_croplandarea <- rbind(df_croplandarea, df)
}
df_urbanarea <- landuse_2001_2010(shp_list[[1]], landuse_2001_2010.array, t, lon, lat, "Urban area", 13)
for (i in 2:length(shp_list)){
  df <- landuse_2001_2010(shp_list[[i]], landuse_2001_2010.array, t, lon, lat, "Urban area", 13)
  df_urbanarea <- rbind(df_urbanarea, df)
}

### Plotting ###