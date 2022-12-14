### GDP dataframe California
#local data source bea.gov
data.frame(year = c(seq(2016, 2019, 1)), GDP = c(24278946,	25417693,	26435763,	27393434)) #needs to be divided total of capita

library(raster)
library(ncdf4)
library(sf)

setwd("C:/Users/5738091/Documents/2022_PhD/Data/")

#functions
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
  lon <- ncvar_get(nc_data, "longitude")
  nc_close(nc_data)
  return(lon)
}
nc_lat <- function(ncfile){
  nc_data <- nc_open(ncfile)
  lat <- ncvar_get(nc_data, "latitude", verbose = F)
  nc_close(nc_data)
  return(lat)
}
slice_to_raster <- function(slice){
  r <- raster(t(slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  return(r)
}
GDP2010_2015 <- function(shp, GDP.array, t, lon, lat){
  slice2010 <- slice_to_raster(GDP.array[,,21])
  slice2011 <- slice_to_raster(GDP.array[,,22])
  slice2012 <- slice_to_raster(GDP.array[,,23])
  slice2013 <- slice_to_raster(GDP.array[,,24])
  slice2014 <- slice_to_raster(GDP.array[,,25])
  slice2015 <- slice_to_raster(GDP.array[,,26])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015)
  names(raster) <- c(sprintf("GDP%d",seq(2010, 2015, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  #mean per capita per year
  return(data.frame(GDP = cellStats(clip, "mean"), year = c(seq(2010, 2015, 1)))) 
}

#read data
GDP.array <- nc_to_array("Indicators/Population/GDP_PPP_1990_2015_5arcmin_v2.nc", "GDP_PPP")
t <- nc_t("Indicators/Population/GDP_PPP_1990_2015_5arcmin_v2.nc")
lon <- nc_lon("Indicators/Population/GDP_PPP_1990_2015_5arcmin_v2.nc")
lat <- nc_lat("Indicators/Population/GDP_PPP_1990_2015_5arcmin_v2.nc")
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

df2 <- GDP2010_2015(shp, GDP.array, t, lon, lat)
