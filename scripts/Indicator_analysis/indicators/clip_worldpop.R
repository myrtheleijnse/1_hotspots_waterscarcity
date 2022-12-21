### population count Worldpop 1km res, 2010-2019, california only

library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

setwd("E:/1_hotspots_waterscarcity/data")

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
  r <- flip(r)
  return(r)
}
pop_2010_2019 <- function(shp, pop_2010_2019.array, t, lon, lat, variable) {
  #monthly average to yearly sum slices
  slice2010 <- slice_to_raster(pop_2010_2019.array[,,1])
  slice2011 <- slice_to_raster(pop_2010_2019.array[,,2])
  slice2012 <- slice_to_raster(pop_2010_2019.array[,,3])
  slice2013 <- slice_to_raster(pop_2010_2019.array[,,4])
  slice2014 <- slice_to_raster(pop_2010_2019.array[,,5])
  slice2015 <- slice_to_raster(pop_2010_2019.array[,,6])
  slice2016 <- slice_to_raster(pop_2010_2019.array[,,7])
  slice2017 <- slice_to_raster(pop_2010_2019.array[,,8])
  slice2018 <- slice_to_raster(pop_2010_2019.array[,,9])
  slice2019 <- slice_to_raster(pop_2010_2019.array[,,10])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016, slice2017, slice2018, slice2019)
  names(raster) <- c(sprintf("Population %d",seq(2010, 2019, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #mean per capita per year
  df <- data.frame(Population = cellStats(clip, "sum"), year = c(seq(2010, 2019, 1)))
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  return(df)
}

#read data
pop_2010_2019.array <- nc_to_array("Indicators/Population/worldpop/ca_ppp_2010-2019.nc", "Band1")
pop_2000.array <- nc_to_array("Indicators/Population/worldpop/ppp_2000_1km.nc", "Band1") 
t <- nc_t("Indicators/Population/worldpop/ca_ppp_2010-2019.nc")
lon <- nc_lon("Indicators/Population/worldpop/ca_ppp_2010-2019.nc")
lat <- nc_lat("Indicators/Population/worldpop/ca_ppp_2010-2019.nc")
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

df_pop <- pop_2010_2019(shp, pop_2010_2019.array, t, lon, lat, pop)
ggplot() + geom_line(data = df_pop,aes(year, Population, group=1), color= "red", lwd=2) +
  ggtitle("Total population count in California 2010-2019") +
  theme_bw()
#total population count in California 2010-2019 according to Worldpop