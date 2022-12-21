########## SPEI ##########
# Author: Myrthe Leijnse

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("E:/1_hotspots_waterscarcity/Data")

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
  lon <- ncvar_get(nc_data, attributes(nc$dim)$names[2])
  nc_close(nc_data)
  return(lon)
}
nc_lat <- function(ncfile){
  nc_data <- nc_open(ncfile)
  lat <- ncvar_get(nc_data, attributes(nc$dim)$names[3], verbose = F)
  nc_close(nc_data)
  return(lat)
}
slice_to_raster <- function(slice){
  r <- raster(t(slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r)
  return(r)
}
SPEI_2010_2019 <- function(shp, array, t, lon, lat, variable) {
  #monthly average to yearly sum slices
  slice2010 <- slice_to_raster(apply(array[,,1309:1320], c(1,2), mean))
  slice2011 <- slice_to_raster(apply(array[,,1321:1332], c(1,2), mean))
  slice2012 <- slice_to_raster(apply(array[,,1333:1344], c(1,2), mean))
  slice2013 <- slice_to_raster(apply(array[,,1345:1356], c(1,2), mean))
  slice2014 <- slice_to_raster(apply(array[,,1357:1368], c(1,2), mean))
  slice2015 <- slice_to_raster(apply(array[,,1369:1380], c(1,2), mean))
  slice2016 <- slice_to_raster(apply(array[,,1381:1392], c(1,2), mean))
  slice2017 <- slice_to_raster(apply(array[,,1393:1404], c(1,2), mean))
  slice2018 <- slice_to_raster(apply(array[,,1405:1416], c(1,2), mean))
  slice2019 <- slice_to_raster(apply(array[,,1417:1428], c(1,2), mean))
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016, slice2017, slice2018, slice2019)
  names(raster) <- c(sprintf("SPEI%d",seq(2010, 2019, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #mean per capita per year
  df <- data.frame(value = cellStats(clip, "mean"), year = c(seq(2010, 2019, 1)))
  #add columns
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  df$Variable <- variable
  df$unit <- "m/d"
  df$hotspot <- shp$hotspot[1]
  return(df)
}

### Reading Data ###
SPEI_1901_2020.array <- nc_to_array("Indicators/SPEI/spei01month.nc", "spei") 
t <- nc_t("Indicators/SPEI/spei12month.nc")
lon <- nc_lon("Indicators/SPEI/spei12month.nc")
lat <- nc_lat("Indicators/SPEI/spei12month.nc")

### Execution ###
# annual average SPEI on monthly basis (1month spei)
df_SPEI <- SPEI_2010_2019(shp_list[[1]], SPEI_1901_2020.array, t, lon, lat, "SPEI")
for (i in 2:length(shp_list)){
  df <- SPEI_2010_2019(shp_list[[i]], SPEI_1901_2020.array, t, lon, lat, "SPEI")
  df_SPEI <- rbind(df_SPEI, df)
}
rownames(df_SPEI) <- seq(1,nrow(df_SPEI), 1)

### Plotting ###
# ggplot() + geom_line(data = df_SPEI,aes(year, SPEI, group=1), color= "red", lwd=2) + theme_bw()