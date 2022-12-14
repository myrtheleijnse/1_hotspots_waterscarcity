### PDSI dataframe California
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

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
  return(r)
}
PDSI_2010_2019 <- function(shp, PDSI_1901_2020.array, t, lon, lat, variable) {
  #monthly average to yearly sum slices
  slice2010 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1321:1332], c(1,2), mean))
  slice2011 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1333:1344], c(1,2), mean))
  slice2012 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1345:1356], c(1,2), mean))
  slice2013 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1357:1368], c(1,2), mean))
  slice2014 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1369:1380], c(1,2), mean))
  slice2015 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1381:1392], c(1,2), mean))
  slice2016 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1393:1404], c(1,2), mean))
  slice2017 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1405:1416], c(1,2), mean))
  slice2018 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1417:1428], c(1,2), mean))
  slice2019 <- slice_to_raster(apply(PDSI_1901_2020.array[,,1429:1440], c(1,2), mean))
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016, slice2017, slice2018, slice2019)
  names(raster) <- c(sprintf("PDSI%d",seq(2010, 2019, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #mean per capita per year
  df <- data.frame(PDSI = cellStats(clip, "mean"), year = c(seq(2010, 2019, 1)))
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  return(df)
}

#read data
PDSI_1901_2020.array <- nc_to_array("Indicators/PDSI/scPDSI.cru_ts4.05early1.1901.2020.cal_1901_20.bams.2021.GLOBAL.1901.2020.nc", "scpdsi")
t <- nc_t("Indicators/PDSI/scPDSI.cru_ts4.05early1.1901.2020.cal_1901_20.bams.2021.GLOBAL.1901.2020.nc")
lon <- nc_lon("Indicators/PDSI/scPDSI.cru_ts4.05early1.1901.2020.cal_1901_20.bams.2021.GLOBAL.1901.2020.nc")
lat <- nc_lat("Indicators/PDSI/scPDSI.cru_ts4.05early1.1901.2020.cal_1901_20.bams.2021.GLOBAL.1901.2020.nc")
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

df_PDSI <- PDSI_2010_2019(shp, PDSI_1901_2020.array, t, lon, lat, PDSI)
ggplot() + geom_line(data = df_PDSI,aes(year, PDSI, group=1), color= "red", lwd=2) + theme_bw()
