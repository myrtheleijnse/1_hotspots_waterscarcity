##### global raster to yearly average value per hotspot region ######

#example state: watergap
#example location: USA

library(raster)
library(ncdf4)
library(sf)
library(tidyverse)
library(readr)
setwd("C:/Users/5738091/Documents/2022_PhD/Data/")

### preprocess
#general functions personalized
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
  r <- flip(r, direction="y")
  return(r)
}

#read nc
watergap.array <- nc_to_array("WSI/totalWaterGap_annuaTot.nc", "total_gross_demand")
t <- nc_t("WSI/totalWaterGap_annuaTot.nc")
lon <- nc_lon("WSI/totalWaterGap_annuaTot.nc")
lat <- nc_lat("WSI/totalWaterGap_annuaTot.nc")
watergap.2010.slice <- watergap.array[,,30]
watergap.2011.slice <- watergap.array[,,31]
watergap.2010 <- slice_to_raster(watergap.2010.slice)
watergap.2011 <- slice_to_raster(watergap.2011.slice)

#write nc to tif (multiple tifs)
writeRaster(watergap.2010, "Indicators/Test_watergap/watergap2010.tif", options=c("TFW=YES"), overwrite = T)
writeRaster(watergap.2011, "Indicators/Test_watergap/watergap2011.tif", options=c("TFW=YES"), overwrite = T)

###############################################################################
#input variables
raster1 <- "Indicators/Test_watergap/watergap2010.tif"
raster2 <- "Indicators/Test_watergap/watergap2011.tif"
### read shapefile region
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")
### read tifs
raster2010 <- raster(raster1)
raster2011 <- raster(raster2)

### compile tif with multiple bands
raster <- stack(raster2010, raster2011)

### clip region from tif
clip <- mask(crop(raster, extent(shp)), shp)

### mean per year
df <- data.frame(mean = cellStats(clip, "mean"))
write_csv(df,"Indicators/Test_watergap/mean_watergap_cali.csv")
