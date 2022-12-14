############## normalized watergap ################
library(ncdf4)
library(raster)
library(rgdal)
library(tmap)
library(dplyr)
library(leaflet)

setwd("C:/Users/5738091/Documents/2022_PhD/Data/WSI")

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

watergap.array <- nc_to_array("totalWaterGap_annuaTot.nc", "total_gross_demand")
gwrecharge.array <- nc_to_array("gwRecharge_annuaTot_output.nc", "groundwater_recharge")
demand.array <- nc_to_array("totalGrossDemand_annuaTot_output.nc", "total_gross_demand")
t <- nc_t("totalWaterGap_annuaTot.nc")
lon <- nc_lon("totalWaterGap_annuaTot.nc")
lat <- nc_lat("totalWaterGap_annuaTot.nc")

array_to_slice <- function(array, timestep){
  slice <- array[,,timestep]
  r <- raster(t(slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r, direction="y")
  return(r)
}
watergap.slice <- array_to_slice(watergap.array,40)
watergap.slice[watergap.slice<0] = 0
gwrecharge.slice <- array_to_slice(gwrecharge.array,40)
gwrecharge.slice[gwrecharge.slice<0] = 0
demand.slice <- array_to_slice(demand.array,40)

#normalize watergap/groundwater recharge
gaprecharge.slice <- watergap.slice/gwrecharge.slice
gaprecharge.slice[gaprecharge.slice < 1] <- NA
gaprecharge.slice[gaprecharge.slice > 100] <- 100
gaprecharge.slice <- na.omit(gaprecharge.slice)
pal <- colorNumeric(c("yellow", "orange", "red"), values(gaprecharge.slice), 
                    na.color = "transparent")
leaflet() %>% addTiles() %>%
  addRasterImage(gaprecharge.slice, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title="ratio watergap/recharge", values=values(gaprecharge.slice))
#writeRaster(gaprecharge.slice, "TIF/gaprecharge.tif", options=c("TFW=YES"))

#normalize watergap/demand
gapdemand.slice <- watergap.slice/demand.slice
gapdemand.slice[gapdemand.slice < 0 | gapdemand.slice > 1] <- NA
gaprecharge.slice <- na.omit(gaprecharge.slice)

pal <- colorNumeric(c("red", "orange", "yellow"), values(gapdemand.slice), 
                    na.color = "transparent")
leaflet() %>% addTiles() %>%
  addRasterImage(gapdemand.slice, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title="ratio watergap/demand", values=values(gapdemand.slice))
#writeRaster(gapdemand.slice, "TIF/gapdemand.tif", options=c("TFW=YES"))
