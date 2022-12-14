############## normalized watergap over demand ################
library(ncdf4)
library(raster)
library(rgdal)
library(tmap)
library(dplyr)
library(leaflet)

setwd("C:/Users/5738091/Documents/2022_PhD/Data/WSI")
load("~/2022_PhD/R_files/Normalized_watergap_v2.RData")

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
  r <- flip(r, direction="y")
  return(r)
}

#load nc data
watergap.array <- nc_to_array("totalWaterGap_annuaTot.nc", "total_gross_demand")
demand.array <- nc_to_array("totalGrossDemand_annuaTot_output.nc", "total_gross_demand")
t <- nc_t("totalWaterGap_annuaTot.nc")
lon <- nc_lon("totalWaterGap_annuaTot.nc")
lat <- nc_lat("totalWaterGap_annuaTot.nc")

# mean watergap 2010-2019
watergap.recent.array <- watergap.array[,,30:40]
watergap.mean.slice <- apply(watergap.recent.array, c(1,2), mean)
watergap.mean <- slice_to_raster(watergap.mean.slice)
watergap.mean[watergap.mean < 0.02] <- NA
watergap.mean <- na.omit(watergap.mean)
writeRaster(watergap.mean, "TIF/watergap_mean.tif", options=c("TFW=YES"))

# mean demand 2010-2019 > 0.1 m
demand.recent.array <- demand.array[,,30:40]
demand.mean.slice <- apply(demand.recent.array, c(1,2), mean)
demand.mean <- slice_to_raster(demand.mean.slice)
demand.mean[demand.mean < 0.1] <- NA
demand.mean <- na.omit(demand.mean)
# plot(demand.mean)
writeRaster(demand.mean, "TIF/demand_mean.tif", options=c("TFW=YES"))

# mean watergap over demand
gapdemand <- watergap.mean/demand.mean
pal <- colorNumeric(c("red", "orange", "yellow"), values(gapdemand),
                    na.color = "transparent")
leaflet() %>% addTiles() %>%
  addRasterImage(gapdemand, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title=paste("Watergap/demand", "2010-2019", sep="<br>"), values=values(gapdemand))

writeRaster(gapdemand, "TIF/gapdemand_mean.tif", options=c("TFW=YES"))

#demand pt2
demand.mean2 <- slice_to_raster(demand.mean.slice)
demand.mean2 <- na.omit(demand.mean2)
gapdemand2 <- watergap.mean/demand.mean2
writeRaster(gapdemand2, "TIF/gapdemand3_mean.tif", options=c("TFW=YES"))
