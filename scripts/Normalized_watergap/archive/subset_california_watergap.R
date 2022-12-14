### subset California ###
library(ncdf4)
library(raster)
library(rgdal)
library(tmap)
library(dplyr)
library(leaflet)

setwd("C:/Users/5738091/Documents/2022_PhD/Data/WSI")

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
subsetslice_to_raster <- function(slice, xmin, xmax, ymin, ymax){
  r <- raster(t(slice), xmn=xmin, xmx=xmax, ymn=ymin, ymx=ymax,
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r, direction="y")
  return(r)
}
find_coord <- function(system, coordinate){
  difference = system - coordinate
  altered_coordinate = which.min(abs(difference))
  return(altered_coordinate)
}
Q90 <- function(x) quantile(x, 0.1, na.rm=T)

#load data
watergap.array <- nc_to_array("totalWaterGap_annuaTot.nc", "total_gross_demand")
discharge.array <- nc_to_array("discharge_annuaAvg_output.nc", "discharge")
demand.array <- nc_to_array("totalGrossDemand_annuaTot_output.nc", "total_gross_demand")
nc_temp <- nc_open("discharge_monthAvg_output_90.nc")
monthly_discharge.array <- ncvar_get(nc_temp, "discharge", start=c(1,1,1), count=c(4320,2160,90))
t <- nc_t("totalWaterGap_annuaTot.nc")
lon <- nc_lon("totalWaterGap_annuaTot.nc")
lat <- nc_lat("totalWaterGap_annuaTot.nc")

#subset california
watergap.cali.array <- watergap.array[find_coord(lon, -124):find_coord(lon,-115),
                                      find_coord(lat,32):find_coord(lat,42),30:40]
discharge.cali.array <- discharge.array[find_coord(lon, -124):find_coord(lon,-115),
                                        find_coord(lat,32):find_coord(lat,42),30:40]
monthly_discharge.cali.array <- monthly_discharge.array[find_coord(lon, -124):find_coord(lon,-115),
                                                        find_coord(lat,32):find_coord(lat,42),]
demand.cali.array <- demand.array[find_coord(lon, -124):find_coord(lon,-115),
                                  find_coord(lat,32):find_coord(lat,42),30:40]

watergap.cali.slice <- apply(watergap.cali.array, c(1,2), mean)
watergap.cali <- subsetslice_to_raster(watergap.cali.slice, -124, -115, 32, 42)
watergap.cali[watergap.cali<0.02] <- NA
watergap.cali <- na.omit(watergap.cali)
discharge.cali.slice <- apply(discharge.cali.array, c(1,2), mean)
discharge.cali <- subsetslice_to_raster(discharge.cali.slice, -124, -115, 32, 42)
discharge.cali <- (discharge.cali/area(discharge.cali)/1000000)*3600*24*365.25
Q90.cali.slice <- apply(monthly_discharge.cali.array, c(1,2), Q90)
Q90.cali <- subsetslice_to_raster(Q90.cali.slice, -124, -115, 32, 42)
Q90.cali <- (Q90.cali/area(Q90.cali)/1000000)*3600*24*365.25
demand.cali.slice <- apply(demand.cali.array, c(1,2), mean)
demand.cali <- subsetslice_to_raster(demand.cali.slice, -124, -115, 32, 42)
demand.cali[demand.cali < 0.1] <- NA
demand.cali <- na.omit(demand.cali)

#EFR
renewdischarge.cali <- discharge.cali - Q90.cali
renewdischarge.cali[renewdischarge.cali <0] <- 0
gapdischarge.cali <- watergap.cali/renewdischarge.cali
gapdischarge.cali[gapdischarge.cali>1] <- 1
gapdemand.cali <- watergap.cali/demand.cali

writeRaster(gapdischarge.cali, "Temp/gapdischarge_cali_v6.tif", options=c("TFW=YES"), overwrite = T)
writeRaster(renewdischarge.cali, "Temp/renewdischarge_cali_v3.tif", options=c("TFW=YES"), overwrite = T)
writeRaster(watergap.cali, "Temp/watergap_cali_v3.tif", options=c("TFW=YES"), overwrite = T)
writeRaster(gapdemand.cali, "Temp/gapdemand_cali.tif", options=c("TFW=YES"), overwrite = T)
writeRaster(demand.cali, "Temp/demand_cali.tif", options=c("TFW=YES"))
