############## normalized watergap over discharge ################
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
discharge.array <- nc_to_array("discharge_annuaAvg_output.nc", "discharge")
t <- nc_t("totalWaterGap_annuaTot.nc")
lon <- nc_lon("totalWaterGap_annuaTot.nc")
lat <- nc_lat("totalWaterGap_annuaTot.nc")
Q90.array <- nc_to_array("Q90.nc", "discharge")

# watergap median 2010-2019 RERUN
watergap.recent.array <- watergap.array[,,30:40]
watergap.mean.slice <- apply(watergap.recent.array, c(1,2), mean)
watergap.mean <- slice_to_raster(watergap.mean.slice)
watergap.mean[watergap.mean < 0.02] <- NA
watergap.mean <- na.omit(watergap.mean)
pal <- colorNumeric(c("yellow", "orange", "red"), values(watergap.median),
                    na.color = "transparent")
leaflet() %>% addTiles() %>%
  addRasterImage(watergap.median, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title=paste("Median watergap", "2010-2019", sep="<br>"), values=values(watergap.median))

# writeRaster(watergap.avg, "TIF/watergap_avg.tif", options=c("TFW=YES"))

# discharge median and mean 2010-2019 (in m)
discharge.recent.array <- discharge.array[,,30:40]
discharge.mean.slice <- apply(discharge.recent.array, c(1,2), mean)
discharge.mean <- slice_to_raster(discharge.mean.slice)
discharge.mean <- (discharge.mean/area(discharge.mean)/1000000)*3600*24*365.25

# normalized watergap/discharge
gapdischarge <- watergap.mean/discharge.mean
gapdischarge[gapdischarge > 1] <- 1
pal <- colorNumeric(c("red", "orange", "yellow"), values(gapdischarge),
                    na.color = "transparent")
leaflet() %>% addTiles() %>%
  addRasterImage(gapdischarge, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title=paste("Watergap/discharge", "2010-2019", sep="<br>"), values=values(gapdischarge))
#writeRaster(gapdischarge, "TIF/gapdischarge_mean.tif", options=c("TFW=YES"))

### correct for Environmental Flow Requirement
#calculate Q90 (in m) 2010-2019
#discharge.recent.array <- discharge.array[,,..:..]
Q90.raster <- slice_to_raster(Q90.array)
Q90.raster <- (Q90.raster/area(Q90.raster)/1000000)*3600*24*365.25

#normalized watergap/renewable surface water
renew.discharge <- discharge.mean-Q90.raster
renew.discharge[renew.discharge < 0] <- 0
gap.renewdischarge <- watergap.mean/renew.discharge
gap.renewdischarge[gap.renewdischarge > 1] <- 1
pal <- colorNumeric(c("yellow", "orange", "red"), values(gap.renewdischarge),
                    na.color = "transparent")
leaflet() %>% addTiles() %>%
  addRasterImage(gap.renewdischarge, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title=paste("Watergap/discharge", "2010-2019", sep="<br>"), values=values(gap.renewdischarge))

writeRaster(Q90.raster, "TIF/Q90_2010-2019.tif", options=c("TFW=YES"))
writeRaster(renew.discharge, "TIF/renewdischarge_v2.tif", options=c("TFW=YES"))
writeRaster(gap.renewdischarge, "TIF/gaprenewdischarge_v2.tif", options=c("TFW=YES"))

clip_gap.renewdischarge <- gap.renewdischarge
clip_gap.demand <- gapdemand
clip_gap.renewdischarge[clip_gap.renewdischarge < 0.5] = NA
clip_gap.demand[clip_gap.demand < 0.5] = NA
clip_gap.renewdischarge <- na.omit(clip_gap.renewdischarge)
clip_gap.demand <- na.omit(clip_gap.demand)
writeRaster(clip_gap.renewdischarge, "TIF/test/gaprenewdischargeAbove50.TIF", options=c("TFW=YES"))
writeRaster(clip_gap.demand, "TIF/test/demandAbove50.TIF", options=c("TFW=YES"))
