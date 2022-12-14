############## WSI analysis ################
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

desalination.array <- nc_to_array("desalinationAbstraction_annuaTot_output.nc", "desalination_source_abstraction")
gwrecharge.array <- nc_to_array("gwRecharge_annuaTot_output.nc", "groundwater_recharge")
runoff.array <- nc_to_array("totalRunoff_annuaTot_output.nc", "total_runoff")
demand.array <- nc_to_array("totalGrossDemand_annuaTot_output.nc", "total_gross_demand")
t <- nc_t("desalinationAbstraction_annuaTot_output.nc")
lon <- nc_lon("desalinationAbstraction_annuaTot_output.nc")
lat <- nc_lat("desalinationAbstraction_annuaTot_output.nc")

### WSI 2019 ###
array_to_slice <- function(array, timestep){
  slice <- array[,,timestep]
  r <- raster(t(slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r, direction="y")
  return(r)
}

desalination.slice <- array_to_slice(desalination.array,40)
gwrecharge.slice <- array_to_slice(gwrecharge.array,40)
gwrecharge.slice[gwrecharge.slice<0] = 0
runoff.slice <- array_to_slice(runoff.array,40)
runoff.slice[runoff.slice<0] = 0
demand.slice <- array_to_slice(demand.array,40)

#plot all WSI components
par(mfrow=c(2,2))
my_window <- extent( -180, 180, -90, 90)
plot(my_window, col=NA, main = "Desalination 2019 (m)")
plot(desalination.slice, add=T)
plot(my_window, col=NA, main = "Groundwater recharge 2019 (m)")
plot(gwrecharge.slice, add=T)
plot(my_window, col=NA, main = "Runoff 2019 (m)")
plot(runoff.slice, add=T)
plot(my_window, col=NA, main = "Demand 2019 (m)")
plot(demand.slice, add=T)

desalination.slice1 = desalination.slice
desalination.slice1[desalination.slice1 == 0] = NA
desalination.slice1[desalination.slice1 > 0.01] = 0.01
pal=colorNumeric("GnBu", values(desalination.slice1),
                 na.color = "transparent")
leaflet() %>% addTiles () %>%
  addRasterImage(desalination.slice1, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title="Desalination 2019", values=values(desalination.slice1))

gwrecharge.slice1 = gwrecharge.slice
gwrecharge.slice1[gwrecharge.slice1 == 0] = NA
gwrecharge.slice1[gwrecharge.slice1 > 3] = 3
pal=colorNumeric("Spectral", values(gwrecharge.slice1),
                 na.color = "transparent")
leaflet() %>% addTiles () %>%
  addRasterImage(gwrecharge.slice1, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title="Groundwater recharge 2019", values=values(gwrecharge.slice1))

runoff.slice1 = runoff.slice
runoff.slice1[runoff.slice1 == 0] = NA
runoff.slice1[runoff.slice1 > 1] = 1
pal=colorNumeric("Spectral", values(runoff.slice1),
                 na.color = "transparent")
leaflet() %>% addTiles () %>%
  addRasterImage(runoff.slice1, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title="Runoff 2019", values=values(runoff.slice1))

demand.slice1 = demand.slice
demand.slice1[demand.slice1 == 0] = NA
demand.slice1[demand.slice1 >= 1] = 1
pal=colorNumeric("Reds", values(demand.slice1),
                 na.color = "transparent")
leaflet() %>% addTiles () %>%
  addRasterImage(demand.slice1, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title="Demand 2019", values=values(demand.slice1))

#plot WSI quick
wsi2019 <- demand.slice/(desalination.slice + gwrecharge.slice + runoff.slice)
par(mfrow=c(1,1))
plot(my_window, col=NA, main = "WSI 2019 (m)")
plot(wsi2019, add=T)

wsi2019[wsi2019>1] = 1
wsi2019[wsi2019<0.4] = NA
wsi2019 <- na.omit(wsi2019)

#plot WSI leaflet
pal=colorNumeric(c("yellow", "orange", "red"), values(wsi2019),
                 na.color = "transparent")
leaflet() %>% addTiles () %>% setView(lng=0, lat=0, zoom=2) %>%
  addRasterImage(wsi2019, colors=pal, opacity = 0.8) %>%
  addLegend(pal=pal, title="WSI > 0.4 in 2019", values=values(wsi2019))

#writeRaster(wsi2019, "wsi2019.tif", options=c("TFW=YES"))
