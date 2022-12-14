########## Watergap PCRGLOB ##########
# Author: Myrthe Leijnse

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("C:/Users/5738091/Documents/2022_PhD/Data")

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
watergap_2010_2019 <- function(shp, array, t, lon, lat, variable) {
  slice2010 <- slice_to_raster(array[,,30])
  slice2011 <- slice_to_raster(array[,,31])
  slice2012 <- slice_to_raster(array[,,32])
  slice2013 <- slice_to_raster(array[,,34])
  slice2014 <- slice_to_raster(array[,,35])
  slice2015 <- slice_to_raster(array[,,36])
  slice2016 <- slice_to_raster(array[,,37])
  slice2017 <- slice_to_raster(array[,,38])
  slice2018 <- slice_to_raster(array[,,39])
  slice2019 <- slice_to_raster(array[,,40])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016, slice2017, slice2018, slice2019)
  names(raster) <- c(sprintf(paste0(variable," %d"),seq(2010, 2019, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #mean per year over area for watergap in region
  df <- data.frame(value = cellStats(clip, "mean"), year = c(seq(2010, 2019, 1)))
  #add columns
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  df$Variable <- variable
  df$unit <- "m/y"
  df$hotspot <- shp$hotspot[1]
  return(df)
}

### Reading Data ###
watergap.array <- nc_to_array("WSI/totalWaterGap_annuaTot.nc", "total_gross_demand")
t <- nc_t("WSI/totalWaterGap_annuaTot.nc")
lon <- nc_lon("WSI/totalWaterGap_annuaTot.nc")
lat <- nc_lat("WSI/totalWaterGap_annuaTot.nc")
#shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

### Execution ###
df_watergap <- watergap_2010_2019(shp_list[[1]], watergap.array, t, lon, lat, "Water gap")
for (i in 2:length(shp_list)){
  df <- watergap_2010_2019(shp_list[[i]], watergap.array, t, lon, lat, "Water gap")
  df_watergap <- rbind(df_watergap, df)
}
rownames(df_watergap) <- seq(1,nrow(df_watergap), 1)

### Plotting ###
ggplot() + geom_line(data = df_watergap, aes(year, value, group=1), color="steelblue", lwd=2) +
  ggtitle("Mean annual watergap in California 2010-2019") +
  labs(y="m") +
  theme_bw()
