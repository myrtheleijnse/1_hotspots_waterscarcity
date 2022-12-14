### evaporation dataframe California
# mm/day to mm/month!
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
PET_2010_2019 <- function(shp, PET_2001_2010.array, PET_2011_2020.array, t, lon, lat) {
  #loops mm/day to mm/month
  days=c(31,28,31,30,31,30,31,31,30,31,30,31)
  leapdays=c(31,29,31,30,31,30,31,31,30,31,30,31)
  i=109
  j=1
  while (i <= 120){
    PET_2001_2010.array[,,i] = PET_2001_2010.array[,,i]*days[j]
    i=i+1
    j=j+1
  }
  i=1
  j=1
  while (i <= 108){
    PET_2011_2020.array[,,i] = PET_2011_2020.array[,,i]*days[j]
    i=i+1
    j=j+1
    if(j==13){j=1}
    if(i>12 & i<25 | i>60 & i<73){days=leapdays}
  }
  
  #monthly average to yearly sum slices
  slice2010 <- slice_to_raster(apply(PET_2001_2010.array[,,109:120], c(1,2), sum))
  slice2011 <- slice_to_raster(apply(PET_2011_2020.array[,,1:12], c(1,2), sum))
  slice2012 <- slice_to_raster(apply(PET_2011_2020.array[,,13:24], c(1,2), sum))
  slice2013 <- slice_to_raster(apply(PET_2011_2020.array[,,25:36], c(1,2), sum))
  slice2014 <- slice_to_raster(apply(PET_2011_2020.array[,,37:48], c(1,2), sum))
  slice2015 <- slice_to_raster(apply(PET_2011_2020.array[,,49:60], c(1,2), sum))
  slice2016 <- slice_to_raster(apply(PET_2011_2020.array[,,61:72], c(1,2), sum))
  slice2017 <- slice_to_raster(apply(PET_2011_2020.array[,,73:84], c(1,2), sum))
  slice2018 <- slice_to_raster(apply(PET_2011_2020.array[,,85:96], c(1,2), sum))
  slice2019 <- slice_to_raster(apply(PET_2011_2020.array[,,97:108], c(1,2), sum))
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016, slice2017, slice2018, slice2019)
  names(raster) <- c(sprintf("PET%d",seq(2010, 2019, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  #plot(clip)
  #mean per year
  df <- data.frame(PET = cellStats(clip, "mean"), year = c(seq(2010, 2019, 1)))
  #add columns
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  df$Variable <- "PET"
  df$unit <- "mm/y"
  df$hotspot <- shp$hotspot[1]
  colnames(df)[which(names(df)=="PET")] <- "value"
  return(df)
}

#read data
PET_2001_2010.array <- nc_to_array("Indicators/Evaporation/cru_ts4.06.2001.2010.pet.dat.nc", "pet")
PET_2011_2020.array <- nc_to_array("Indicators/Evaporation/cru_ts4.06.2011.2020.pet.dat.nc", "pet")
t <- nc_t("Indicators/Evaporation/cru_ts4.06.2001.2010.pet.dat.nc")
lon <- nc_lon("Indicators/Evaporation/cru_ts4.06.2001.2010.pet.dat.nc")
lat <- nc_lat("Indicators/Evaporation/cru_ts4.06.2001.2010.pet.dat.nc")
#shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

#obtain dataframe
#df_PET <- PET_2010_2019(shp, PET_2001_2010.array, PET_2011_2020.array, t, lon, lat)

#lineplot example for 1 hotspot
#ggplot() + geom_line(data = df_PET,aes(year, value, group=1), color= "red", lwd=2) + theme_bw()

#Obtain dataframe for all hotspot regions
df_PET <- PET_2010_2019(shp_list[[1]], PET_2001_2010.array, PET_2011_2020.array, t, lon, lat)
for (i in 2:length(shp_list)){
  df <- PET_2010_2019(shp_list[[i]], PET_2001_2010.array, PET_2011_2020.array, t, lon, lat)
  df_PET <- rbind(df_PET, df)
}
rownames(df_PET) <- seq(1,nrow(df_PET), 1)