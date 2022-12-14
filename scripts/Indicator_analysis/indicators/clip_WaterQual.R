### water quality
# Total dissolved solids (TDS)/salinity threshold: 2100 mg/L
# Biological oxygen demand (BOD) threshold: 8 mg/L
# Pathogenes/fecal coliform concentration (FC) threshold: cfu/100ml
# Water temperature (degrees Celcius)

library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)
require(gridExtra)

setwd("C:/Users/5738091/surfdrive3/Data")

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
  lon <- ncvar_get(nc_data, "longitude")
  nc_close(nc_data)
  return(lon)
}
nc_lat <- function(ncfile){
  nc_data <- nc_open(ncfile)
  lat <- ncvar_get(nc_data, "latitude", verbose = F)
  nc_close(nc_data)
  return(lat)
}
slice_to_raster <- function(slice){
  r <- raster(t(slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  return(r)
}
waterqual_2010_2019 <- function(shp, waterqual_2010_2019.array, t, lon, lat, variable) {
  slice2010 <- slice_to_raster(waterqual_2010_2019.array[,,1])
  slice2011 <- slice_to_raster(waterqual_2010_2019.array[,,2])
  slice2012 <- slice_to_raster(waterqual_2010_2019.array[,,3])
  slice2013 <- slice_to_raster(waterqual_2010_2019.array[,,4])
  slice2014 <- slice_to_raster(waterqual_2010_2019.array[,,5])
  slice2015 <- slice_to_raster(waterqual_2010_2019.array[,,6])
  slice2016 <- slice_to_raster(waterqual_2010_2019.array[,,7])
  slice2017 <- slice_to_raster(waterqual_2010_2019.array[,,8])
  slice2018 <- slice_to_raster(waterqual_2010_2019.array[,,9])
  slice2019 <- slice_to_raster(waterqual_2010_2019.array[,,10])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016, slice2017, slice2018, slice2019)
  names(raster) <- c(sprintf(paste0(variable," %d"),seq(2010, 2019, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #mean per year over area for waterquality in region
  df <- data.frame(value = cellStats(clip, "mean"), year = c(seq(2010, 2019, 1)))
  rownames(df) <- seq(1,nrow(df), 1)
  df$variable <- variable
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  return(df)
}

#read data
organic_2010_2019.array <- nc_to_array("Indicators/waterqual/organic_yearlyAvg_2010_2019.nc", "organic") 
watTemp_2010_2019.array <- nc_to_array("Indicators/waterqual/waterTemperature_yearlyAvg_2010_2019.nc", "waterTemperature")
pathogen_2010_2019.array <- nc_to_array("Indicators/waterqual/pathogen_yearlyAvg_2010_2019.nc", "pathogen")
salinity_2010_2019.array <- nc_to_array("Indicators/waterqual/salinity_yearlyAvg_2010_2019.nc", "salinity")
t <- nc_t("Indicators/waterqual/organic_yearlyAvg_2010_2019.nc")
lon <- nc_lon("Indicators/waterqual/organic_yearlyAvg_2010_2019.nc")
lat <- nc_lat("Indicators/waterqual/organic_yearlyAvg_2010_2019.nc")
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

#dataframes
df_organic <- waterqual_2010_2019(shp, organic_2010_2019.array, t, lon, lat, "Biological oxygen demand")
df_watTemp <- waterqual_2010_2019(shp, watTemp_2010_2019.array, t, lon, lat, "Water temperature")
df_pathogen <- waterqual_2010_2019(shp, pathogen_2010_2019.array, t, lon, lat, "Fecal coliform concentration")
df_salinity <- waterqual_2010_2019(shp, salinity_2010_2019.array, t, lon, lat, "Total dissolved solids")
df_waterqual <- rbind(df_organic, df_watTemp, df_pathogen, df_salinity)

#plot
grid.arrange(
 ggplot() + geom_line(data = df_organic, aes(year, value, group=1), color="yellow", lwd=2) +
  ggtitle("BOD in California 2010-2019") +
  labs(y="mg/L") +
  theme_bw(),
 ggplot() + geom_line(data = df_watTemp, aes(year, value, group=1), color= "green", lwd=2) +
   ggtitle("Water Temperature in California 2010-2019") +
   labs(y="°C") +
   theme_bw(),
 ggplot() + geom_line(data = df_pathogen, aes(year, value, group=1),color= "darkslateblue", lwd=2) +
   ggtitle("Fecal coliform concentration in California 2010-2019") +
   labs(y="cfu/100ml") +
   theme_bw(),
 ggplot() + geom_line(data = df_salinity, aes(year, value, group=1),color="black", lwd=2) +
   ggtitle("Total dissolved solids in California 2010-2019") +
   labs("mg/L") +
   theme_bw() 
)
#or count of pixels with exceeding thresholds/area exceeding threshold