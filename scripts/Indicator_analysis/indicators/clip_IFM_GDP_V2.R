########## GDP IFM 1980-2016 ##########
# Author: Myrthe Leijnse

# Data: IFM (?) Murakami & Yamagata (2019), Hoch et al., 2022
# Spatialres: 5 arcmin/10km (also lower resolution available)
# Timeref: 1980-2016
# Definitions: Purchasing power parity (billion US$ 2005 reference year)

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("E:/1_hotspots_waterscarcity/Data")

### Functions ###
ncdf_to_df_mean <- function(ncinput, shp, variable, unit, shpcount){
  b <- brick(ncinput)
  clip <- mask(crop(b, extent(shp[[shpcount]])), shp[[shpcount]])
  df <- data.frame(value = cellStats(clip, "mean"), 
                   year = as.numeric(seq(1980,2015,1)),
                   variable = variable,
                   unit = unit,
                   hotspot = names(shp[shpcount]),
                   value.ref = cellStats(clip, "mean")) #initialize
  for (i in 1:nrow(df)){
    df$value.ref[i] <- df$value.ref[i]/df$value[1]
  }
  rownames(df) <- NULL
  return (df)
}

### Execution ###
df_GDP <- ncdf_to_df_mean("Indicators/GDP/GDP_Murakami&Yamagata/gdp_historic_5min.nc", shp_list, "GDP", "PPP $US in 2005 year rate", 1)
for (i in 2:length(shp_list)){
  df <- ncdf_to_df_mean("Indicators/GDP/GDP_Murakami&Yamagata/gdp_historic_5min.nc", shp_list, "GDP", "PPP $US in 2005 year rate", i)
  df_GDP <- rbind(df_GDP, df)
  print(i)
}

### Write CSV ###
write.csv(df_GDP, "Indicators/Indicator_tables/GDP_1980_2015.csv", row.names=F)

### Plotting ###

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
  return(r)
}
GDP2010_2016 <- function(shp, GDP.array, t, lon, lat){
  slice2010 <- slice_to_raster(GDP.array[,,30])
  slice2011 <- slice_to_raster(GDP.array[,,31])
  slice2012 <- slice_to_raster(GDP.array[,,32])
  slice2013 <- slice_to_raster(GDP.array[,,33])
  slice2014 <- slice_to_raster(GDP.array[,,34])
  slice2015 <- slice_to_raster(GDP.array[,,35])
  slice2016 <- slice_to_raster(GDP.array[,,36])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016)
  names(raster) <- c(sprintf("GDP%d",seq(2010, 2016, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #mean per capita per year
  df <- data.frame(GDP = cellStats(clip, "mean"), year = c(seq(2010, 2016, 1)))
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  return(df) 
}

#read data
GDP.array <- nc_to_array("Indicators/GDP/GDP_Murakami&Yamagata/gdp_historic.nc", "gdp")
GDP2.array <- nc_to_array("Indicators/GDP/GDP_Murakami&Yamagata/gdp_historic_5min.nc", "gdp")
t <- nc_t("Indicators/GDP/GDP_Murakami&Yamagata/gdp_historic.nc")
lon <- nc_lon("Indicators/GDP/GDP_Murakami&Yamagata/gdp_historic.nc")
lat <- nc_lat("Indicators/GDP/GDP_Murakami&Yamagata/gdp_historic.nc")
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

#mean annual GDP PPP in 2011 international US$
df_GDP <- GDP2010_2016(shp, GDP.array, t, lon, lat)
ggplot() + geom_line(data = df_GDP,aes(year, GDP, group=1), color= "red", lwd=2) + theme_bw()

#make timeseries for one point
LA_lon=-118.243683
LA_lat=34.052235
LA_lon=-73.935242
LA_lat=	40.730610
lon_diff = lon - LA_lon
LA_x = which.min(abs(lon_diff))
lat_diff = lat - LA_lat
LA_y = which.min(abs(lat_diff))
LA_GDP.series = GDP.array[LA_x, LA_y, ]

LA_df <- data.frame(Year= seq(from=1980, to=2015, by=1), GDP_PPP=LA_GDP.series)
ggplot(data=LA_df, aes(x=Year, y=GDP_PPP, group=1)) +
  geom_line() + 
  ggtitle("Yearly GDP (PPP) per capita US$ 2005 in LA") + 
  theme_bw() 

# GDP per capita should be around 70,000 in 2019
# Total GDP should be around 3.36 trillion in 2021
