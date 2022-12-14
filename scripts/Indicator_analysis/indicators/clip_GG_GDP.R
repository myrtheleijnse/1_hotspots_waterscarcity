### GDP dataframe California
#local data source bea.gov for cali in Real GDP (millions of chained 2012 dollars)
df_GDP_local <- data.frame(year = c(seq(2016, 2019, 1)), GDP = c(24278946,	25417693,	26435763,	27393434)) #needs to be divided total per capita
df_GDP_local$GDP <- df_GDP_local$GDP*1000000 #Real GDP (needs to be corrected for inflation to become GDP PPP)

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
GDP2010_2015 <- function(shp, GDP.array, t, lon, lat){
  slice2010 <- slice_to_raster(GDP.array[,,21])
  slice2011 <- slice_to_raster(GDP.array[,,22])
  slice2012 <- slice_to_raster(GDP.array[,,23])
  slice2013 <- slice_to_raster(GDP.array[,,24])
  slice2014 <- slice_to_raster(GDP.array[,,25])
  slice2015 <- slice_to_raster(GDP.array[,,26])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015)
  names(raster) <- c(sprintf("GDP%d",seq(2010, 2015, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #mean per capita per year
  df <- data.frame(GDP = cellStats(clip, "mean"), year = c(seq(2010, 2015, 1)))
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  return(df) 
}

#read data
GDP.array <- nc_to_array("Indicators/GDP/GDP_PPP_1990_2015_5arcmin_v2.nc", "GDP_PPP")
t <- nc_t("Indicators/GDP/GDP_PPP_1990_2015_5arcmin_v2.nc")
lon <- nc_lon("Indicators/GDP/GDP_PPP_1990_2015_5arcmin_v2.nc")
lat <- nc_lat("Indicators/GDP/GDP_PPP_1990_2015_5arcmin_v2.nc")
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

#mean annual GDP PPP in 2011 international US$
df_GDP <- GDP2010_2015(shp, GDP.array, t, lon, lat)
ggplot() + geom_line(data = df_GDP,aes(year, GDP, group=1), color= "red", lwd=2) + theme_bw()

#make timeseries for one point
LA_lon=-118.243683
LA_lat=34.052235
lon_diff = lon - LA_lon
LA_x = which.min(abs(lon_diff))
lat_diff = lat - LA_lat
LA_y = which.min(abs(lat_diff))
LA_GDP.series = GDP.array[LA_x, LA_y, ]

LA_df <- data.frame(Year= seq(from=1990, to=2015, by=1), GDP_PPP=LA_GDP.series)
ggplot(data=LA_df, aes(x=Year, y=GDP_PPP, group=1)) +
  geom_line() + 
  ggtitle("Yearly GDP (PPP) per capita US$ 2011 in LA") + 
  theme_bw()

# GDP per capita should be around 70000 in 2019
# Total GDP should be around 3.36 trillion in 2021
