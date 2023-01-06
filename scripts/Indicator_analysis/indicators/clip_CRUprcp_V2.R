########## precipitation dataframe California ##########
# Author: Myrthe Leijnse

# Data: CRU TS 4.05 dataset
# Spatialres: 0.5deg/50km
# Timeref: 1901-2019, monthly
# Definition: 

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("E:/1_hotspots_waterscarcity/Data")

### Functions ###
ncdf_to_df_mean <- function(ncinput, shp, varname, variable, unit, shpcount){
  b <- brick(ncinput, varname = varname)
  clip <- mask(crop(b, extent(shp[[shpcount]])), shp[[shpcount]])
  yearly_clip <- calc(clip[[1:12]], sum)
  for (i in seq(13,120, 12)) {
    raster <- calc(clip[[i:(i+11)]], sum)
    yearly_clip <- stack(yearly_clip, raster)
  }
  df <- data.frame(value = cellStats(yearly_clip, "mean"), 
                   year = unique(as.numeric(format(strptime(getZ(clip), "%Y-%m-%d"), format = "%Y"))),
                   variable = variable,
                   unit = unit,
                   hotspot = names(shp[shpcount]),
                   value.ref = cellStats(yearly_clip, "mean")) #initialize
  return (df)
}

### Reading Data ###
filelist <- list.files(path = "Indicators/Precipitation/", pattern = "\\.nc$", full.names=T, recursive = F)

### Execution ###
list_df_prcp <- lapply(filelist, function(x){ncdf_to_df_mean(x, shp_list, "pre", "Precipitation", "mm/y", 1)})
for (i in 2:length(shp_list)){
  list_df_prcp_shp <- lapply(filelist, function(x){ncdf_to_df_mean(x, shp_list, "pre", "Precipitation", "mm/y", i)} )
  list_df_prcp <- append(list_df_prcp, list_df_prcp_shp)
  print(i)
}
df_prcp <- do.call("rbind", list_df_prcp)
df_prcp <- df_prcp[df_prcp$year >= 1960 & df_prcp$year <= 2019,]
rownames(df_prcp) <- seq(1,nrow(df_prcp),1)

for (j in seq(1,nrow(df_prcp),60)){
  for (i in j:(j+59)){
    df_prcp$value.ref[i] <- df_prcp$value[i]/df_prcp$value[j]
  }
}

### Write CSV ###
write.csv(df_prcp, "Indicators/Indicator_tables/CRU_prcp_1960_2019.csv", row.names=F)

###################################################################################################

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
prcp_2010_2019 <- function(shp, prcp_2001_2010.array, prcp_2011_2020.array, t, lon, lat) {
  #monthly average to yearly sum slices
  slice2010 <- slice_to_raster(apply(prcp_2001_2010.array[,,109:120], c(1,2), sum))
  slice2011 <- slice_to_raster(apply(prcp_2011_2020.array[,,1:12], c(1,2), sum))
  slice2012 <- slice_to_raster(apply(prcp_2011_2020.array[,,13:24], c(1,2), sum))
  slice2013 <- slice_to_raster(apply(prcp_2011_2020.array[,,25:36], c(1,2), sum))
  slice2014 <- slice_to_raster(apply(prcp_2011_2020.array[,,37:48], c(1,2), sum))
  slice2015 <- slice_to_raster(apply(prcp_2011_2020.array[,,49:60], c(1,2), sum))
  slice2016 <- slice_to_raster(apply(prcp_2011_2020.array[,,61:72], c(1,2), sum))
  slice2017 <- slice_to_raster(apply(prcp_2011_2020.array[,,73:84], c(1,2), sum))
  slice2018 <- slice_to_raster(apply(prcp_2011_2020.array[,,85:96], c(1,2), sum))
  slice2019 <- slice_to_raster(apply(prcp_2011_2020.array[,,97:108], c(1,2), sum))
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016, slice2017, slice2018, slice2019)
  names(raster) <- c(sprintf("prcp%d",seq(2010, 2019, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  #plot(clip)
  #mean per year
  df <- data.frame(prcp = cellStats(clip, "mean"), year = c(seq(2010, 2019, 1)))
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  df$Variable <- "prcp"
  df$unit <- "mm/y"
  df$hotspot <- shp$hotspot[1]
  colnames(df)[which(names(df)=="prcp")] <- "value"
  return(df)
  print(df$hotspot[1], " done")
}

### Reading Data ###
prcp_2001_2010.array <- nc_to_array("Indicators/Precipitation/cru_ts4.06.2001.2010.pre.dat.nc", "pre")
prcp_2011_2020.array <- nc_to_array("Indicators/Precipitation/cru_ts4.06.2011.2020.pre.dat.nc", "pre")
t <- nc_t("Indicators/Precipitation/cru_ts4.06.2001.2010.pre.dat.nc")
lon <- nc_lon("Indicators/Precipitation/cru_ts4.06.2001.2010.pre.dat.nc")
lat <- nc_lat("Indicators/Precipitation/cru_ts4.06.2001.2010.pre.dat.nc")
#shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")

### Execution ###
df_prcp <- prcp_2010_2019(shp_list[[1]], prcp_2001_2010.array, prcp_2011_2020.array, t, lon, lat)
for (i in 2:length(shp_list)){
  df <- prcp_2010_2019(shp_list[[i]], prcp_2001_2010.array, prcp_2011_2020.array, t, lon, lat)
  df_prcp <- rbind(df_prcp, df)
}
rownames(df_prcp) <- seq(1,nrow(df_prcp), 1)

### Plotting ###
#ggplot() + geom_line(data = df_prcp,aes(year, prcp, group=1), color= "red", lwd=2) + theme_bw()

