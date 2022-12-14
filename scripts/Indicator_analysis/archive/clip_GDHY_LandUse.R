### Land use crop type
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

setwd("C:/Users/5738091/surfdrive3/Data/")

#functions
nc_to_array <- function(ncfile, ncvariable){
  nc_data <- nc_open(ncfile)
  array <- ncvar_get(nc_data, ncvariable)
  nc_close(nc_data)
  return(array)
}
nc_t <- function(ncfile){
  nc_data <- nc_open(ncfile)
  t <- ncvar_get(nc_data, "sfc")
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
slice_to_raster_yield <- function(slice){
  r <- raster(t(slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  r <- flip(r)
  #change 0to360 to -180to180 longitudes
  x1 <- crop(r, extent(180, 360, -90, 90))
  x2 <- crop(r, extent(0, 180, -90, 90))   
  extent(x1) <- c(-180, 0, -90, 90)
  template<- projectRaster(from = x2, to= x1, alignOnly=TRUE)
  x2_aligned <- projectRaster(from=x2, to = template)
  m <- merge(x2_aligned, x1)
  return(m)
}
Yield_2010_2016 <- function(shp, array, t, lon, lat) {
  #monthly average to yearly sum slices
  slice2010 <- slice_to_raster_yield(array[,,1])
  slice2011 <- slice_to_raster_yield(array[,,2])
  slice2012 <- slice_to_raster_yield(array[,,3])
  slice2013 <- slice_to_raster_yield(array[,,4])
  slice2014 <- slice_to_raster_yield(array[,,5])
  slice2015 <- slice_to_raster_yield(array[,,6])
  slice2016 <- slice_to_raster_yield(array[,,7])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016)
  names(raster) <- c(sprintf("Yield%d",seq(2010, 2016, 1)))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #sum per year
  df <- data.frame(Yield = cellStats(clip, "mean"), year = c(seq(2010, 2016, 1)))
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  return(df)
}
slice_to_raster_cwr <- function(slice){
  r <- raster(t(slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  #change 0to360 to -180to180 longitudes
  x1 <- crop(r, extent(0, 360, -90, 90))
  extent(x1) <- c(-180, 180, -90, 90)
  return(x1)
}
cwr_1971_to_df <- function(irr_cwr_maize.array, irr_cwr_soybean.array, irr_cwr_rice.array, irr_cwr_wheat.array, shp){
  slice_maize <- slice_to_raster_cwr(irr_cwr_maize.array)
  slice_soybean <- slice_to_raster_cwr(irr_cwr_soybean.array)
  slice_rice <- slice_to_raster_cwr(irr_cwr_rice.array)
  slice_wheat <- slice_to_raster_cwr(irr_cwr_wheat.array)
  #stack but leave out soybean as it returns NAs
  raster <- stack(slice_maize, slice_rice, slice_soybean, slice_wheat)
  names(raster) <- c(sprintf("CropWaterRequirement %s",c("maize", "rice", "soybean", "wheat")))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #sum per year
  df <- data.frame(CropWaterRequirement = cellStats(clip, "sum"), crop = c("maize", "rice", "soybean", "wheat"))
  rownames(df) <- seq(1,nrow(df), 1)
  return(df)
}
cwr_1971_to_clip <- function(irr_cwr_maize.array, irr_cwr_soybean.array, irr_cwr_rice.array, irr_cwr_wheat.array, shp){
  slice_maize <- slice_to_raster_cwr(irr_cwr_maize.array)
  slice_soybean <- slice_to_raster_cwr(irr_cwr_soybean.array)
  slice_rice <- slice_to_raster_cwr(irr_cwr_rice.array)
  slice_wheat <- slice_to_raster_cwr(irr_cwr_wheat.array)
  #stack but leave out soybean as it returns NAs
  raster <- stack(slice_maize, slice_rice, slice_soybean, slice_wheat)
  names(raster) <- c(sprintf("CropWaterRequirement %s",c("maize", "rice", "soybean", "wheat")))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  return(clip)
}
yield_1971_to_df <- function(yield_maize_1971.array, yield_soybean_1971.array, yield_rice_1971.array, yield_wheat_1971.array, shp){
  slice_maize <- slice_to_raster_cwr(yield_maize_1971.array)
  slice_soybean <- slice_to_raster_cwr(yield_soybean_1971.array)
  slice_rice <- slice_to_raster_cwr(yield_rice_1971.array)
  slice_wheat <- slice_to_raster_cwr(yield_wheat_1971.array)
  #stack but leave out soybean as it returns NAs
  raster <- stack(slice_maize, slice_rice, slice_soybean, slice_wheat)
  names(raster) <- c(sprintf("Yield %s",c("maize", "rice", "soybean", "wheat")))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  #sum total yield 1971
  df <- data.frame(Yield = cellStats(clip, "mean"), crop = c("maize", "rice", "soybean", "wheat"))
  rownames(df) <- seq(1,nrow(df), 1)
  return(df)
}
yield_1971_to_clip <- function(yield_maize_1971.array, yield_soybean_1971.array, yield_rice_1971.array, yield_wheat_1971.array, shp){
  slice_maize <- slice_to_raster_cwr(yield_maize_1971.array)
  slice_soybean <- slice_to_raster_cwr(yield_soybean_1971.array)
  slice_rice <- slice_to_raster_cwr(yield_rice_1971.array)
  slice_wheat <- slice_to_raster_cwr(yield_wheat_1971.array)
  #stack but leave out soybean as it returns NAs
  raster <- stack(slice_maize, slice_rice, slice_soybean, slice_wheat)
  names(raster) <- c(sprintf("Yield %s",c("maize", "rice", "soybean", "wheat")))
  #clip
  clip <- mask(crop(raster, extent(shp)), shp)
  plot(clip)
  return(clip)
}
cwr_2010_2016 <- function(shp, array, t, lon, lat, irr_demand) {
  #monthly average to yearly sum slices
  slice2010 <- slice_to_raster_yield(array[,,1])
  slice2011 <- slice_to_raster_yield(array[,,2])
  slice2012 <- slice_to_raster_yield(array[,,3])
  slice2013 <- slice_to_raster_yield(array[,,4])
  slice2014 <- slice_to_raster_yield(array[,,5])
  slice2015 <- slice_to_raster_yield(array[,,6])
  slice2016 <- slice_to_raster_yield(array[,,7])
  #stack
  raster <- stack(slice2010, slice2011, slice2012, slice2013, slice2014, slice2015, slice2016)
  names(raster) <- c(sprintf("CWR%d",seq(2010, 2016, 1)))
  #clip and multiply with irrigation demand
  clip <- mask(crop(raster, extent(shp)), shp)
  clip <- resample(clip, irr_demand)
  for (i in 1:4){
    clip[[i]]*irr_demand
  }
  plot(clip)
  #sum per year
  df <- data.frame(CWR = cellStats(clip, "sum"), year = c(seq(2010, 2016, 1)))
  rownames(df) <- seq(1,nrow(df), 1)
  df$year <- format(strptime(df$year, "%Y"), format = "%Y")
  #multiply by area
  clip_area <- clip
  for (i in 1:7){
    cellsize <- area(clip_area[[i]], na.rm=T, weights=F)
    cellsize <- cellsize[!is.na(cellsize)]
    area <- length(cellsize)*median(cellsize) #in km2 https://caucasus-spiders.info/r-spatial/raster-basics-3/
    area <- area *100                         #in ha
    clip_area[[i]] <- clip_area[[i]] * area
  }
  
  return(df)
}

#read data
GDHY_maize_2010_2016.array <- nc_to_array("Indicators/LandUse/gdhy_v1.2_v1.3_20190128_majorcrops/maize_yield.nc4", "var") 
GDHY_soybean_2010_2016.array <- nc_to_array("Indicators/LandUse/gdhy_v1.2_v1.3_20190128_majorcrops/soybean_yield.nc4", "var")
GDHY_rice_2010_2016.array <- nc_to_array("Indicators/LandUse/gdhy_v1.2_v1.3_20190128_majorcrops/rice_yield.nc4", "var")
GDHY_wheat_2010_2016.array <- nc_to_array("Indicators/LandUse/gdhy_v1.2_v1.3_20190128_majorcrops/wheat_yield.nc4", "var")
t <- nc_t("Indicators/LandUse/gdhy_v1.2_v1.3_20190128_majorcrops/maize_yield.nc4")
lon <- nc_lon("Indicators/LandUse/gdhy_v1.2_v1.3_20190128_majorcrops/maize_yield.nc4")
lat <- nc_lat("Indicators/LandUse/gdhy_v1.2_v1.3_20190128_majorcrops/maize_yield.nc4")
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")
irr_cwr_maize.array <- nc_to_array("Indicators/LandUse/CropWaterRequirement/crop_water_requirement_irrigated_maize.nc", "crop_water_requirement")
irr_cwr_soybean.array <- nc_to_array("Indicators/LandUse/CropWaterRequirement/crop_water_requirement_irrigated_soybeans.nc", "crop_water_requirement")
irr_cwr_rice.array <- nc_to_array("Indicators/LandUse/CropWaterRequirement/crop_water_requirement_irrigated_rice.nc", "crop_water_requirement")
irr_cwr_wheat.array <- nc_to_array("Indicators/LandUse/CropWaterRequirement/crop_water_requirement_irrigated_wheat.nc", "crop_water_requirement")
yield_maize_1971.array <- nc_to_array("Indicators/LandUse/CropWaterRequirement/crop_yield.nc", "crop_yield_irrigated_maize")
yield_soybean_1971.array <- nc_to_array("Indicators/LandUse/CropWaterRequirement/crop_yield.nc", "crop_yield_irrigated_soybeans")
yield_rice_1971.array <- nc_to_array("Indicators/LandUse/CropWaterRequirement/crop_yield.nc", "crop_yield_irrigated_rice")
yield_wheat_1971.array <- nc_to_array("Indicators/LandUse/CropWaterRequirement/crop_yield.nc", "crop_yield_irrigated_wheat")

#crop yield t/ha 2010-2016
df_maize <- Yield_2010_2016(shp, GDHY_maize_2010_2016.array, t, lon, lat)
df_maize$croptype = "maize"
df_soybean <- Yield_2010_2016(shp, GDHY_soybean_2010_2016.array, t, lon, lat)
df_soybean$croptype = "soybean"
df_rice <- Yield_2010_2016(shp, GDHY_rice_2010_2016.array, t, lon, lat)
df_rice$croptype = "rice"
df_wheat <- Yield_2010_2016(shp, GDHY_wheat_2010_2016.array, t, lon, lat)
df_wheat$croptype = "wheat"
df_crops <- rbind(df_maize, df_soybean, df_rice, df_wheat)
ggplot(df_crops) + 
  geom_line(aes(year, Yield, group=croptype, color=croptype), lwd=2) +
  labs(y="Yield (t/ha)") +
  ggtitle("Annual yield per major croptype in California") +
  theme_bw()

#Crop water requirements irrigation m/y 1971 (reference year)
df_cwr_1971 <- cwr_1971_to_df(irr_cwr_maize.array, irr_cwr_soybean.array, irr_cwr_rice.array, irr_cwr_wheat.array, shp)
clip_cwr_1971 <- cwr_1971_to_clip(irr_cwr_maize.array, irr_cwr_soybean.array, irr_cwr_rice.array, irr_cwr_wheat.array, shp)

#Crop yield irrigation t/ha 1971 (reference year)
df_yield_1971 <- yield_1971_to_df(yield_maize_1971.array, yield_soybean_1971.array, yield_rice_1971.array, yield_wheat_1971.array, shp)
clip_yield_1971 <- yield_1971_to_clip(yield_maize_1971.array, yield_soybean_1971.array, yield_rice_1971.array, yield_wheat_1971.array, shp)

#Calculate water requirement for 1 t/ha yield (in meters) -> normalized crop water requirement
irr_demand_maize <- clip_cwr_1971[[1]]/clip_yield_1971[[1]]
irr_demand_rice <- clip_cwr_1971[[2]]/clip_yield_1971[[2]]
irr_demand_soybean <- clip_cwr_1971[[3]]/clip_yield_1971[[3]]
irr_demand_wheat <- clip_cwr_1971[[4]]/clip_yield_1971[[4]]

#Calculate irrigation water demand per year (2010-2016)
df_cwr_maize <- cwr_2010_2016(shp, GDHY_maize_2010_2016.array, t, lon, lat, irr_demand_maize)
df_cwr_maize$croptype = "maize"
df_cwr_soybean <- cwr_2010_2016(shp, GDHY_soybean_2010_2016.array, t, lon, lat, irr_demand_soybean)
df_cwr_soybean$croptype = "soybean"
df_cwr_rice <- cwr_2010_2016(shp, GDHY_rice_2010_2016.array, t, lon, lat, irr_demand_rice)
df_cwr_rice$croptype = "rice"
df_cwr_wheat <- cwr_2010_2016(shp, GDHY_wheat_2010_2016.array, t, lon, lat, irr_demand_wheat)
df_cwr_wheat$croptype = "wheat"
df_cwr <- rbind(df_cwr_maize, df_cwr_soybean, df_cwr_rice, df_cwr_wheat)

#plot total water use over entire region in California
ggplot(df_cwr) + 
  geom_line(aes(year, CWR, group=croptype, color=croptype), lwd=2) +
  labs(y="Crop water requirement (m)") +
  ggtitle("Annual mean crop water requirement per major croptype in California") +
  theme_bw()
