########## MODIS Land Use change ##########
# Author: Myrthe Leijnse

# Data: MCD12Q1.006 MODIS Land Cover Type Yearly Global 500m; LC_Type1 Classes
# Timeref: 2001-2010
# Classed map with 
# 12: Croplands: at least 60% of area is cultivated cropland - "Cropland area" 
# 13: Urban and Built-up Lands: at least 30% impervious surface area including building materials, asphalt and vehicles - "Urban area"
#TODO extend to 2019 (earlier not available)

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("E:/1_hotspots_waterscarcity/data")

### Functions ###
nc_to_df_area <- function(shp, shpcount, file, varname, variable, class){
  raster <- brick(file, varname = varname)
  clip <- mask(crop(raster, extent(shp[[shpcount]])), shp[[shpcount]])
  plot(clip)
  #calculating area of land use class
  area_list <- list()
  for (i in 1:nlayers(clip)) {
    area <- clip[[i]]
    area[area < class] <- NA 
    area[area > class] <- NA
    cellsize <- area(area, na.rm=T, weights=F)
    cellsize <- cellsize[!is.na(cellsize)]
    if(length(cellsize) == 0){
      area_list[[i]] <- 0
    } else {
      area_list[[i]] <- length(cellsize)*median(cellsize) # in km2        
    }
  }
  #make data frame area and add columns
  df <- do.call(rbind.data.frame, area_list)
  colnames(df) <- c("value")
  df$year = as.numeric(format(strptime(getZ(clip), "%Y-%m-%d"), format = "%Y"))
  df$variable <- variable
  df$unit <- "km2"
  df$hotspot <- names(shp[shpcount])
  df$value.ref <- df$value
  #compare to reference year (2001)
  for (i in 1:nrow(df)){
    df$value.ref[i] <- df$value[i]/df$value[1]
  }
  return(df)
}

### Execution ###
df_croplandarea <- nc_to_df_area(shp_list, 1, "Indicators/LandUse/MODIS_2001_2010/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg_fv0.02.nc4",
                                "landcover_igbp", "Cropland area", 12)
for(i in 2:length(shp_list)){
  df <- nc_to_df_area(shp_list, i, "Indicators/LandUse/MODIS_2001_2010/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg_fv0.02.nc4",
                                   "landcover_igbp", "Cropland area", 12)
  df_croplandarea <- rbind(df_croplandarea, df)
}
df_urbanarea <- nc_to_df_area(shp_list, 1, "Indicators/LandUse/MODIS_2001_2010/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg_fv0.02.nc4",
                                 "landcover_igbp", "Urban area", 13)
for(i in 2:length(shp_list)){
  df <- nc_to_df_area(shp_list, i, "Indicators/LandUse/MODIS_2001_2010/MODIS-C006_MCD12C1_landcover__LPDAAC__0.05deg_fv0.02.nc4",
                                "landcover_igbp", "Urban area", 13)
  df_urbanarea <- rbind(df_urbanarea, df)
}

### Write CSV ###
write.csv(df_croplandarea, "Indicators/Indicator_tables/MODIS_croplandarea_2001_2010.csv", row.names=F)
write.csv(df_urbanarea, "Indicators/Indicator_tables/MODIS_urbanarea_2001_2010.csv", row.names=F)

### Execution ###
df_croplandarea <- landuse_2001_2010(shp_list[[1]], landuse_2001_2010.array, t, lon, lat, "Cropland area", 12)
for (i in 2:length(shp_list)){
  df <- landuse_2001_2010(shp_list[[i]], landuse_2001_2010.array, t, lon, lat, "Cropland area", 12)
  df_croplandarea <- rbind(df_croplandarea, df)
}
df_urbanarea <- landuse_2001_2010(shp_list[[1]], landuse_2001_2010.array, t, lon, lat, "Urban area", 13)
for (i in 2:length(shp_list)){
  df <- landuse_2001_2010(shp_list[[i]], landuse_2001_2010.array, t, lon, lat, "Urban area", 13)
  df_urbanarea <- rbind(df_urbanarea, df)
}

### Plotting ###