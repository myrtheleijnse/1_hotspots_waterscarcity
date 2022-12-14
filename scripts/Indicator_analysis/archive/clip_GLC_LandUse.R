### Land use dataframe California
# downloaded from https://lcviewer.vito.be/download

library(raster)
library(sf)
library(lubridate)
library(ggplot2)

setwd("C:/Users/5738091/Documents/2022_PhD/Data/")

#read, clip and merge data 
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")
Read_LanduseTIF <- function(raster_1, raster_2, raster_3, shp){
  raster_1 <- mask(crop(raster_1, extent(shp)), shp) #clip
  raster_2 <- mask(crop(raster_2, extent(shp)), shp)
  raster_3 <- mask(crop(raster_3, extent(shp)), shp)
  raster <- do.call(merge,list(raster_1, raster_2, raster_3)) #merge
  raster[raster != 40] <- NA
  return(raster)
}
raster2015 <- Read_LanduseTIF(raster("Indicators/LandUse/W140N40_PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W120N40_PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W140N60_PROBAV_LC100_global_v3.0.1_2015-base_Discrete-Classification-map_EPSG-4326.TIF"), shp)
raster2016 <- Read_LanduseTIF(raster("Indicators/LandUse/W140N40_PROBAV_LC100_global_v3.0.1_2016-conso_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W120N40_PROBAV_LC100_global_v3.0.1_2016-conso_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W140N60_PROBAV_LC100_global_v3.0.1_2016-conso_Discrete-Classification-map_EPSG-4326.TIF"), shp)
raster2017 <- Read_LanduseTIF(raster("Indicators/LandUse/W140N40_PROBAV_LC100_global_v3.0.1_2017-conso_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W120N40_PROBAV_LC100_global_v3.0.1_2017-conso_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W140N60_PROBAV_LC100_global_v3.0.1_2017-conso_Discrete-Classification-map_EPSG-4326.TIF"), shp)
raster2018 <- Read_LanduseTIF(raster("Indicators/LandUse/W140N40_PROBAV_LC100_global_v3.0.1_2018-conso_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W120N40_PROBAV_LC100_global_v3.0.1_2018-conso_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W140N60_PROBAV_LC100_global_v3.0.1_2018-conso_Discrete-Classification-map_EPSG-4326.TIF"), shp)
raster2019 <- Read_LanduseTIF(raster("Indicators/LandUse/W140N40_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W120N40_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.TIF"),
                              raster("Indicators/LandUse/W140N60_PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.TIF"), shp)

#stack rasters
raster <- stack(raster2015, raster2016, raster2017, raster2018, raster2019)
names(raster) <- c(sprintf("Cropland %d",seq(2015, 2019, 1)))
plot(raster)
#calculate raster statistics in km2
df <- data.frame(Cropland_area = cellStats(raster/4000, "sum"), year = c(seq(2015, 2019, 1)))
rownames(df) <- seq(1,nrow(df), 1)
df$year <- format(strptime(df$year, "%Y"), format = "%Y")
#plot line data
ggplot() + geom_line(data = df,aes(year, Cropland_area, group=1), color= "red", lwd=2) + theme_bw()
