########## Population count Worldpop 1km res, 2000-2019, per hotspot ##########
# Author: Myrthe Leijnse

# Data: Worldpop
# Spatialres: 1km
# Timeref: 2000-2019

### Libraries ###
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
library(ggplot2)

### Directory ###
setwd("E:/1_hotspots_waterscarcity/data")

### Functions ###
#TODO fix names shp
ncdf_to_df_sum <- function(ncinput, shp, variable, unit, shpcount){
  b <- brick(ncinput)
  clip <- mask(crop(b, extent(shp[[shpcount]])), shp[[shpcount]])
  df <- data.frame(value = cellStats(clip, "sum"), 
                   year = as.numeric(format(strptime(getZ(clip), "%Y-%m-%d"), format = "%Y")),
                   variable = variable,
                   unit = unit,
                   hotspot = names(shp[shpcount]),
                   value.ref = cellStats(clip, "sum")) #initialize
  return (df)
}

### Reading Data ###
filelist <- list.files(path = "Indicators/Population/worldpop_2000_2019/", pattern = "\\.nc$", full.names=T, recursive = F)

### Execution ###
list_df_pop <- lapply(filelist, function(x){ncdf_to_df_sum(x, shp_list, "Population", "count", 1)} )
for (i in 2:length(shp_list)){
  list_df_pop_shp <- lapply(filelist, function(x){ncdf_to_df_sum(x, shp_list, "Population", "count", i)} )
  list_df_pop <- append(list_df_pop, list_df_pop_shp)
  print(i)
}
df_pop <- do.call("rbind", list_df_pop)
for (j in seq(1,nrow(df_pop),20)){
  for (i in j:(j+19)){
  df_pop$value.ref[i] <- df_pop$value[i]/df_pop$value[j]
  }
}

### Write CSV ###
write.csv(df_pop, "Indicators/Indicator_tables/worldpop_2000_2019.csv", row.names=F)

### Plotting ###
ggplot() + geom_line(data = df_pop,aes(year, value, group=hotspot, color = hotspot), lwd=2) +
  ggtitle("Total population count in California 2000-2019") +
  theme_bw()