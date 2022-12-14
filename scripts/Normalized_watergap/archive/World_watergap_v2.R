library(ncdf4)
#library(ggplot2)
library(raster)
library(rgdal)
#library(plotly)
#library(rasterly)
#library(viridis)
library(tmap)
library(dplyr)
library(leaflet)

load("~/2022_PhD/R_files/World_watergap.RData")
setwd("C:/Users/5738091/Documents/2022_PhD/Data")

#read and inspect nc data
nc_data <- nc_open("totalWaterGap_annuaTot.nc")

{
  sink('totalWaterGap.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(t)

total_demand.array <- ncvar_get(nc_data, "total_gross_demand") # store the data in a 3-dimensional array
dim(total_demand.array)
fillvalue <- ncatt_get(nc_data, "total_gross_demand", "_FillValue")
fillvalue
nc_close(nc_data)

#### plot 2019 timestep ####
total_demand.slice <- total_demand.array[,,40]

r <- raster(t(total_demand.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction="y")
plot(r)

#### change classification ####
reclass_df <- c(-Inf, 0.02, NA,
                0.02, 0.1, 1,
                0.1, 0.3, 2,
                0.3, Inf, 3)
reclass_m <- matrix(reclass_df, ncol=3, byrow=T)
r_classified <- reclassify(r, reclass_m)
r_classified <- na.omit(r_classified)

# plot reclassification
labels <- c("0.02-0.1", "0.1-0.3", ">0.3")
leaflet() %>% addTiles() %>% 
  addRasterImage(r_classified, colors = c("yellow", "orange", "red"), opacity = 0.8) %>%
  addLegend(colors = c("yellow", "orange", "red"), title = "Watergap 2019", labels=labels)
