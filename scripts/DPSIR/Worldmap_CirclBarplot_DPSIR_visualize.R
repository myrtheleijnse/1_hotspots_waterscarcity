########## Worldmap + circular barplot  ##########
# Author: Myrthe Leijnse

### Libraries ###
library(readxl)
library(fmsb)
library(tidyverse)
library(xlsx)

### Directory ###
setwd("C:/Users/5738091/Documents/2022_PhD/NatGeo_programming/1_hotspots_waterscarcity/data")

### Functions ###
obtain_indicator <- function(sheet, indicator_list, indicator_name_string_singular, indicator_name_string_plural, hotspots_list) {
  df <- data.frame()
  for (region in hotspots_list){
    sheet_region <- sheet[sheet$Hotspot == region, ]
    col_region <- sheet_region[, indicator_list]
    col_region[is.na(col_region)] <- 0.0
    values <- unname(colSums(col_region[, indicator_list]))
    DPSIR <- indicator_name_string_singular
    df_region <- cbind(region, DPSIR, indicator_list, data.frame(values))
    names(df_region)[names(df_region) == "indicator_list"] <- "indicator"
    df <- rbind(df, df_region)
  }
  
  return(df)
}


### Reading Data ###
driver_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Driver", range = cell_rows(3:999))
pressure_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Pressure", range = cell_rows(3:999))
state_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "State", range = cell_rows(3:999))
impact_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Impact", range = cell_rows(3:999))
response_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Response", range = cell_rows(3:999))

drivers <- c(
  "Population", "Low natural water availability", "Economic growth",
  "Inefficient irrigation system", "Siltation", "Snowpack reduction", "Rainfall seasonality changes",
  "Aridification", "Drought frequency", "Temperature increase", "Sea level rise"
)
pressures <- c(
  "Tourism", "Agri water use", "Ecosystem water demand", "Total water use",
  "Domestic water demand", "Crop change/intensification", "Urbanization", "Mining",
  "Industrial water use", "Livestock water use", "Virtual water trade", "Aquaculture",
  "Forestry", "Horticulture"
)
states <- c(
  "Water use per capita", "GW depletion", "Contamination",
  "Overallocation of surface water", "Low water storage capacity", "Salinization"
)
impacts <- c(
  "Fallowing acreage", "Subsidence", "Reduced hydroelectricity production", "Damage to ecosystems",
  "Reduced food production", "Conflict", "Health", "Costs (pumping/energy)", "High GHG emissions",
  "Water collection", "Migration"
)
responses <- c(
  "RGW (+)", "URGW (-)", "WI (+)", "WI (-)", "IWUE (+)", "IWUE (-)", "WC (+)", "WC(-)", "WRe (+)",
  "WRe (-)", "DC (+)", "DC (-)", "WP (+)", "WP (-)", "DP (+)", "DP(-)", "Wri (+)", "Wri (-)",
  "SRP (+)", "SRP (-)", "GTW (+)", "UGTW (-)", "PT (+)", "PT (-)"
)
df_hotspots <- unique(read.xlsx("./raw/DPSIR/DPSIR_analysis.xlsx", "Driver", colIndex=2, rowIndex = 3:999))
df_hotspots$lon <-
df_hotspots$lat <-
hotspots_list <- df_hotspots$Hotspot

### Execution ###
df_driver <- obtain_indicator(driver_sheet, drivers, "driver", "drivers", hotspots_list)
df_pressure <- obtain_indicator(pressure_sheet, pressures, "pressure", "pressures", hotspots_list)
df_state <- obtain_indicator(state_sheet, states, "state", "states", hotspots_list)
df_impact <- obtain_indicator(impact_sheet, impacts, "impact", "impacts", hotspots_list)
df_response <- obtain_indicator(response_sheet, responses, "response", "responses", hotspots_list)
df_DPSIR <- rbind(df_driver, df_pressure, df_state, df_impact, df_response)
df$DPSIR <- factor(df$DPSIR, levels = c("driver", "pressure", "state", "impact", "response")) # set DPSIR sequence

### Plotting ###



### Testing Plotting ###
# https://stackoverflow.com/questions/36063043/how-to-plot-barchart-onto-ggplot2-map
library(rgdal)
library(ggplot2)
library(rgeos)
library(maptools)
library(grid)
library(gridExtra)

map.test <- shp_list

#get centroids
map.test.centroids <- data.frame()
for (i in 1:length(shp_list)){
  centroids <- gCentroid(map.test[[i]], byid=T)
  centroids <- as.data.frame(centroids)
  centroids$OBJECTID <- row.names(centroids)
  map.test.centroids <- rbind(map.test.centroids, centroids)
}

#create df for ggplot
kt_geom <- data.frame()
for (i in 1:length(shp_list)){
  kt_geom_hotspot <- fortify(map.test[[i]], region="OBJECTID")
  kt_geom <- rbind(kt_geom, kt_geom_hotspot)
}

#Plot map
ggplot(kt_geom)+
  geom_polygon(aes(long, lat, group=group), fill="white")+
  coord_fixed()+
  geom_path(color="gray48", mapping=aes(long, lat, group=group), size=0.2)+
  geom_point(data=map.test.centroids, aes(x=x, y=y), size=2, alpha=6/10)

world <- map_data("world")
ggplot() +
  geom_map(data = world, map = world, aes(long, lat, map_id = region),
           color = "black", fill = "lightgray", size = 0.1) +
  theme(panel.background = element_blank()) +
  geom_point(data=map.test.centroids, aes(x=x, y=y), size=2, alpha=6/10, colour = "red")