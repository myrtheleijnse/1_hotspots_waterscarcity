########## DPSIR Data collection ##########
# Author: Myrthe Leijnse

### Libraries ###
library(sf)
library(viridis)
library(hrbrthemes)
library(gghighlight)
library(raster)
library(gridExtra)
library(maptools)

### Functions ###
relative_spaghetti_plot <- function(df, ylab_abs, title_abs, ylab_rel, title_rel, startyear, endyear) {
  grid.arrange(
    ggplot(df, aes(year, value, group=hotspot, colour = hotspot)) + 
      geom_line(lwd=1.5) + 
      gghighlight(max(value)>-2) +
      scale_color_viridis_d(option = "H") +
      scale_x_discrete(breaks=seq(startyear, endyear, 2)) +
      ylab(ylab_abs) +
      ggtitle(title_abs) +
      theme_ipsum(axis_title_size = 12),
    ggplot(df, aes(year, value.ref, group=hotspot, colour = hotspot)) + 
      geom_line(lwd=1.5) + 
      gghighlight(max(value)>-2) +
      scale_color_viridis_d(option = "H") +
      scale_x_discrete(breaks=seq(2001, 2010, 2)) +
      ylab(ylab_rel) +
      ggtitle(paste0(title_rel, startyear)) +
      theme_ipsum(axis_title_size = 12),
    nrow = 1
  )
}

### Reading Data ###
# shapefiles
setwd("E:/1_waterscarcity_hotspots/Data")
shp <- st_read("Water_provinces/Hotspot_Shapefiles/California.shp")
ff <- list.files("Water_provinces/Hotspot_Shapefiles/", pattern="\\.shp$", full.names=T)
shp_list <- lapply(ff, shapefile)
hotspots <- c("Irrawaddy", "California", "ChaoPhraya", "Chile", "China", "Euphrates", "Ganges", "Indus", "Japan", "Java", "Jordan",
              "Mekong", "Mexico", "MurrayDarling", "Nile", "Rhine", "Spain", "Sudan", "USHighPlains")
names(shp_list) = hotspots

# csv
df_pop <- read.csv("Indicators/Indicator_tables/worldpop_2000_2019.csv")
df_croplandarea_2 <- read.csv("Indicators/Indicator_tables/MODIS_croplandarea_2001_2010.csv")
df_urbanarea_2 <- read.csv("Indicators/Indicator_tables/MODIS_urbanarea_2001_2010.csv")

# source indicator Rfiles
setwd("C:/Users/5738091/Documents/2022_PhD/NatGeo_programming/1_hotspots_waterscarcity/scripts/Indicator_analysis/indicators")
source("clip_CRUpet.R")
source("clip_CRUprcp.R")
source("clip_GDHY_LandUse_V2.R")
source("PCR_sectoralDemand.R")
source("clip_SPEI.R")
source("clip_watergap.R")
source("clip_landusechange.R") # + values with regards to refyear
# source("clip_WaterQual.R")
source("clip_worldpop_v2.R")
# source("clip_snotel_SWE.R") #California only
# source("clip_GLOBGM_wtd.R")
# source("clip_IFM_GDP.R")

# sapply(list.files(), source)

### Execution ###

### Plotting ###

#plot timeseries per variable for all hotspots
ggplot(df_PET, aes(year,value, group=hotspot, colour = hotspot)) + 
  geom_line(lwd=1.5) + 
  gghighlight(max(value)>0, use_direct_label = FALSE) +
  scale_color_viridis_d() +
  scale_x_discrete(breaks=seq(2010, 2019, 2)) +
  ylab("PET (mm)") +
  ggtitle("Annual mean PET per hotspot") +
  facet_wrap(~ hotspot) +
  theme_ipsum(axis_title_size = 12)

ggplot(df_prcp, aes(year,value, group=hotspot, colour = hotspot)) + 
  geom_line(lwd=1.5) + 
  gghighlight(max(value)>0, use_direct_label = FALSE) +
  scale_color_viridis_d() +
  scale_x_discrete(breaks=seq(2010, 2019, 2)) +
  ylab("precipitation (mm)") +
  ggtitle("Annual mean precipitation per hotspot") +
  facet_wrap(~hotspot) +
  theme_ipsum(axis_title_size = 12)

ggplot(df_domesticWaterUse, aes(year,value, group=hotspot, colour = hotspot)) + 
  geom_line(lwd=1.5) + 
  gghighlight(max(value)>0, use_direct_label = FALSE) +
  scale_color_viridis_d() +
  scale_x_discrete(breaks=seq(2010, 2019, 2)) +
  ylab("gross water use (m/day)") +
  ggtitle("Annual total domestic water use per hotspot") +
  facet_wrap(~hotspot) +
  theme_ipsum(axis_title_size = 12)

ggplot(df_industrialWaterUse, aes(year,value, group=hotspot, colour = hotspot)) + 
  geom_line(lwd=1.5) + 
  gghighlight(max(value)>0, use_direct_label = FALSE) +
  scale_color_viridis_d() +
  scale_x_discrete(breaks=seq(2010, 2019, 2)) +
  ylab("gross water use (m/day)") +
  ggtitle("Annual total industrial water use per hotspot") +
  facet_wrap(~hotspot) +
  theme_ipsum(axis_title_size = 12)

ggplot(df_livestockWaterUse, aes(year,value, group=hotspot, colour = hotspot)) + 
  geom_line(lwd=1.5) + 
  gghighlight(max(value)>0, use_direct_label = F) +
  scale_color_viridis_d() +
  scale_x_discrete(breaks=seq(2010, 2014, 2)) +
  ylab("gross water use (m)") +
  ggtitle("Annual total livestock water use per hotspot") +
  facet_wrap(~hotspot) +
  theme_ipsum(axis_title_size = 12)

ggplot(df_watergap, aes(year,value, group=hotspot, colour = hotspot)) + 
  geom_line(lwd=1.5) + 
  gghighlight(max(value)>0, use_direct_label = F) +
  scale_color_viridis_d() +
  scale_x_discrete(breaks=seq(2010, 2019, 2)) +
  ylab("gross water use (m)") +
  ggtitle("Annual mean water gap per hotspot") +
  facet_wrap(~hotspot) +
  theme_ipsum(axis_title_size = 12)

ggplot(df_SPEI, aes(year,value, group=hotspot, colour = hotspot)) + 
  geom_line(lwd=1.5) + 
  gghighlight(max(value)>-2, use_direct_label = F) +
  scale_color_viridis_d() +
  scale_x_discrete(breaks=seq(2010, 2019, 2)) +
  ylab("SPEI") +
  ggtitle("Annual mean SPEI per hotspot") +
  facet_wrap(~hotspot) +
  theme_ipsum(axis_title_size = 12)

ggplot(df_cwr[df_cwr$Variable == "Crop water requirement maize",], aes(year,value, group=hotspot, colour = hotspot)) + 
  geom_line(lwd=1.5) + 
  gghighlight(max(value)>-2, use_direct_label = F) +
  scale_color_viridis_d() +
  scale_x_discrete(breaks=seq(2010, 2019, 2)) +
  ylab("Crop water requirement (m)") +
  ggtitle("Annual total maize water requirement per hotspot") +
  facet_wrap(~hotspot) +
  theme_ipsum(axis_title_size = 12)

relative_spaghetti_plot(df_croplandarea, 
                        "Cropland area (km2)", 
                        "Annual total cropland area per hotspot", 
                        "Relative cropland area",  
                        "Annual cropland area per hotspot relative to ", 
                        2001, 2010)
relative_spaghetti_plot(df_urbanarea,
                        "Urban area (km2)",
                        "Annual total urban area per hotspot",
                        "Relative urban area",  
                        "Annual urban area per hotspot relative to ", 
                        2001, 2010)

relative_spaghetti_plot(df_pop,
                        "Population count",
                        "Annual total population count per hotspot",
                        "Relative population count",  
                        "Annual population count per hotspot relative to ", 
                        2000, 2019)
