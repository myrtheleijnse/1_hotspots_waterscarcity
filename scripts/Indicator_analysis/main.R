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
#TODO discrete x axes (as posix or strptime format %y ...)
relative_spaghetti_plot <- function(df, ylab_abs, title_abs, ylab_rel, title_rel, startyear, endyear) {
  grid.arrange(
    ggplot(df, aes(year, value, group=hotspot, colour = hotspot)) + 
      geom_line(lwd=1.5) + 
      gghighlight(max(value)>-2) +
      scale_color_viridis_d(option = "H") +
      #scale_x_discrete(breaks=seq(startyear, endyear, 2)) +
      ylab(ylab_abs) +
      ggtitle(title_abs) +
      theme_ipsum(axis_title_size = 12),
    ggplot(df, aes(year, value.ref, group=hotspot, colour = hotspot)) + 
      geom_line(lwd=1.5) + 
      gghighlight(max(value)>-2) +
      scale_color_viridis_d(option = "H") +
      #scale_x_discrete(breaks=seq(2001, 2010, 2)) +
      ylab(ylab_rel) +
      ggtitle(paste0(title_rel, startyear)) +
      theme_ipsum(axis_title_size = 12),
    nrow = 1
  )
}
spaghetti_plot <- function(df, ylab_abs, title_abs, startyear, endyear) {
  ggplot(df, aes(year, value, group=hotspot, colour = hotspot)) + 
    geom_line(lwd=1.5) + 
    gghighlight(hotspot == "California") +
    scale_color_viridis_d(option = "H") +
    #scale_x_discrete(breaks=seq(startyear, endyear, 2)) +
    ylab(ylab_abs) +
    ggtitle(title_abs) +
    theme_ipsum(axis_title_size = 12)
}

### Reading Data ###
# shapefiles
setwd("E:/1_hotspots_waterscarcity/Data")
ff <- list.files("Water_provinces/Hotspot_Shapefiles/", pattern="\\.shp$", full.names=T)
shp_list <- lapply(ff, shapefile)
hotspots <- c("Irrawaddy", "California", "ChaoPhraya", "Chile", "China", "Euphrates", "Ganges", "Indus", "Japan", "Java", "Jordan",
              "Mekong", "Mexico", "MurrayDarling", "Nile", "Rhine", "Spain", "Sudan", "USHighPlains")
names(shp_list) = hotspots

# csv
df_pop <- read.csv("Indicators/Indicator_tables/worldpop_2000_2019.csv")
df_croplandarea <- read.csv("Indicators/Indicator_tables/MODIS_croplandarea_2001_2010.csv")
df_urbanarea <- read.csv("Indicators/Indicator_tables/MODIS_urbanarea_2001_2010.csv")
df_watergap <- read.csv("Indicators/Indicator_tables/watergap_1980_2019.csv")
df_SPEI <- read.csv("Indicators/Indicator_tables/CRU_SPEI_1960_2019.csv")
df_prcp <- read.csv("Indicators/Indicator_tables/CRU_prcp_1960_2019.csv")
df_organic <- read.csv("Indicators/Indicator_tables/DynQual_organic_1980_2019.csv")
df_watTemp <- read.csv("Indicators/Indicator_tables/DynQual_waterTemperature_1980_2019.csv")
df_pathogen <- read.csv("Indicators/Indicator_tables/DynQual_pathogen_1980_2019.csv")
df_salinity <- read.csv("Indicators/Indicator_tables/DynQual_salinity_1980_2019.csv")
df_domesticWaterUse <- read.csv("Indicators/Indicator_tables/domesticWaterUse_1960_2019.csv")
df_industryWaterUse <- read.csv("Indicators/Indicator_tables/industryWaterUse_1960_2019.csv")
df_livestockWaterUse <- read.csv("Indicators/Indicator_tables/livestock_1961_2014.csv")

# source indicator Rfiles
setwd("C:/Users/5738091/Documents/2022_PhD/NatGeo_programming/1_hotspots_waterscarcity/scripts/Indicator_analysis/indicators")
source("clip_CRUpet.R")
source("clip_CRUprcp.R")
source("clip_GDHY_LandUse_V2.R")
source("PCR_sectoralDemand.R")
source("clip_SPEI.R")
source("clip_watergap_V2.R") # + values with regards to refyear
source("clip_landusechange_V2.R") # + values with regards to refyear
# source("clip_WaterQual.R")
source("clip_worldpop_v2.R") # + values with regards to refyear
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
relative_spaghetti_plot(df_watergap,
                        "Watergap (m)",
                        "Annual mean watergap per hotspot",
                        "Relative watergap",  
                        "Annual mean watergap per hotspot relative to ", 
                        1980, 2019)
spaghetti_plot(df_SPEI,
               "SPEI",
               "Zonal mean SPEI per hotspot",
               1960, 2019)
relative_spaghetti_plot(df_prcp,
                        "Precipitation (mm/y)",
                        "Annual mean precipitation per hotspot",
                        "Relative precipitation",  
                        "Annual mean precipitation per hotspot relative to ", 
                        1960, 2019)
relative_spaghetti_plot(df_organic,
                        "Biological oxygen demand (mg/L)",
                        "Annual mean BOD per hotspot",
                        "Relative biological oxygen demand",  
                        "Annual mean BOD per hotspot relative to ", 
                        1980, 2019)
relative_spaghetti_plot(df_watTemp,
                        "Water temperature (°C)",
                        "Annual mean water temperature per hotspot",
                        "Relative water temperature",  
                        "Annual mean water temperature per hotspot relative to ", 
                        1980, 2019)
relative_spaghetti_plot(df_pathogen,
                        "Fecal coliform concentration (cfu/100ml)",
                        "Annual mean pathogen concentration per hotspot",
                        "Relative fecal coliform concentration",  
                        "Annual mean pathogen concentration per hotspot relative to ", 
                        1980, 2019)
relative_spaghetti_plot(df_salinity,
                        "Total dissolved solids (mg/L)",
                        "Annual mean TDS per hotspot",
                        "Relative total dissolved solids",  
                        "Annual mean TDS per hotspot relative to ", 
                        1980, 2019)
relative_spaghetti_plot(df_salinity,
                        "Total dissolved solids (mg/L)",
                        "Annual mean TDS per hotspot",
                        "Relative total dissolved solids",  
                        "Annual mean TDS per hotspot relative to ", 
                        1980, 2019)
relative_spaghetti_plot(df_domesticWaterUse,
                        "Gross domestic demand (m/d)",
                        "Annual mean domestic demand per hotspot",
                        "Relative domestic demand",  
                        "Annual mean domestic demand per hotspot relative to ", 
                        1960, 2019)
relative_spaghetti_plot(df_industryWaterUse,
                        "Gross industry demand (m/d)",
                        "Annual mean industry demand per hotspot",
                        "Relative industry demand",  
                        "Annual mean industry demand per hotspot relative to ", 
                        1960, 2019)
relative_spaghetti_plot(df_livestockWaterUse,
                        "Gross livestock demand (m)",
                        "Annual mean livestock demand per hotspot",
                        "Relative livestock demand",  
                        "Annual mean livestock demand per hotspot relative to ", 
                        1961, 2014)
