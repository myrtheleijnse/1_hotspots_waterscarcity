### snotel data SWE
#only stations from basins that subsequently flow through California
#SWE in mm

library(lubridate)
library(dplyr)
library(leaflet)

#snotel_explorer()

df_snow_cali <- snotel_download(site_id = c(1277,1258,1067,1051,1049,1050,1052,977,633,301,778,356,428,463,540,575,697,724,809,834,446,473,784,848,462,508,518,539,541,587,771,846,391), internal = TRUE)
#subset 2010-2019
df_snow_cali$year <-format(as.Date(df_snow_cali$date), "%Y")
df_snow_cali <- df_snow_cali[df_snow_cali$year >= 2010 & df_snow_cali$year <= 2019,]

#max SWE per year per station
df_maxSWE_cali <- df_snow_cali %>% group_by(site_name, year) %>% summarize(max_swe = max(snow_water_equivalent))
ggplot(df_maxSWE_cali) + geom_line(aes(year, max_swe, group=site_name, color = site_name)) + 
  labs(y= "Max SWE (mm)") +
  ggtitle("Maximum SWE per station in California")

#plot locations
locations <- df_snow_cali %>% group_by(site_name, longitude, latitude) %>% summarize(swe = mean(snow_water_equivalent))
leaflet() %>% addTiles() %>% addMarkers(lng=locations$longitude, lat= locations$latitude)
ggplot(locations) + geom_point(aes(x = longitude, y = latitude))

#mean SWE over all stations
df_mean_SWE_Cali <- df_maxSWE_cali %>% group_by(year) %>% summarize(max_swe = mean(max_swe))
ggplot(data=df_mean_SWE_Cali, aes(x=year, y=max_swe, group=1)) +
  geom_line(color = "red", lwd = 2) +
  labs(y="SWE (mm)") +
  ggtitle("Mean over max SWE per station near by California") + 
  theme_bw()
