### Circular Barplot DPSIR analysis per hotspot ###
library(readxl)
library(fmsb)
library(tidyverse)
library(viridis)

setwd("./data")

###reading xlsx from 3rd row onwards
driver_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Driver", range = cell_rows(3:999))
pressure_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Pressure", range = cell_rows(3:999))
state_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "State", range = cell_rows(3:999))
impact_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Impact", range = cell_rows(3:999))
response_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Response", range = cell_rows(3:999))

###list indicators
drivers <- c("Population", "Low natural water availability",	"Economic growth",	
             "Inefficient irrigation system", "Siltation", "Snowpack reduction", "Rainfall seasonality changes",	
             "Aridification", "Drought frequency", "Temperature increase", "Sea level rise")
pressures <- c("Tourism",	"Agri water use",	"Ecosystem water demand",	"Total water use",	
               "Domestic water demand",	"Crop change/intensification",	"Urbanization", "Mining", 
               "Industrial water use", "Livestock water use", "Virtual water trade",  "Aquaculture")
states <- c("Water use per capita",	"GW depletion",	"Contamination",	
            "Overallocation of surface water", "Low water storage capacity", "Salinization")
impacts <- c("Fallowing acreage", "Subsidence",	"Reduced hydroelectricity production",	"Damage to ecosystems",	
             "Reduced food production", "Conflict",	"Health",	"Costs (pumping/energy)", "High GHG emissions", 
             "Water collection", "Migration")
responses <- c("RGW (+)",	"URGW (-)",	"WI (+)",	"WI (-)",	"IWUE (+)",	"IWUE (-)",	"WC (+)",	"WC(-)", 	"WRe (+)",
               "WRe (-)",	"DC (+)",	"DC (-)",	"WP (+)", 	"WP (-)",	"DP (+)",	"DP(-)",	"Wri (+)",	"Wri (-)",
               "SRP (+)",	"SRP (-)", "GTW (+)", "UGTW (-)",	"PT (+)",	"PT (-)")
###functions reading xlsx
obtain_drivers <- function(drivers, region){
  driver_region <- driver_sheet[driver_sheet$Hotspot == region,]
  col_drivers <- driver_region[,drivers]
  col_drivers[is.na(col_drivers)] <- 0.0
  values <- unname(colSums(col_drivers[,drivers]))
  DPSIR <- "driver"
  df_driver <- cbind(region, DPSIR, drivers, data.frame(values))
  names(df_driver)[names(df_driver)=="drivers"] <- "indicator"
  return(df_driver)
}
obtain_pressures <- function(pressures, region){
  pressure_region <- pressure_sheet[pressure_sheet$Hotspot == region,]
  col_pressures <- pressure_region[,pressures]
  col_pressures[is.na(col_pressures)] <- 0.0
  values <- unname(colSums(col_pressures[,pressures]))
  DPSIR <- "pressure"
  df_pressure <- cbind(region, DPSIR, pressures, data.frame(values))
  names(df_pressure)[names(df_pressure)=="pressures"] <- "indicator"
  return(df_pressure)
}
obtain_states <- function(states, region){
  state_region <- state_sheet[state_sheet$Hotspot == region,]
  col_states <- state_region[,states]
  col_states[is.na(col_states)] <- 0.0
  values <- unname(colSums(col_states[,states]))
  DPSIR <- "state"
  df_state <- cbind(region, DPSIR, states, data.frame(values))
  names(df_state)[names(df_state)=="states"] <- "indicator"
  return(df_state)
}
obtain_impacts <- function(impacts, region){
  impact_region <- impact_sheet[impact_sheet$Hotspot == region,]
  col_impacts <- impact_region[,impacts]
  col_impacts[is.na(col_impacts)] <- 0.0
  values <- unname(colSums(col_impacts[,impacts]))
  DPSIR <- "impact"
  df_impact <- cbind(region, DPSIR, impacts, data.frame(values))
  names(df_impact)[names(df_impact)=="impacts"] <- "indicator"
  return(df_impact)
}
obtain_responses <- function(responses, region){
  response_region <- response_sheet[response_sheet$Hotspot == region,]
  col_responses <- response_region[,responses]
  col_responses[is.na(col_responses)] <- 0.0
  values <- unname(colSums(col_responses[,responses]))
  DPSIR <- "response"
  df_response <- cbind(region, DPSIR, responses, data.frame(values))
  names(df_response)[names(df_response)=="responses"] <- "indicator"
  return(df_response)
}

select_region <- function(drivers, pressures, states, impacts, responses, region) {
  df_driver <- obtain_drivers(drivers, region)
  df_pressure <- obtain_pressures(pressures, region) 
  df_state <- obtain_states(states, region)
  df_impact <- obtain_impacts(impacts, region)
  df_response <- obtain_responses(responses, region)
  df_DPSIR <- rbind(df_driver, df_pressure, df_state, df_impact, df_response)
  return(df_DPSIR)
}

###calculate df
df_california <- select_region(drivers, pressures, states, impacts, responses, "California")
df_Indus <- select_region(drivers, pressures, states, impacts, responses, "Indus")
df_USHighPlain <- select_region(drivers, pressures, states, impacts, responses, "US High Plain")
df_China <- select_region(drivers, pressures, states, impacts, responses, "China")
df_Ganges <- select_region(drivers, pressures, states, impacts, responses, "Ganges")
df_Nile <- select_region(drivers, pressures, states, impacts, responses, "Nile")
df_Mexico <- select_region(drivers, pressures, states, impacts, responses, "Mexico")
df <- rbind(df_california, df_Indus, df_USHighPlain, df_China, df_Ganges, df_Nile, df_Mexico)
#df <-df[df$values != 0, ] #remove 0 values
df$DPSIR <- factor(df$DPSIR, levels = c("driver", "pressure", "state", "impact", "response")) #set DPSIR sequence

df_driver <- df[df$DPSIR == "driver",]
df_pressure <- df[df$DPSIR == "pressure",]
df_state <- df[df$DPSIR == "state",]
df_impact <- df[df$DPSIR == "impact",]
df_response <- df[df$DPSIR == "response",]

#barplot D/P/S/I/R
ggplot(data = df_driver, aes(x=indicator, y=values, fill = region)) +
  geom_bar(stat="identity", width = 0.6, position = position_dodge(width = 0.9)) +
  coord_flip() + scale_fill_viridis_d(option="A") +
  theme_minimal()+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
    strip.text.x = element_text(size=12)) +
  facet_wrap(~DPSIR) +
  ylab("Count") +
  xlab("")
  
ggplot(data = df_pressure, aes(x=indicator, y=values, fill = region)) +
  geom_bar(stat="identity", width = 0.6, position = position_dodge(width = 0.9)) +
  coord_flip() + scale_fill_viridis_d(option="A") +
  theme_minimal()+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
    strip.text.x = element_text(size=12)) +
  facet_wrap(~DPSIR) +
  ylab("Count") +
  xlab("")

ggplot(data = df_state, aes(x=indicator, y=values, fill = region)) +
  geom_bar(stat="identity", width = 0.6, position = position_dodge(width = 0.9)) +
  coord_flip() + scale_fill_viridis_d(option="A") +
  theme_minimal()+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
    strip.text.x = element_text(size=12)) +
  facet_wrap(~DPSIR) +
  ylab("Count") +
  xlab("")

ggplot(data = df_impact, aes(x=indicator, y=values, fill = region)) +
  geom_bar(stat="identity", width = 0.6, position = position_dodge(width = 0.9)) +
  coord_flip() + scale_fill_viridis_d(option="A") +
  theme_minimal()+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
    strip.text.x = element_text(size=12)) +
  facet_wrap(~DPSIR) +
  ylab("Count") +
  xlab("")
  
ggplot(data = df_response, aes(x=indicator, y=values, fill = region)) +
  geom_bar(stat="identity", width = 0.6, position = position_dodge(width = 0.9)) +
  coord_flip() + scale_fill_viridis_d(option="A") +
  theme_minimal()+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size=12),
    axis.title = element_text(size=12),
    strip.text.x = element_text(size=12)) +
  facet_wrap(~DPSIR) +
  ylab("Count") +
  xlab("")