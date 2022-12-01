# Circular Barplot DPSIR analysis per hotspot 
library(readxl)
library(fmsb)
library(tidyverse)

setwd("./data")

#reading xlsx from 3rd row onwards
driver_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Driver", range = cell_rows(3:999))
pressure_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Pressure", range = cell_rows(3:999))
state_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "State", range = cell_rows(3:999))
impact_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Impact", range = cell_rows(3:999))
response_sheet <- read_excel("./raw/DPSIR/DPSIR_analysis.xlsx", sheet = "Response", range = cell_rows(3:999))

#list indicators
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
#functions reading xlsx
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

#input variables
region <- "Mexico"

#calculate df
df <- select_region(drivers, pressures, states, impacts, responses, region)
df <-df[df$values != 0, ] #remove 0 values
df$DPSIR <- factor(df$DPSIR, levels = c("driver", "pressure", "state", "impact", "response")) #set DPSIR sequence

##### circular barplot ######
df <- df %>% group_split(DPSIR) %>% map_dfr(~ add_row(.x, .after = Inf)) #add empty row for id
df$id <- seq(1, nrow(df)) #set id of rows

label_data <- df
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

#prepare a data frame for grid lines
grid_data <- df %>% 
  group_by(DPSIR) %>% 
  summarize(start=min(id), end=max(id) - 1) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1

#normalize by number of papers
nrpapers = nrow(driver_sheet[driver_sheet$Hotspot == region,])-nrow(driver_sheet[driver_sheet$Hotspot == "NA",])
maxindicators = max(df$values, na.rm=T)
axisbreaks = c(maxindicators, maxindicators*0.8, maxindicators*0.6, maxindicators*0.4, maxindicators*0.2, 0)

#plot with image size of at least 2000x2000
ggplot(data = df, aes(x=as.factor(id), y=values, fill=DPSIR)) +
  geom_bar(stat="identity", alpha = 0.3) +
  # Add a val=0,2,4,6,8,10 lines
  geom_segment(data=grid_data, aes(x = end, y = maxindicators, xend = start, yend = maxindicators), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = maxindicators*0.8, xend = start, yend = maxindicators*0.8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = maxindicators*0.6, xend = start, yend = maxindicators*0.6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = maxindicators*0.4, xend = start, yend = maxindicators*0.4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = maxindicators*0.2, xend = start, yend = maxindicators*0.2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # Add text showing the value of each 0,2,4,6,8,10 lines
  annotate("text", x = rep(max(df$id),6), y = c(axisbreaks), 
           label = rev(c(round(seq(0,maxindicators/nrpapers*100,0.2*maxindicators/nrpapers*100),0))) , 
           color="grey", size=12 , angle=0, fontface="bold", hjust=0) +
  geom_bar(aes(x=as.factor(id), y=values, fill=DPSIR), stat="identity", alpha=0.3) +
  ylim(-5,maxindicators) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,5), "cm"),
    legend.title=element_text(size=30), 
    legend.text=element_text(size=29),
    legend.position = "bottomright",
    rect = element_rect(fill = "transparent")) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=id, y=values, label=indicator, hjust=hjust), 
            color="black", angle=label_data$angle, inherit.aes=F, size = 12)#12
