library(readxl)
library(fmsb)
library(tidyverse)

setwd("C:/Users/5738091/Documents/2022_PhD/Data/DPSIR_visuals")

#reading xlsx from 3rd row onwards
driver_sheet <- read_excel("C:/Users/5738091/Documents/2022_PhD/Data/DPSIR_visuals/DPSIR_analysis_v3.xlsx", sheet = "Driver", range = cell_rows(3:999))
pressure_sheet <- read_excel("C:/Users/5738091/Documents/2022_PhD/Data/DPSIR_visuals/DPSIR_analysis_v3.xlsx", sheet = "Pressure", range = cell_rows(3:999))
state_sheet <- read_excel("C:/Users/5738091/Documents/2022_PhD/Data/DPSIR_visuals/DPSIR_analysis_v3.xlsx", sheet = "State", range = cell_rows(3:999))
impact_sheet <- read_excel("C:/Users/5738091/Documents/2022_PhD/Data/DPSIR_visuals/DPSIR_analysis_v3.xlsx", sheet = "Impact", range = cell_rows(3:999))
response_sheet <- read_excel("C:/Users/5738091/Documents/2022_PhD/Data/DPSIR_visuals/DPSIR_analysis_v3.xlsx", sheet = "Response", range = cell_rows(3:999))

#change to automated reading of indicators
drivers <- c("Population", "Low natural water availability",	"Economic growth",	
             "Inefficient irrigation system", "Siltation", "Snowpack reduction", "Rainfall seasonality changes",	
             "Aridification", "Drought frequency", "Temperature increase", "Sea level rise")
pressures <- c("Tourism",	"Agricultural water use",	"Ecosystem water demand",	"Total water use",	
               "Domestic water demand",	"Crop change/intensification",	"Urbanization", "Mining", 
               "Industrial water use", "Livestock water use", "Virtual water trade",  "Aquaculture")
states <- c("Water use per capita",	"Groundwater depletion",	"Contamination",	
            "Overallocation of surface water", "Low water storage capacity", "Salinization", 
            "Running dry of surface water")
impacts <- c("Fallowing acreage", "Subsidence",	"Reduced hydroelectricity production",	"Damage to ecosystems",	
             "Reduced food production", "Conflict",	"Health",	"Costs (pumping/energy)", "High GHG emissions", 
             "Water collection")
responses <- c("RGW (+)",	"URGW (-)",	"WI (+)",	"WI (-)",	"IWUE (+)",	"IWUE (-)",	"WC (+)",	"WC(-)", 	"WRe (+)",
               "WRe (-)",	"DC (+)",	"DC (-)",	"WP (+)", 	"WP (-)",	"DP (+)",	"DP(-)",	"Wri (+)",	"Wri (-)",
               "SRP (+)",	"SRP (-)", "GTW (+)", "UGTW (-)",	"LP (+)", "LP (-)",	"PT (+)",	"PT (-)")

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

df_Pakistan <- select_region(drivers, pressures, states, impacts, responses, "Indus")
df_Pakistan <-df_Pakistan[df_Pakistan$values != 0, ] #remove 0 values
df_Pakistan$DPSIR <- factor(df_Pakistan$DPSIR, levels = c("driver", "pressure", "state", "impact", "response")) #set DPSIR sequence

##### circular barplot ######
df_Pakistan <- df_Pakistan %>% group_split(DPSIR) %>% map_dfr(~ add_row(.x, .after = Inf)) #add empty row for id
df_Pakistan$id <- seq(1, nrow(df_Pakistan))

label_data <- df_Pakistan
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for grid lines
grid_data <- df_Pakistan %>% 
  group_by(DPSIR) %>% 
  summarize(start=min(id), end=max(id) - 1) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1

#normalize by number of papers
nrpapers = nrow(driver_sheet[driver_sheet$Hotspot == "Indus",])-nrow(driver_sheet[driver_sheet$Hotspot == "NA",])

#plot
ggplot(data = df_Pakistan, aes(x=as.factor(id), y=values, fill=DPSIR)) +
  geom_bar(stat="identity", alpha = 0.3) +
  # Add a val=0,2,4,6,8,10 lines
  geom_segment(data=grid_data, aes(x = end, y = 10, xend = start, yend = 10), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 8, xend = start, yend = 8), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 6, xend = start, yend = 6), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 4, xend = start, yend = 4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 2, xend = start, yend = 2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  # Add text showing the value of each 0,2,4,6,8,10 lines
  annotate("text", x = rep(max(df_Pakistan$id),6), y = c(seq(0,10,2)), 
           label = c(round(seq(0/nrpapers*100,10/nrpapers*100,2/nrpapers*100),0)) , 
           color="grey", size=6 , angle=0, fontface="bold", hjust=-0.5) +
  geom_bar(aes(x=as.factor(id), y=values, fill=DPSIR), stat="identity", alpha=0.3) +
  ylim(-5,10) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,5), "cm"),
    legend.title=element_text(size=20), 
    legend.text=element_text(size=19)) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=id, y=values, label=indicator, hjust=hjust), 
            color="black", angle=label_data$angle, inherit.aes=F, size = 5)
