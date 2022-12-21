### Circular Barplot DPSIR analysis per hotspot ###
# Author: Myrthe Leijnse

### Libraries ###
library(readxl)
library(fmsb)
library(tidyverse)
library(viridis)
library(xlsx)

### Directory ###
setwd("E:/1_hotspots_waterscarcity/data")

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
    #merge indicators (see notes)
    merge1 <- df_region %>% group_by(region) %>% 
      filter(indicator %in% c("Aridification", "Drought frequency", "Rainfall seasonality changes")) %>% 
      summarize(DPSIR = "driver", indicator = "Rainfall reduction", values = sum(values))
    merge2 <- df_region %>% group_by(region) %>% 
      filter(indicator %in% c("Tourism", "Urbanization", "Domestic water use")) %>% 
      summarize(DPSIR = "pressure", indicator = "Municipal water use", values = sum(values))
    merge3 <- df_region %>% group_by(region) %>% 
      filter(indicator %in% c("Agri water use", "Crop change/intensification")) %>% 
      summarize(DPSIR = "pressure", indicator = "Agri water use", values = sum(values))
    merge4 <- df_region %>% group_by(region) %>% 
      filter(indicator %in% c("Livestock water use", "Aquaculture", "Horticulture", "Forestry")) %>% 
      summarize(DPSIR = "pressure", indicator = "Other farming water use", values = sum(values))
    merge5 <- df_region %>% group_by(region) %>% 
      filter(indicator %in% c("Industrial water use", "Mining")) %>% 
      summarize(DPSIR = "pressure", indicator = "Industrial water use", values = sum(values))
    merge6 <- df_region %>% group_by(region) %>% 
      filter(indicator %in% c("Fallowing acreage", "Reduced food production")) %>% 
      summarize(DPSIR = "impact", indicator = "Reduced food production", values = sum(values))
    merge7 <- df_region %>% group_by(region) %>% 
      filter(indicator %in% c("Reduced hydroelectricity production", "Costs (pumping/energy)")) %>% 
      summarize(DPSIR = "impact", indicator = "Increased energy costs", values = sum(values))
    df_region <- df_region %>% group_by(region) %>% filter(indicator != "Aridification",
                                                           indicator != "Drought frequency", 
                                                           indicator != "Rainfall seasonality changes",
                                                           indicator != "Tourism",
                                                           indicator != "Urbanization", 
                                                           indicator != "Domestic water use",
                                                           indicator != "Agri water use",
                                                           indicator != "Crop change/intensification",
                                                           indicator != "Livestock water use", 
                                                           indicator != "Aquaculture",
                                                           indicator != "Horticulture",
                                                           indicator != "Forestry",
                                                           indicator != "Industrial water use",
                                                           indicator != "Mining",
                                                           indicator != "Fallowing acreage",
                                                           indicator != "Reduced food production",
                                                           indicator != "Reduced hydroelectricity production",
                                                           indicator != "Costs (pumping/energy)"
                                                           )
    df_region <- rbind(df_region, merge1, merge2, merge3, merge4, merge5, merge6, merge7)
    #relative count
    nrpapers <- nrow(sheet[sheet$Hotspot == region, ]) - nrow(sheet[sheet$Hotspot == "NA", ])
    print(nrpapers)
    df_region$Count.rel <- df_region$values/nrpapers*100
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
  "Irrigation mismanagement", "Siltation", "Snowpack reduction", "Rainfall seasonality changes",
  "Aridification", "Drought frequency", "Temperature increase", "Sea level rise"
)
pressures <- c(
  "Tourism", "Agri water use", "Ecosystem water demand", "Total water use (unspecified)",
  "Domestic water use", "Crop change/intensification", "Urbanization", "Mining",
  "Industrial water use", "Livestock water use", "Virtual water trade", "Aquaculture",
  "Forestry", "Horticulture"
)
states <- c(
  "Water use per capita", "GW depletion", "Contamination",
  "SW depletion", "Low water storage capacity", "Salinization"
)
impacts <- c(
  "Fallowing acreage", "Subsidence", "Reduced hydroelectricity production", "Damage to ecosystems",
  "Reduced food production", "Conflict", "Health", "Costs (pumping/energy)", "High GHG emissions",
  "Water collection", "Migration"
)
responses <- c(
  "Regulated GW (+)",	"Unregulated GW (-)",	"Water transfer (+)",	"Water transfer (-)",	"Improved water use efficiency (+)",
  "Efficiency paradox (-)",	"Water conservation (+)",	"Water treatment (+)",	"Increased storage capacity (+)",
  "Increased storage capacity (dam construction) (-)",	"Water pricing (+)",	"Water pricing (-)",	"Desalination plants (+)",
  "Water rights (+)",	"Water rights (-)",	"Stakeholder involvement (+)",	"Stakeholder involvement (-)"
)
df_hotspots <- unique(read.xlsx("./raw/DPSIR/DPSIR_analysis.xlsx", "Driver", colIndex=2, rowIndex = 3:999))
hotspots_list <- df_hotspots$Hotspot

### Execution ###
df_driver <- obtain_indicator(driver_sheet, drivers, "driver", "drivers", hotspots_list)
df_pressure <- obtain_indicator(pressure_sheet, pressures, "pressure", "pressures", hotspots_list)
df_state <- obtain_indicator(state_sheet, states, "state", "states", hotspots_list)
df_impact <- obtain_indicator(impact_sheet, impacts, "impact", "impacts", hotspots_list)
df_response <- obtain_indicator(response_sheet, responses, "response", "responses", hotspots_list)
df_DPSIR <- rbind(df_driver, df_pressure, df_state, df_impact, df_response)
df_DPSIR$DPSIR <- factor(df_DPSIR$DPSIR, levels = c("driver", "pressure", "state", "impact", "response")) # set DPSIR sequence

### Plotting ###
#barplot D/P/S/I/R
plot_DPSIR_bar <- function(df, indicator_name_string_singular) {
  print(
    ggplot(data = df[df$DPSIR == indicator_name_string_singular,], aes(x=indicator, y=Count.rel, fill = region)) +
      geom_bar(stat="identity", width = 0.6, position = position_dodge(width = 0.9)) +
      coord_flip() + scale_fill_viridis_d(option="H") +
      theme_minimal()+
      theme(
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size=12),
        axis.title = element_text(size=12),
        strip.text.x = element_text(size=12)) +
      facet_wrap(~DPSIR) +
      ylab("(%)") +
      xlab("") 
    )
}

plot_DPSIR_bar(df_DPSIR, "driver")
plot_DPSIR_bar(df_DPSIR, "pressure")
plot_DPSIR_bar(df_DPSIR, "state")
plot_DPSIR_bar(df_DPSIR, "impact")
plot_DPSIR_bar(df_DPSIR, "response")