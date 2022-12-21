########## Worldmap + circular barplot  ##########
# Author: Myrthe Leijnse

### Libraries ###
library(readxl)
library(fmsb)
library(tidyverse)
library(xlsx)
library(RColorBrewer)

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
circular_barplot_all <- function(df_DPSIR, hotspots_list){
  #subset region
  for(i in 1:length(hotspots_list)) {
    df <- df_DPSIR
    df <- df[df$region == hotspots_list[i],]
    df <- df[df$values != 0, ] # remove 0 values
    #circular barplot
    ##### circular barplot ######
    df <- df %>%
      group_split(DPSIR) %>%
      map_dfr(~ add_row(.x, .after = Inf)) # add empty row for id
    df$id <- seq(1, nrow(df)) # set id of rows
    
    label_data <- df
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # prepare a data frame for grid lines
    grid_data <- df %>%
      group_by(DPSIR) %>%
      summarize(start = min(id), end = max(id) - 1) %>%
      rowwise() %>%
      mutate(title = mean(c(start, end)))
    grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
    grid_data$start <- grid_data$start - 1
    
    # normalize by number of papers
    nrpapers <- nrow(driver_sheet[driver_sheet$Hotspot == hotspots_list[i], ]) - nrow(driver_sheet[driver_sheet$Hotspot == "NA", ])
    maxindicators <- max(df$values, na.rm = T)
    axisbreaks <- c(maxindicators, maxindicators * 0.8, maxindicators * 0.6, maxindicators * 0.4, maxindicators * 0.2, 0)
    
    # plot with image size of at least 2000x2000
    print(ggplot(data = df, aes(x = as.factor(id), y = values, fill = DPSIR)) +
            geom_bar(stat = "identity", alpha = 0.3) +
            # Add a val=0,2,4,6,8,10 lines
            geom_segment(data = grid_data, aes(x = end, y = maxindicators, xend = start, yend = maxindicators), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = maxindicators * 0.8, xend = start, yend = maxindicators * 0.8), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = maxindicators * 0.6, xend = start, yend = maxindicators * 0.6), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = maxindicators * 0.4, xend = start, yend = maxindicators * 0.4), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = maxindicators * 0.2, xend = start, yend = maxindicators * 0.2), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            # Add text showing the value of each 0,2,4,6,8,10 lines
            annotate("text",
                     x = rep(max(df$id), 6), y = c(axisbreaks),
                     label = rev(c(round(seq(0, maxindicators / nrpapers * 100, 0.2 * maxindicators / nrpapers * 100), 0))),
                     color = "grey", size = 6, angle = 0, fontface = "bold", hjust = 0
            ) +
            annotate("text", x = 0, y = -4.5, label = hotspots_list[i], size = 6) +
            geom_bar(aes(x = as.factor(id), y = values, fill = DPSIR), stat = "identity", alpha = 0.3) +
            ylim(-5, maxindicators) +
            theme_minimal() +
            theme(
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              plot.margin = unit(rep(-1, 5), "cm"),
              legend.title = element_text(size = 30),
              legend.text = element_text(size = 29),
              legend.position = "bottomright",
              rect = element_rect(fill = "transparent")
            ) +
            coord_polar(start = 0) +
            geom_text(
              data = label_data, aes(x = id, y = values, label = indicator, hjust = hjust),
              color = "black", angle = label_data$angle, inherit.aes = F, size = 6
              ) # 12
    )
  }
}
circular_barplot_simplified <- function(df_DPSIR, hotspots_list){
  #subset region
  for(i in 1:length(hotspots_list)) {
    df <- df_DPSIR
    df <- df[df$region == hotspots_list[i],]
    df <- df[df$values != 0, ] # remove 0 values
    #circular barplot
    ##### circular barplot ######
    df <- df %>%
      group_split(DPSIR) %>%
      map_dfr(~ add_row(.x, .after = Inf)) # add empty row for id
    df$id <- seq(1, nrow(df)) # set id of rows
    
    label_data <- df
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
    label_data$hjust <- ifelse(angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle + 180, angle)
    
    # prepare a data frame for grid lines
    grid_data <- df %>%
      group_by(DPSIR) %>%
      summarize(start = min(id), end = max(id) - 1) %>%
      rowwise() %>%
      mutate(title = mean(c(start, end)))
    grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
    grid_data$start <- grid_data$start - 1
    
    # normalize by number of papers
    nrpapers <- nrow(driver_sheet[driver_sheet$Hotspot == hotspots_list[i], ]) - nrow(driver_sheet[driver_sheet$Hotspot == "NA", ])
    maxindicators <- max(df$values, na.rm = T)
    axisbreaks <- c(maxindicators, maxindicators * 0.8, maxindicators * 0.6, maxindicators * 0.4, maxindicators * 0.2, 0)
    
    # plot with image size of at least 2000x2000
    print(ggplot(data = df, aes(x = as.factor(id), y = values, fill = DPSIR)) +
            geom_bar(stat = "identity", alpha = 0.5) +
            # Add a val=0,2,4,6,8,10 lines
            geom_segment(data = grid_data, aes(x = end, y = maxindicators, xend = start, yend = maxindicators), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = maxindicators * 0.8, xend = start, yend = maxindicators * 0.8), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = maxindicators * 0.6, xend = start, yend = maxindicators * 0.6), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = maxindicators * 0.4, xend = start, yend = maxindicators * 0.4), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = maxindicators * 0.2, xend = start, yend = maxindicators * 0.2), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            geom_segment(data = grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha = 1, size = 0.3, inherit.aes = FALSE) +
            # Add text showing the value of each 0,2,4,6,8,10 lines
            annotate("text",
                     x = rep(max(df$id), 6), y = c(axisbreaks),
                     label = rev(c(round(seq(0, maxindicators / nrpapers * 100, 0.2 * maxindicators / nrpapers * 100), 0))),
                     color = "grey", size = 6, angle = 0, fontface = "bold", hjust = 0
            ) +
            annotate("text", x = 0, y = -4.5, label = hotspots_list[i], size = 6) +
            geom_bar(aes(x = as.factor(id), y = values, fill = DPSIR), stat = "identity", alpha = 0.3) +
            ylim(-5, maxindicators) +
            theme_minimal() +
            theme(
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              plot.margin = unit(rep(-1, 5), "cm"),
              legend.title = element_text(size = 30),
              legend.text = element_text(size = 29),
              legend.position = "bottomright",
              rect = element_rect(fill = "transparent")
            ) +
            #scale_fill_manual(values = c(col_vector)) +
            scale_fill_brewer(palette = "Dark2") +
            coord_polar(start = 0) +
            geom_text(
              data = label_data, aes(x = id, y = values, label = indicator, hjust = hjust),
              color = "black", angle = label_data$angle, inherit.aes = F, size = 6
            ) # 12
    )
  }
}

### testing colors ###
n=45
col_pals = brewer.pal.info[brewer.pal.info$category == 'seq',]
col_vector = unlist(mapply(brewer.pal, col_pals$maxcolors, rownames(col_pals)))
col_vector = col_vector[,c(1,5,7,12,14)]
pie(rep(1,n), col=col_vector)

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
df_DPSIR$indicator<- factor(df_DPSIR$indicator, levels = c(drivers, pressures, states, impacts, responses))

### Plotting ###
circular_barplot_all(df_DPSIR, hotspots_list)

####################################################################################################################################################

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

#### calc total counts per indicator to see which relevant and which not
df_test <- df_DPSIR
df_test <- df_test %>% group_by(indicator) %>% summarize(values = sum(values))
