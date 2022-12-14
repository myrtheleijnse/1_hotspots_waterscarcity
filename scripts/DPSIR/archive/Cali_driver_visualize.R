library(readxl)
library(fmsb)
library(tidyverse)

setwd("C:/Users/5738091/Documents/2022_PhD/Data/DPSIR_visuals")

### load data (water provinces, DPSIR outcome)
data <- read_excel("C:/Users/5738091/Documents/2022_PhD/Data/DPSIR_visuals/DPSIR_analysis_Cali.xlsx", sheet = "Driver")

#data frame california drivers
Cali <- data[data$Country == "USA",]
drivers <- c("Population", "Low natural water availability",	"Economic growth",	
             "Inefficient irrigation system", "Snowpack reduction",	"Rainfall seasonality changes",	
             "Aridification", "Drought frequency")
Cali_drivers <- Cali[,drivers]
Cali_drivers[is.na(Cali_drivers)] <- 0.0
values <- unname(colSums(Cali_drivers[,drivers]))
DPSIR <- "driver"
Region <- "California"
df_Cali <- cbind(Region, DPSIR, drivers, data.frame(values))

#spider plot
data <- t(colSums(Cali_drivers[,drivers]))
rownames(data) <- c(3)
data <- as.data.frame(data)
data <- rbind(max(data), min(data), data)
radarchart(data  , axistype=1 , 
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
           cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(0,6,1.25),
           vlcex=0.8)

#bar plot
ggplot(data = df_Cali, aes(x=drivers, y=values)) +
  geom_bar(stat="identity", fill="#69b3a2", width = 0.6) +
  coord_flip() + theme_minimal()+
  facet_wrap(~Region)+
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size=18), 
    axis.title = element_text(size=18),
    strip.text.x = element_text(size=18)) +
  ylim(0,5) +
  ylab("value") +
  xlab("") #, ncol=4)

#circular barplot
df_Cali <- df_Cali %>% mutate(id = row_number())
label_data <- df_Cali
label_data <- label_data %>% mutate(id = row_number())
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

ggplot(data = df_Cali, aes(x=as.factor(id), y=values, fill=Region)) +
  geom_bar(stat="identity", fill=alpha("#69b3a2", 0.3)) +
  ylim(-1,5) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=id, y=values, label=drivers, hjust=hjust), 
            color="black", angle=label_data$angle, inherit.aes=F)
