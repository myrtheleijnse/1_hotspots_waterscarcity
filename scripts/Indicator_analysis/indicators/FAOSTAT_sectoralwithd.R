### Sectoral water use AQUASTAT ###
# country level data 2010-2019

library(readxl)
library(tidyverse)
library(viridis)
library(hrbrthemes)
library(lubridate)

#read data
df_withdrawal <- read_excel("C:/Users/5738091/Documents/2022_PhD/Data/Indicators/SectoralWithdrawal/AQUASTAT.xlsx")
#define hotspots
df_withdrawal$hotspot = df_withdrawal$Country
for(i in (1:nrow(df_withdrawal))){
  if(df_withdrawal$Country[i] == "Netherlands" | df_withdrawal$Country[i] == "Belgium" | df_withdrawal$Country[i] == "Germany" 
     | df_withdrawal$Country[i] == "Luxembourg"){
    df_withdrawal$hotspot[i] = "Rhine"
  } else if (df_withdrawal$Country[i] == "Bangladesh" | df_withdrawal$Country[i] == "India"){
    df_withdrawal$hotspot[i] = "Ganges/Brahmaputra"
  } else if (df_withdrawal$Country[i] == "Iran (Islamic Republic of)" | df_withdrawal$Country[i] == "Iraq"){
    df_withdrawal$hotspot[i] = "Tigris/Euphrates"
  } else if (df_withdrawal$Country[i] == "Jordan" | df_withdrawal$Country[i] == "Israel" | df_withdrawal$Country[i] == "Palestine(1996-)"){
    df_withdrawal$hotspot[i] = "Jordan"
  } else if (df_withdrawal$Country[i] == "United States of America"){
    df_withdrawal$hotspot[i] = "USA"
  } else if (df_withdrawal$Country[i] == "Viet Nam"){
    df_withdrawal$hotspot[i] = "Vietnam"
  }
}

#separate df and summarize per hotspot
df_sectoral <- df_withdrawal[df_withdrawal$Variable != "Total water withdrawal" & df_withdrawal$Variable != "Total water withdrawal per capita" & 
                               df_withdrawal$Variable != "Irrigation water withdrawal",]
df_sectoral$Variable <- factor(df_sectoral$Variable, levels = c("Water withdrawal for livestock (watering and cleaning)",
                                                                "Water withdrawal for aquaculture","Municipal water withdrawal",
                                                                "Industrial water withdrawal","Agricultural water withdrawal"))
df_sectoral <- df_sectoral %>% group_by(hotspot, Variable, Year) %>% summarize(value = sum(value)) #sum sectoral water use hotspot countries
df_withdrawalpercap <- df_withdrawal[df_withdrawal$Variable == "Total water withdrawal per capita",]
df_withdrawalpercap <- df_withdrawalpercap %>% group_by(hotspot, Variable, Year) %>% summarize(value = mean(value)) #mean water use per capita hotspot countries
df_withdrawaltot <- df_withdrawal[df_withdrawal$Variable == "Total water withdrawal",]
df_withdrawaltot <- df_withdrawaltot %>% group_by(hotspot, Variable, Year) %>% summarize(value = sum(value)) #sum total water use hotspot countries
df_withdrawalirr <- df_withdrawal[df_withdrawal$Variable == "Irrigation water withdrawal",]
df_withdrawalirr <- df_withdrawalirr %>% group_by(hotspot, Variable, Year) %>% summarize(value = sum(value)) #sum irrigation water use hotspot countries

#hotspot per sector
df <- df_sectoral[df_sectoral$hotspot == "USA",]
ggplot(df, aes(Year, value, fill=Variable)) +
  geom_area(alpha = 0.6, size = .5, colour="white") +
  scale_fill_viridis_d(direction = -1) +
  #facet_wrap(~hotspot) +
  theme_ipsum() +
  scale_x_continuous(breaks=seq(2010, 2019, 2)) +
  ylab("Withdrawal (10^9 m3/y)") +
  ggtitle("Sectoral water withdrawal")

#withdrawal per capita
ggplot(df_withdrawalpercap, aes(Year, value)) +
  geom_area(alpha = 0.6, size = .5, colour="grey") +
  facet_wrap(~hotspot) +
  theme_ipsum() +
  scale_x_continuous(breaks=seq(2010, 2019, 2)) +
  ylab("Withdrawal (m3/capita/y)") +
  ggtitle("Water withdrawal per capita")
  
#withdrawal total
ggplot(df_withdrawaltot, aes(Year, value)) +
  geom_area(alpha = 0.6, size = .5, colour="white") +
  facet_wrap(~hotspot) +
  theme_ipsum() +
  scale_x_continuous(breaks=seq(2010, 2019, 2)) +
  ylab("Withdrawal (10^9 m3/y)") +
  ggtitle("Total water withdrawal")

#withdrawal irrigation
ggplot(df_withdrawalirr, aes(Year, value)) +
  geom_area(alpha = 0.6, size = .5, colour="white") +
  facet_wrap(~hotspot) +
  theme_ipsum() +
  scale_x_continuous(breaks=seq(2010, 2019, 2)) +
  ylab("Withdrawal (10^9 m3/y)") +
  ggtitle("Irrigation water withdrawal")
