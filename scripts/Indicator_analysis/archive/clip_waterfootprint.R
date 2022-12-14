### Water footprint per ton of crop (m3/ton) 1996-2005

library(readxl)

setwd("C:/Users/5738091/Documents/2022_PhD/Data")

#read data
wf_all <- read_excel("Indicators/LandUse/WF-of-production-RasterFiles/Report47-Appendix-II.xlsx", "Sheet1")
#wf_all <- read.csv("Indicators/LandUse/WF-of-production-RasterFiles/Report47-Appendix-II.csv", sep=";", header=F)[4:1065,]

#subset maize, soybean, rice, wheat
#find rownumber of wheat
df_wheat <- 

#subset california
#wf_cali <- wf_all[wf_all$ == "California",]

#
