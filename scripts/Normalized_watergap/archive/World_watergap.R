library(ncdf4)
library(ggplot2)
library(raster)
library(rgdal)
library(plotly)
library(rasterly)
library(viridis)

load("~/2022_PhD/R_files/World_watergap.RData") # or load("~/2022_PhD/Data/World_watergap.RData")

#source https://rpubs.com/boyerag/297592

setwd("C:/Users/5738091/Documents/2022_PhD/Data")

#read and inspect nc data
nc_data <- nc_open("totalWaterGap_annuaTot.nc")

{
  sink('totalWaterGap.txt')
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(t)

total_demand.array <- ncvar_get(nc_data, "total_gross_demand") # store the data in a 3-dimensional array
dim(total_demand.array)
fillvalue <- ncatt_get(nc_data, "total_gross_demand", "_FillValue")
fillvalue
nc_close(nc_data)

#### plot 2019 timestep ####
total_demand.slice <- total_demand.array[,,40]

r <- raster(t(total_demand.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r <- flip(r, direction="y")
plot(r) # colour scale not nice yet
abline(v=120)
abline(h=33)

# ggplot raster
r_df <- as.data.frame(r, xy=T)
colnames(r_df) <- c("lon", "lat", "demand")
r_df2 <- na.omit(r_df)
i=1
while (i <= nrow(r_df)){
    if (r_df2$demand[i] < 0){
      r_df2$demand[i] = 0
    }
  i=i+1
}
r_df2$Demand <- log(r_df2$demand)

ggplot()+
  geom_tile(data=r_df2, aes(x=lon, y=lat, fill=demand))+
  scale_fill_gradientn(colours=c("grey", "yellow", "red"))+#, breaks = c(0, -40), labels = c(1,0)) +
  ggtitle("Water gap 2019 (m/y)") +
  labs(x="longitude", y="latitude") +
  theme_bw()
#ggplotly()

#### discrete classifications ####
i=1
while (i <= nrow(r_df2)){
  if (r_df2$demand[i] > 0 & r_df2$demand[i] <= 0.002){
    r_df2$classification[i] = "< 0.002 m"
  } else if (r_df2$demand[i] > 0.002 & r_df2$demand[i] <= 0.02){
    r_df2$classification[i] = "< 0.02 m"
  } else if (r_df2$demand[i] > 0.02 & r_df2$demand[i] <= 0.1){
    r_df2$classification[i] = "< 0.1 m"
  } else if (r_df2$demand[i] > 0.1 & r_df2$demand[i] <= 0.3){
    r_df2$classification[i] = "< 0.3 m"
  }  else if (r_df2$demand[i] > 0.3){
    r_df2$classification[i] = "> 0.3 m"
  } else {
    r_df2$classification[i] = NA
  }
  i=i+1
}

#plot
ggplot()+
  geom_tile(data=r_df2, aes(x=lon, y=lat, fill=classification))+
  #scale_fill_gradientn(colours=c("grey", "yellow", "red"))+#, breaks = c(0, -40), labels = c(1,0)) +
  ggtitle("Water gap 2019 (m/y)") +
  labs(x="longitude", y="latitude") +
  theme_bw()

#### plot summed timesteps 1980-2019 ####
sum_total_demand <- rowSums(total_demand.array, dims=2)
r2 <- raster(t(sum_total_demand), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat) ,
             +             crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
r2 <- flip(r2, direction="y")
plot(r2) # colour scale not nice yet

# ggplot raster
r2_df <- as.data.frame(r2, xy=T)
colnames(r2_df) <- c("lon", "lat", "demand")
r2_df2 <- na.omit(r2_df)
i=1
while (i <= nrow(r2_df)){
  if (r2_df2$demand[i] < 0){
    r2_df2$demand[i] = 0
  }
  i=i+1
}
r2_df2$Demand <- log(r2_df2$demand)

ggplot()+
  geom_tile(data=r2_df2, aes(x=lon, y=lat, fill=demand))+
  scale_fill_gradientn(colours=c("grey", "yellow", "red"), limits=c(0,300)) +
  ggtitle("Water gap (m/y)") +
  labs(x="longitude", y="latitude") +
  theme_bw()
#ggplotly()

#timeseries
china_lon <- 120
china_lat <- 33

lon_diff = lon - china_lon
china_x = which.min(abs(lon_diff))
lat_diff = lat - china_lat
china_y = which.min(abs(lat_diff))
total_demand.series = total_demand.array[china_x, china_y, ]

china_df <- data.frame(Year= seq(from=1, to=40, by=1), Demand=total_demand.series)
ggplot(data=china_df, aes(x=Year, y=Demand, group=1)) +
  geom_line() + 
  ggtitle("Yearly water demand (m)") + 
  theme_bw() 
